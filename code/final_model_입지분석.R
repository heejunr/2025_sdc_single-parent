## 목표
# 마포/강서/강남의 구 단위 한부모가구 인구를 동 단위로 비례 배분해 수요를 추정하고,
# 주거지역 기반 dasymetric mapping으로 수요를 주거 가능 면적에 재분배한 뒤,
# 복지시설 서비스 반경(BUFFER) 외에 위치한 수요 집중 지역(복지 사각지대)을 도출한다.

## 절차
# 1) 동별 인구 비율로 구 단위 한부모 인구를 동 단위로 비례 배분(동별 수요 추정)
# 2) 행정동 경계(shp)와 결합해 공간 단위로 수요 매핑
# 3) 주거지역(토지이용 3110/3120)과 교차 후, 주거 면적 비례로 수요 재분배(dasymetric mapping)
# 4) 복지시설 좌표로 BUFFER 반경 서비스권역을 생성하고, 서비스권역 외 주거지역 추출
# 5) 동별로 '사각지대 내 추정 수요/면적'을 계산해 밀도 상위 지역을 선별


library(dplyr)
library(sf)
library(stringr)
library(ggplot2)
library(purrr)

BUFFER <- 1000  # 복지시설 서비스 반경 (단위: m)

### 동별 인구 데이터 전처리 -----------------------------------------------------------
path <- "./data"
pop_raw <- read.csv(file.path(path, "등록인구(연령별_동별)_마포구, 강서구, 강남구_2020.csv"))

#동별 인구 
pop_dong <- pop_raw %>%
  filter(동별.2. != "소계") %>%
  select(시군구명 = 동별.1., 동별명 = 동별.2., 동별인구수 = 합계)

#구별 인구
pop_gu <- pop_raw %>%
  filter(동별.2. == "소계") %>%
  select(시군구명 = 동별.1., 구별총인구 = 합계)

#동별 인구 기준 구별 인구 leftjoin 및 ratio 생성 
pop_ratio <- left_join(pop_dong, pop_gu, by = "시군구명") %>%
  mutate(동별인구비율 = 동별인구수 / 구별총인구)

## 한부모 가구 데이터 처림 및 동별 추정 -----
single_raw <- read.csv(file.path(path, "시군구_한부모.csv"), fileEncoding="cp949")

single_gu <- single_raw %>%
  filter(시군구 %in% c('마포구', '강서구', '강남구')) %>%
  select(시군구명 = 시군구, 한부모추정인구)

#동별 한부모추정인구 
single_dong <- left_join(pop_ratio, single_gu, by = "시군구명") %>%
  mutate(
    동별명 = ifelse(동별명 == "일원2동", "개포3동", 동별명),  # 동명 수정
    동별_한부모추정인구 = round(한부모추정인구 * 동별인구비율)
  )



### 공간 데이터 병합 ---------------------------------------------------------------
shp_path <- "./data/행정동_shp/BND_ADM_DONG_PG.shp"
map_all <- st_read(shp_path)

# 마포구(140), 강서구(160), 강남구(230)
map_filtered <- map_all %>%
  filter(str_sub(ADM_CD, 1, 2) == "11" & str_sub(ADM_CD, 3, 5) %in% c("140", "160", "230"))
map_final <- left_join(map_filtered, single_dong, by = c("ADM_NM" = "동별명"))

## 토지 이용 현황 및 주거지역
# 일반주택지(3110), 고층주택지(3120)
landuse_path <- "./data/NGII_LUM_11_서울/"
shp_files <- list.files(landuse_path, pattern = "\\.shp$", full.names = TRUE)

landuse_residential <- map_dfr(shp_files, ~{
  st_read(.x, quiet = TRUE) %>% filter(UCB %in% c(3110, 3120))
})

if (is.na(st_crs(landuse_residential))) st_crs(landuse_residential) <- 5174
landuse_residential <- st_transform(landuse_residential, st_crs(map_final))

## Dasymetric mapping 
# 실제 거주가 가능한 주거지역 면적을 기준으로 인구 분배
intersected <- st_intersection(map_final, landuse_residential) %>%
  mutate(area = st_area(.)) %>%
  group_by(ADM_NM) %>%
  mutate(total_area = sum(area)) %>%
  ungroup() %>%
  mutate(final_single_parent_pop = as.numeric(동별_한부모추정인구 * area / total_area))

## 복지시설 데이터 및 서비스 권역
facilities_df <- read.csv("./data/서울시_복지시설_좌표.csv") %>%
  filter(자치구 %in% c("마포구", "강서구", "강남구"), Latitude > 37)

facilities_sf <- st_as_sf(facilities_df, coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(st_crs(map_final))

service_areas <- st_buffer(facilities_sf, dist = BUFFER) #BUFFER = 1000

## 사각지대
covered_mat <- st_intersects(intersected, service_areas, sparse = FALSE)
is_covered <- apply(covered_mat, 1, any)
gap_map <- intersected[!is_covered, ]

# 값이 클수록 복지 사각지대 안에 한부모 수요가 밀집되었다고 해석할 수 있음
gap_by_dong <- gap_map %>%
  group_by(시군구명, ADM_NM) %>%
  summarise(
    사각지_한부모인구 = sum(final_single_parent_pop, na.rm = TRUE),
    면적합 = sum(as.numeric(st_area(geometry))),
    .groups = "drop"
  ) %>%
  mutate(추정_밀도 = 사각지_한부모인구 / 면적합) 

# 상위 3개 동
gap_top_dong <- gap_by_dong %>%
  group_by(시군구명) %>%
  arrange(desc(추정_밀도)) %>%
  slice_head(n = 3)



### 시각화 ---------------------------------------------------------------------
# ...
