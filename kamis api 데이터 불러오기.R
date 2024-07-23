#농수산물 API 불러오기

library(httr)
library(rvest)
library(jsonlite)
library(tidyverse)
library(curl)


# kamis 에서 최근 일자 도소매 가격 정보 불러오기

# http://www.kamis.co.kr/service/price/xml.do?action=dailySalesList&p_cert_key=test&p_cert_id=test&p_returntype=xml

# 홈페이지에서 소개하는 url에는 이미 요청 값들이 & 다음으로 들어가 있다
# 요청갑은 총 3가지
# p_cert_key=test API 키값 입력
# p_cert_id=test 승인 요청 당ㅅ 할당된 ID
# p_returntype=xml 자료 받을 형식 json:Json 데이터 형식, xml:XML데이터형식

# 위 3가지 형식 제외 하고 url로 입력 후 
#  GET(url = url,query = list(LAWD_CD = "41465", DEAL_YMD = "202312", serviceKey = mykey %>% I()))

# 형식으로 입력


############################# 실패 ###################################

#res <- GET(url = url,
 #          query = list(p_cert_key = mykey %>% I(),
#                      p_cert_id = id,
#                        p_returntype = type),
#           verbose())

#실패 동일한 오류

res <- GET(url,
           query = list(p_cert_key = mykey,
                        p_cert_id = id,
                        p_returntype = type),
           timeout(30),  # 타임아웃 설정
           verbose())

#res <- GET(url,
           query = list(p_cert_key = mykey,
                        p_cert_id = id,
                        p_returntype = type),
           timeout(30)  # 타임아웃 설정
           verbose())
#########################################  실패################

usethis::edit_r_environ()
Sys.getenv("kamiskey")

url <- 'http://www.kamis.co.kr/service/price/xml.do?action=dailySalesList'

mykey <-  Sys.getenv("kamiskey") %>% I()

id <- '4575'

type <- "json"

# URL 작성
# full_url <- paste0(url, "&p_cert_key=", mykey, "&p_cert_id=", id, "&p_returntype=", type)
#정상 URL로 확인 되나 R로 받을때는 이상하다

full_url <- paste0(url, "&p_cert_key=", URLencode(mykey), "&p_cert_id=", id, "&p_returntype=", type)
full_url


# API 호출
#handle <- new_handle()
#handle_setopt(handle, url = full_url)
#handle_setopt(handle, timeout = 30)  # 타임아웃 설정
#res <- curl_fetch_memory(handle = handle)

handle <- new_handle()
handle_setopt(handle, timeout = 30)  # 타임아웃 설정
response <- curl_fetch_memory(full_url, handle = handle)


print(response)

# 응답 확인
if (response$status_code == 200) {
  content <- rawToChar(response$content)
  cat("Response Content: ", content, "\n")  # 응답 내용 출력
  
  if (type == "json") {
    parsed_content <- fromJSON(content)
    print(parsed_content)  # 파싱된 JSON 출력
    parsed_content
  } else {
    content  # XML 형식의 경우 추가 파싱 필요
  }
} else {
  stop("API 호출 실패: ", response$status_code)
}

print(parsed_content)


df <- parsed_content$price

str(df)

#리스트는 dpr2, dpr3, dpr4, value, 그리고 direction

#####리스트로 저장된 컬럼 변경

## 리스트의 빈값을 0 또는 na로 변경할것 일부 컬럼에서는 0이 의미를 가짐

#리스트의 빈값을 0으로 변경하는 함수
replace_empty_with_zero <- function(lst) {
  lapply(lst, function(x) {
    if (length(x) == 0) {
      return("0")
    } else {
      return(x)
    }
  })
}

#리스트의 빈값을 NA으로 변경하는 함수
replace_empty_with_NA <- function(lst) {
  lapply(lst, function(x) {
    if (length(x) == 0) {
      return("NA")
    } else {
      return(x)
    }
  })
}


# 리스트를 벡터로 변환하는 함수
list_to_vector <- function(lst) {
  sapply(lst, function(x) unlist(x))
}


df$dpr2 <- replace_empty_with_zero(df$dpr2)
df$dpr3 <- replace_empty_with_zero(df$dpr3)
df$dpr4 <- replace_empty_with_zero(df$dpr4)
df$value <- replace_empty_with_zero(df$value)
df$direction <- replace_empty_with_NA(df$direction)

df$dpr2 <- list_to_vector(df$dpr2)
df$dpr3 <- list_to_vector(df$dpr3)
df$dpr4 <- list_to_vector(df$dpr4)
df$value <- list_to_vector(df$value)
df$direction <- list_to_vector(df$direction)

str(df)

########### 리스트 컬럼 변경 완료

### 속성값 변경

df$product_cls_code <- as.factor(df$product_cls_code)
df$product_cls_name <- as.factor(df$product_cls_name)
df$category_code <- as.factor(df$category_code)
df$direction <- as.factor(df$direction)

#####na 값으로 변경됨 ###
df$dpr1 <- as.numeric(df$dpr1)
df$dpr2 <- as.numeric(df$dpr2)
df$dpr3 <- as.numeric(df$dpr3)
df$dpr4 <- as.numeric(df$dpr4)
###### ,등이 포함되어 변경됨

test <- df$dpr1

test <- readr::parse_number(test)
remove(test)

#### test 성공 -> readr 의 parse_number 함수 사용 결정

df$dpr1 <- readr::parse_number(df$dpr1)
df$dpr2 <- readr::parse_number(df$dpr2)
df$dpr3 <- readr::parse_number(df$dpr3)
df$dpr4 <- readr::parse_number(df$dpr4)

str(df)

################## 데이터 살펴보기


df %>% 
  select(category_code) %>% 
  distinct()
#카테고리 총수는 6개


df %>%
  group_by(category_code) %>% 
  summarise(n = n()) %>% 
  arrange(-n)

#채소류인 200이 가장 많음

df %>% group_by(direction) %>% summarise(n = n())
# 가격 상승 하락 별 숫자 보기

df %>% select(productName, item_name)

df %>% select(day1) %>% distinct()
df %>% select(day2) %>% distinct()
df %>% select(day3) %>% distinct()
df %>% select(day4) %>% distinct()
