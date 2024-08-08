# kamis 데이터 불러오기 _2


#  필요패키지 불러오기

library(httr)
library(rvest)
library(jsonlite)
library(tidyverse)
library(curl)
library(openxlsx)


# 품목코드를 활용하여 특정 품목의 최근가격 데이터 조회

#요청변수
# 1. p_regday = yyyy-mm-dd 형식
# 2. p_cert_key = 키값
# 3. p_cert_id = 아이디
# 4. p_returntype = 리턴형식
# 5. p_productno = 품목코드 소매와 도매의 품목코드가 다름



# 1. 소매 수박 _ 307

#요청값 정리

usethis::edit_r_environ()
Sys.getenv("kamiskey") # kamis 인증 키

url <- 'https://www.kamis.or.kr/service/price/xml.do?action=recentlyPriceTrendList'

#홈페이지의 URL이 잘못 설정되어 있어 샘플토드의 URL로 변경하여 에러 해결함

mykey <-  Sys.getenv("kamiskey") %>% I()

id <- '4575'

type <- "json"

code <- '307'

date <- '2024-07-31'

headers = c('User-Agent' = 'Mozilla/5.0')

#Request 조합

res <- GET(url,
           query = list(p_productno = code,
                        p_regday = date,
                        p_cert_key = mykey,
                        p_cert_id = id,
                        p_returntype = type),
           add_headers(.headers = headers))

print(res)

#불러오기 성공


#Json과 리스트 형식의 데이터를 데이터프레임으로 변경

res %>% 
  content(as = "text", encoding = 'UTF-8') %>% 
  fromJSON() -> json_1


#변경이 제대로 되어 있는지 확인

str(json_1)

#리스트 중, 정보를 담고 있는 항목만 빼내어 데이터프레임 형식으로 저장

df <- json_1$price

#확인

str(df)

glimpse(df)

df


