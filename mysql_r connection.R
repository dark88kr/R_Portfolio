### R과 MYSQL 연결하기

library(DBI)
library(RMySQL)
library(tidyverse)

#암호 설정 
usethis::edit_r_environ()

#sql 접속키 설정
pw <-  Sys.getenv("mysql")



db <- dbConnect(
  MySQL(),
  user = 'root',
  password = pw ,
  host = '127.0.0.1',
  dbname = 'world'
)

# db 연결 성공

#실패 사례 - pw가 틀렸다는 에러 메시지 회신
#pw를 벡터로 만든 과정에서 이미 " " 표시가 입력되어 있엇음

# password = 'pw' 로 입력시 "'1234'" 로 표기되어 오류 발생
# password = pw 로 변경 후 성공

#db내 데이터  불러오기

country <- dbGetQuery(
  db,
  "select * from country ;"
)

#데이터 불러오기 성공

#실패 사례 - from 뒤에 'country' 로 입력해서 에러 회신
#'' 항목 제외 후 성공
lang <- dbGetQuery(
  db,
  "select * from countrylanguage ;"
)


str(lang)




lang_1 <- lang %>% 
  group_by(CountryCode) %>% 
  summarise(per = max(Percentage)) %>% 
  data.frame %>% 
  left_join(lang, by = c("CountryCode" = "CountryCode", "per" = "Percentage" ))
 

#국가코드별 가장 많이 사용하는 언어만 선택하고 
#나머지 정보는 기존 테이블에서 가져와 붙임

#중간에 data.frame을 넣어 별도의 테이블로 만들고
#기존의 테이블과 조인함

lang %>% 
  group_by(CountryCode) %>% 
  summarise(per = max(Percentage)) %>% 
  data.frame %>% 
  left_join(lang, by = c("CountryCode" = "CountryCode", "per" = "Percentage" )) %>% 
  data.frame %>% 
  group_by(Language) %>% 
  summarise(n = n()) %>% 
  filter(n >2) %>% 
  arrange(-n)

#가장 많이 사용 하는 언저 확인


lang %>% 
  group_by(CountryCode) %>% 
  summarise(per = max(Percentage)) %>% 
  data.frame %>% 
  left_join(lang, by = c("CountryCode" = "CountryCode", "per" = "Percentage" )) %>% 
  data.frame %>% 
  group_by(IsOfficial) %>% 
  summarise(n = n()) %>% 
  mutate(per = n/sum(n))

#30% 가량의 국가에서는 표준어가 아닌 언어를 더 많이 사용한다



str(lang_1)
str(country)

sum(duplicated(country$Code))
#국가 템플리에는 국가코드 중복은 없다


glimpse(country)

country %>% 
  left_join(lang_1, by = c("Code" = "CountryCode"))


#접속종료
dbDisconnect(db)

