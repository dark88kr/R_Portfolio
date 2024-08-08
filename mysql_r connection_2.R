# MYSQL_R connection_2
# sql 분석을 r로 연결하여 처리하기

######필요 라이브러리 불러오기

library(DBI)
library(RMySQL)
library(tidyverse)
library(lubridate)

###### mysql 서버와 연결하기

#sql 접속키 설정
pw <-  Sys.getenv("mysql")

sqldb <- dbConnect(
  MySQL(),
  user = 'root',
  password = pw ,
  host = '127.0.0.1',
  dbname = 'edu'
)

# edu 스키마와 연결 성공


###### 스키마 내부의 테이블을 데이터 프레임 형식으로 불러오기

member <- dbGetQuery(
  sqldb,
  "select * from car_member ;"
)

order <- dbGetQuery(
  sqldb,
  "select * from car_order ;"
)

detail <- dbGetQuery(
  sqldb,
  "select * from car_orderdetail ;"
)

product <- dbGetQuery(
  sqldb,
  "select * from car_product ;"
)

store <- dbGetQuery(
  sqldb,
  "select * from car_store ;"
)

# edu 스키마의 5개 데이블 불러오기 완료



######### 요청된 데이터 마트 구성 발식

# order 테이블을 외쪽으로 하여 모든 데이틀을 붙인다
# order 1, detail 2 store 3 product 4 member 5
# detail 의 수량과 product의 가격을 곱하여 sales amt 열 생성

str(order)
str(detail)

car_mart <- order %>% left_join(detail, by = c("order_no" = "order_no"))

str(car_mart)
str(store)
car_mart <- car_mart %>% left_join(store, by = c("store_cd" = "store_cd"))

str(car_mart)
str(product)
car_mart <- car_mart %>% left_join(product, by = c("prod_cd" = "prod_cd"))

str(car_mart)
str(member)
car_mart <- car_mart %>% left_join(member, by = c("mem_no" = "mem_no"))


str(car_mart)

#car_mart %>% 
#  mutate(sales_amt = quantity * price) %>% 
#  select(sales_amt)
# 실패 사례 - product 테이블의 price 가 chr의 속성유지, 속성변경 필요
# price 컬럼에 , 역시 제거 필요


car_mart$price <- readr::parse_number(car_mart$price)
str(car_mart)

car_mart %>% 
  mutate(sales_amt = quantity * price) 

# 실패 사례 -order no가 중복 되었다 - 실패 아님

sum(duplicated(order$order_no))
sum(duplicated(detail$order_no))

detail %>% filter(order_no == 2000003)


car_mart <- 
  car_mart %>% 
  mutate(sales_amt = quantity * price) 

## 데이터마트 구축 완료


###임시 컬럼 생성
#연령대 컬럼 생성 ex) 10대 20대,... 60대 이상 

car_mart <- 
  car_mart %>% 
  mutate(ageband = ifelse(age <20, "20대미만",
                          ifelse(age < 30 , "20대",
                                 ifelse(age < 40, "30대",
                                        ifelse(age <50, "40대",
                                               ifelse(age < 60 , "50대", "60대 이상")))))) 

# 검정

car_mart %>% filter(age <30) %>% select(ageband) %>% distinct()
car_mart %>% filter(age <40) %>% select(ageband) %>% distinct() 


## 연령대 컬럼 생성 완료

## 성별, 연령대 별 구매자 수


car_mart %>% 
  group_by(gender, ageband) %>% 
  summarise(n = n()) 


#성별, 연령대, 년도별 구매자 수
#날짜 컬럼으로 속성변경하고 as.Date 사용, 년도정보만 추출 year 사용

car_mart$order_date <- as.Date(car_mart$order_date)
str(car_mart)

car_mart %>% 
  group_by(gender, ageband, year(order_date)) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x = ageband, y = n, color = factor(`year(order_date)`), group = `year(order_date)`)) +
  geom_line(aes(linetype = factor(`year(order_date)`)), size = 1) +
  geom_point(size = 2) +
  facet_wrap(~ gender) +
  labs(title = "Gender and Ageband vs. Number of Orders",
       x = "Age Band",
       y = "Number of Orders",
       color = "Year",
       linetype = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#접속종료
dbDisconnect(sqldb)


