library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)

call <- read_excel("call.xlsx")

glimpse(call)

call %>% filter(`Serial Number` == "2167")
#두건 이상의 데이터 확인

call_sel <- call %>% select(`Serial Number`, `Task Group` ,`Task Closed Date`)

call_sel <- call_sel %>% 
  mutate(new  = paste(`Serial Number`,`Task Group`, sep = "") ) %>% 
  group_by(new) %>% 
  mutate(count = n())

#Task별로 나누어 보았으나 call의 횟수를 카운드하여 새로운 컬럼으로 생성하는
#작업과 맞지 않는다

call_sel %>%
  group_by(`Serial Number`) %>% 
  mutate(count = n()) %>% 
  arrange(desc(`Task Closed Date`)) %>% 
  head()


#해당 작업으로 call 데이터 테이블은 23년 7월부터 24년 1월까지로 확인
#약정 생성일이 23년 7월부터 24년 1월까지의 데이터만 사용 예정
#전처리의 목적은 전화 횟수와 평생 후원금액의 상관관계를 보고 싶기 때문.


call_sel <- call_sel %>%
  group_by(`Serial Number`) %>% 
  mutate(count = n()) %>% 
  arrange(desc(`Task Closed Date`)) %>% 
  distinct(`Serial Number`, .keep_all = TRUE)

call_sel %>% filter(`Serial Number` == "2167")

#가장 최신 데이터 이외에 데이터는 삭제 되고 
#count 함수로 해당 기간 몇번의 전화 이력이 있는지 확인

#call_sel 데이터를 사용 예정






call_sel %>% filter(`Serial Number` == "2167")

