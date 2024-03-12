library(readxl)
library(dplyr)
library(lubridate)
library(tidymodels)

new <- read_excel("new.xlsx")

glimpse(new)

#후원자 테이블, 컬럼이 너무 많다
#TotalContribution 이 목표 변수
#call 테이블 범위에서 데이터가 얼마나 포함되어 있는지 확인
#등록일 기준으로 확인


new %>% 
  select(SerialNo,`Pledge Created`) %>% 
  filter(`Pledge Created` >= as.Date("2023-07-01") & `Pledge Created` <= as.Date("2024-01-31"))

#2180개의 데이터 행 확인


new_date <- new %>% 
    filter(`Pledge Created` >= as.Date("2023-07-01") & `Pledge Created` <= as.Date("2024-01-31")) %>% 
  filter(Frequency == 1)
new_date

#그중 정기 후원만 선택
#1,908행의 데이터만 선택하여 call 데이터와 함께 사용 예정

#47개의 컬럼 중 유의한 컬럼만 선택 

new_sel <- new_date %>% 
  select(SerialNo, DOB, Gender, SourceCode, Bank, TotalContribution, ChannelType,
         D0, `Pledge Created`, successful_transactions_count, Campaign)

glimpse(new_sel)

ggplot(data = new_sel, aes(x = TotalContribution)) + 
  geom_histogram(fill = "blue", color = "black", alpha = 1)

#그림으로 보는 것처럼 왼쪽으로 치우친 형태의 분포를 가지고 있어
#변경이 필요해 보인다.

new_sel %>% 
  select(TotalContribution) %>% 
  mutate(total = TotalContribution / 10000) %>% 
  ggplot( aes(x= total))+
  geom_histogram(fill = "blue", color = "black", bins = 100) +
  scale_x_continuous(breaks = seq(0,100,5), labels = seq(0,100,5)) 

#y값 변환을 위한 람다 값 위한 패키지
library(MASS)
boxcox(TotalContribution, data = new_sel)$lamda

#0값이 포함되어 있어 변환이 불가하다.

new_sel %>% 
  dplyr:: select(TotalContribution) %>% 
  mutate(total = (TotalContribution / 10000) +1) %>%
  boxcox(total ~ 1)$lamda -> 실패값

#양수값으로 변경하였으나 실패하여 해당 값을 log 변환이후 그래프 재확인

new_sel %>% 
  mutate(total =log(TotalContribution )) %>% 
  ggplot( aes(x= total))+
  geom_histogram(fill = "blue", color = "black", bins = 100) +
  scale_x_continuous(breaks = seq(0,100,5), labels = seq(0,100,5))

#왼쪽 치우침 현상 해결
#log취한 갑을 목표변수로 설정
#유의미한 chr 속성을 fct로 변경

new_tran <- 
  new_sel %>% 
  mutate(total =log(TotalContribution )) %>% 
  dplyr::select(- TotalContribution) %>% 
  mutate_if(is.character,as.factor)
 
#날짜 컬럼을 숫자로 변경
#회귀분석의 경우, 해당 컬럼을 쉽게 사용할수 있도록 날짜를 숫자로 뱐경

new_tran$DOB <- as.numeric(new_tran$DOB)
new_tran$D0 <- as.numeric(new_tran$D0)
new_tran$`Pledge Created` <- as.numeric(new_tran$`Pledge Created`)

glimpse(new_tran)

# 각 변환된 fct 변수의 법위값 확인

library(skimr)

skim(new_tran)

#시리얼번호는 id값으로 사용될 예정이라서 상관없으나
#campaign 의 수가 22개로 확인됨
#campaign 별 후원의 차이를 확인하고 싶은 부분이 아니라서 변환이 필요함
#해당컬럽값 유무를 나타내는 값으로 변경 

new_tran <- 
  new_tran %>% 
  mutate(cam = ifelse(is.na(Campaign),0,1)) %>% 
  dplyr::select(-Campaign)

new_tran$cam <- as.factor(new_tran$cam)  

glimpse(new_tran)
skim(new_tran)

##new_tran 데이터를 사용 예정





  

