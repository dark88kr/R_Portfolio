library(readxl)
library(dplyr)
library(lubridate)
library(tidymodels)

# 앞선 전처리 작업을 마친 call , pledge 데이터를 합쳐서 
# train, test 파일로 나눈다


#call 데이터 불러오기
call <- read_excel("call.xlsx")
#call 필요컬럼 선별
call_sel <- call %>% select(`Serial Number`, `Task Group` ,`Task Closed Date`)
#신규변수 생성
call_sel <- call_sel %>% 
  mutate(new  = paste(`Serial Number`,`Task Group`, sep = "") ) %>% 
  group_by(new) %>% 
  mutate(count = n())

#가장 최신 데이터 이외에 데이터는 삭제 
#count 함수로 해당 기간 몇번의 전화 이력이 있는지 확인
call_sel <- call_sel %>%
  group_by(`Serial Number`) %>% 
  mutate(count = n()) %>% 
  arrange(desc(`Task Closed Date`)) %>% 
  distinct(`Serial Number`, .keep_all = TRUE)

#필요컬럼 재선정, chr -> fct 로 변경
call_tran <- 
  call_sel %>% 
  select(`Serial Number`, `Task Closed Date`, count) %>% 
  mutate_if(is.character, as.factor)
  
call_tran$`Task Closed Date` <- as.numeric(call_tran$`Task Closed Date`)

call_tran <- as.data.frame(call_tran)

glimpse(call_tran)


#####call_tran 사용


#pledge 데이터 불러오기

new <- read_excel("new.xlsx")

#call 데이터 맞는 데이터로 선별
new_date <- new %>% 
  filter(`Pledge Created` >= as.Date("2023-07-01") & `Pledge Created` <= as.Date("2024-01-31")) %>% 
  filter(Frequency == 1)

#47개의 컬럼 중 유의한 컬럼만 선택 
new_sel <- new_date %>% 
  select(SerialNo, DOB, Gender, SourceCode, Bank, TotalContribution, ChannelType,
         D0, `Pledge Created`, successful_transactions_count, Campaign)

#목표변수 치환, 독립변수 속성변경
new_tran <- 
  new_sel %>% 
  mutate(total =TotalContribution) %>% 
  dplyr::select(- TotalContribution) %>% 
  mutate_if(is.character,as.factor)

#날짜 컬럼을 숫자로 변경
new_tran$DOB <- as.numeric(new_tran$DOB)
new_tran$D0 <- as.numeric(new_tran$D0)
new_tran$`Pledge Created` <- as.numeric(new_tran$`Pledge Created`)

#변수가 많은 fct 변수 변환
new_tran <- 
  new_tran %>% 
  mutate(cam = ifelse(is.na(Campaign),0,1)) %>% 
  dplyr::select(-Campaign)

new_tran$cam <- as.factor(new_tran$cam)  
new_tran$SerialNo <- as.character(new_tran$SerialNo)


glimpse(new_tran)
glimpse(call_tran)

#pledge 데이터를 기준으로 call 데이터 합치고 없는 정보는 0으로 채움

#call의 `Serial Number`는 new의 SerialNo와 같은 같이다
#call 테이블의 컬럼면 변경
library(data.table)
setnames(call_tran, old = "Serial Number", new = "SerialNo")
setnames(call_tran, old = "Task Closed Date", new = "task date")

new_tran %>% 
  left_join(call_tran, by = SerialNo)

#테스트 코드로 병합에 실패 하였다
#by = SerialNo -> by = "serialNo"로 대소문자에 구애받지 않는 코드로 변경

pled <- 
  new_tran %>% 
  left_join(call_tran, by = "SerialNo")

glimpse(pled)

#병합 당시 일치 하지 않는 값에 채워진 NA값 대신 0으로 넣기

pled <- 
  pled %>% 
  mutate(task = ifelse(is.na(`task date`),0,`task date`)) %>% 
  mutate(count = ifelse(is.na(count),0,count)) %>% 
  select(-`task date`)

glimpse(pled)

colSums(is.na(pled))
skimr::skim(pled)


#마지막 NA 처리
#gender 와 channeltype는 0이아닌 emt로 변경 
#do와 dob의 경우 날짜를 숫자로 변경했기 때문에 0으로 변경

pled <- 
  pled %>% 
  mutate(DOB = ifelse(is.na(DOB),0,DOB)) %>% 
  mutate(D0 = ifelse(is.na(D0),0,D0)) %>% 
  mutate(Gender = if_else(is.na(Gender),"emt",Gender)) %>% 
  mutate(ChannelType = if_else(is.na(ChannelType),"emt",ChannelType)) %>% 
  mutate_if(is.character,as.factor)

pled$SerialNo <-as.character(pled$SerialNo) 



glimpse(pled)
colSums(is.na(pled))


######전처리 완료

#test, train 데이터 분류

set.seed(123)
split <- initial_split(pled, prop = 0.7)
train<- training(split)
test <- testing(split)


#레시피 함수 사용, total이 y 이며 나머지는 x

rf_recipe <- 
  recipe(total ~ ., data = train) %>%
  step_center(all_numeric_predictors()) %>%      #평균으로부터 중심화
  step_scale(all_numeric_predictors()) %>%       #표준편차로 스케일링
  step_rm(SerialNo) %>%                          #시리얼 번호는 제외
  step_zv(all_numeric_predictors()) %>%          #제로 분산을 가진 변수 제외(분산이 0이며 예측 안됨)
  step_normalize(all_numeric_predictors()) %>%   #정규화
  step_log(total) # 'total' 열을 로그 변환


# Recipe 적용
rf_pre <- rf_recipe %>% prep() %>% bake(new_data = train)
summary(rf_pre)

#모델생성
#랜덤포레스트 모델 사용 
#mtry값 확인을 위해 전체 독립변수 수 확인

mty <- rf_recipe %>% 
  prep() %>% 
  bake(new_data = NULL)

num_ptors <- ncol(mty) - 1
num_ptors

#11의 제급곤과 로그 값도 3의 근사치로 확인
#mtry = 3

rf_model <- 
  rand_forest(mtry = 3) %>%
  set_engine("ranger") %>%
  set_mode("regression")


pre_train <- rf_recipe %>% prep() %>% bake(new_data = train)
pre_test <- rf_recipe %>% prep() %>% bake(new_data = test)

glimpse(pre_train)
glimpse(pre_test)


rf_fit <- 
  rf_model %>% 
  fit(total ~ ., data = pre_train)

summary(rf_fit)

rf_pred <- rf_fit %>% predict(new_data = pre_test) 

summary(rf_pred)

모댈이 맞지 않는다.

#y값 변경 취소
rf_recipe_1 <- 
  recipe(total ~ ., data = train) %>%
  step_center(all_numeric_predictors()) %>%      #평균으로부터 중심화
  step_scale(all_numeric_predictors()) %>%       #표준편차로 스케일링
  step_rm(SerialNo) %>%                          #시리얼 번호는 제외
  step_zv(all_numeric_predictors()) %>%          #제로 분산을 가진 변수 제외(분산이 0이며 예측 안됨)
  step_normalize(all_numeric_predictors())   #정규화

#변경없는 y 값으로 레시피 적용
pre_train <- rf_recipe_1 %>% prep() %>% bake(new_data = train)
pre_test <- rf_recipe_1 %>% prep() %>% bake(new_data = test)

#모델 훈련
rf_fit <- 
  rf_model %>% 
  fit(total ~ ., data = pre_train)

#테스트 데이터로 모델 학습
rf_pred <- 
  rf_fit %>% 
  predict(new_data = pre_test) 

#실제데이터와 학습된 데이터를 하나의 테이블로 만들기
com_data <- data.frame(truth = pre_test$total, estimate = rf_pred$.pred)
com_data_tbl <- as_tibble(com_data)

#R^2 값으로 모델 평가 
r_squared <- 1 - sum((com_data_tbl$truth - com_data_tbl$estimate)^2) / sum((com_data_tbl$truth - mean(com_data_tbl$truth))^2)
r_squared #57.8%

#해당 모형은 한사람의 약정의 총후원금을 약 58% 설명하는 모형
#파라미터 튜닝이나 다른 독립변수 추가 등으로 설명력 높여 재시도 예정
#tidymodels로 시도 하였으나 인스톨 문제 등이 산재해 있어 
#패키지 업데이트 이후 재 시도.



