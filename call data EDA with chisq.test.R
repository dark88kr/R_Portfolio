library(rmarkdown)
library(dplyr)
library(readxl)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(stringr)
library(tidyr)

#전화기록 불러오기
call <- read_excel("call.xlsx")

#Change column names to lowercase
colnames(call) <- tolower(colnames(call))
colnames(call)[2] <- "serialno"
glimpse(call)

#인바운드 및 취소 연관관계를 확인을 위한 컬럼 선별

call_cols <- 
  call %>% 
  select(serialno, `source code`, user, `task group`, `task closed date`, `task type`, 
         outcome, `pledge status`, `donation amount`)

#컬럼별 값 확인 - #결측치 없이 chr 컬럼이 다수 존재
skimr::skim(call_cols)

glimpse(call_cols)
#분포 확인
plot1 <- ggplot(call_cols, aes(x = `source code`)) +
  geom_bar() +
  labs(title = "Distribution of source_code")

plot2 <- ggplot(call_cols, aes(x = user)) +
  geom_bar() +
  labs(title = "Distribution of user")

plot3 <- ggplot(call_cols, aes(x = `task group`)) +
  geom_bar() +
  labs(title = "Distribution of task_group")

plot4 <- ggplot(call_cols, aes(x = `task type`)) +
  geom_bar() +
  labs(title = "Distribution of task_type")

# 4분할된 화면에 시각화 나타내기
grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)

plot5 <- ggplot(call_cols, aes(x = outcome)) +
  geom_bar() +
  labs(title = "Distribution of outcome")

plot6 <- ggplot(call_cols, aes(x = `pledge status`)) +
  geom_bar() +
  labs(title = "Distribution of pledge_status")

grid.arrange(plot5,plot6)

#취소 전화와 관련된 데이터 시각화
grid.arrange(plot2,plot3,plot4,plot5, ncol = 2)


#전화 수신 량의 차이가 있고 cancell 숫자가 많다

call_cols %>% 
  filter(outcome == "Cancelled") %>% 
  group_by(`task group`) %>% 
  summarise(n = n()) %>% 
  data.frame %>% 
  mutate(perc = (n / sum(n))*100) %>% 
  arrange(desc(n))

#상담사를 거치지 않는 webaccess 제외하고 시각화

call_cols %>% 
  filter(outcome == "Cancelled") %>% 
  filter(`task group` != "WebAccess") %>% 
  group_by(`task group`) %>% 
  summarise(n = n()) %>% 
  data.frame %>% 
  mutate(perc = (n / sum(n))*100) %>% 
  arrange(desc(n))

#인바운드가 70%를 차지한다

call_cols %>% 
  filter(outcome == "Cancelled") %>%
  group_by(`task type`) %>% 
  summarise(n = n()) %>% 
  data.frame %>% 
  mutate(prec = (n / sum(n))*100) %>% 
  arrange(desc(n))
#콜의 목적을 나타내는 인바운드도 해지를 포함하면 84% 이상을 차지 한다


call_cols %>% 
  filter(`task group` == "Inbound") %>% 
  group_by(outcome) %>% 
  summarise(n = n()) %>% 
  data.frame %>% 
  mutate(perc = n / sum(n),
         per = scales::percent(perc)) %>% 
  select(-perc) %>% 
  arrange(desc(n))

#인바운드 중 53% 가 취소로 이어지고 나머지는 해지 방어가 이루어 진다
call_cols %>%
  filter(`task group` == "Inbound") %>% 
  group_by(username, outcome) %>%
  summarise(n = n())

#유저별로 취소와 유지 비율을 확인하기 이하여 새로운 컬럼을 생성

call_cols <- 
  call_cols %>% 
  mutate(username = str_extract(user, "^[^.]+")) %>% 
  select(-user)

colnames(call_cols)[9] <- "user"

#인바운드만 있는 새로운 데이터 프레임 생성
call_in <- 
  call_cols %>% 
  filter(`task group` == "Inbound")

call_in %>% 
  group_by(outcome) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

#아웃컴에서 cacelled 를 제외한 나머지는 방어로 보고 이진카테고리 형성
call_in <- 
  call_in %>% 
  mutate(outcome = ifelse(outcome == "Cancelled", "cancelled", "defense"))
  
#유저중 전화와 관련 없는 사람제거 하기

call_in %>% 
  select(user) %>% 
  distinct()

call_in <- 
  call_in %>% 
  filter(user != "yonggi") %>% 
  filter(user != "heeweon") 

  
call_in %>% 
  group_by(user) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

call_in <- 
  call_in %>% 
  mutate_if(is.character, as.factor)

call_in$serialno <- as.character(call_in$serialno)

glimpse(call_in)


###user와 outcome 검증을 위한 테이블표 생성

call_table <- 
  call_in %>% 
  count(user, outcome) %>% 
  spread(outcome, n , fill = 0)

#먼저 cancal과 defense의 평균 차이가 나는지 t -test 검증

#h0 :두 그룹의 평균차이가 없다
#h1 :두 그룹의 평균차이가 있다

t.test(call_table$cancelled, call_table$defense, parid = FALSE)

#P값이 0.9413으로 평균의 차이가 있다는 H1을 기각하고 
#두 그룹의 평균차이는 없다로 결론


call_tale <- 
  table(call_in$user,call_in$outcome)

call_table
proportions(call_table) *100

ggplot(call_in , aes(x = outcome, fill = user)) + geom_bar()

#유저의 수가 많아서 숙련과 비숙련으로 나눠서 진행


call_in<- 
  call_in %>% 
  mutate(expert = ifelse(user == "myunghoon" | user == "sangok" 
                         |user == "jihye" | user == "wonhee","exp","nobi"))

call_table <- xtabs(~expert + outcome, data = call_in)


call_table %>% 
  chisq.test() %>% 
  broom::tidy()

proportions(call_table) * 100
prop.table(call_table, margin = 1)
prop.table(call_table, margin = 2)

ggplot(call_in , aes(x = outcome, fill = expert)) + geom_bar()


#숙련그룹과 비숙련 그룹의 outcome 값의 차이는 유의미하다 - p값 0.02이므로
#그래프 확인시 cancel값을 차지하는 비율은 숙련, 비숙련이 비슷하나
#defense값을 차지하는 비율이 달랐다


#숙련그룹을 늘린다면 inbound 경로로 들어오는 취소를 더 많이 방어 가능할수 있다.

