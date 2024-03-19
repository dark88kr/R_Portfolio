library(rmarkdown)
library(dplyr)
library(readxl)
library(lubridate)


#취소 기록 불러오기
can <- read_excel("cancell.xlsx")

glimpse(can)

#Change column names to lowercase

colnames(can) <- tolower(colnames(can))

#remove Personal Information and select column

can_sel <- 
  can %>% 
  select(sourcecode, canceldate,cancelcode,cancelreasondescription,
         serialno,bank,frequency,donationamount,dob,
         gender,firstdeduction,totalcollected)

glimpse(can_sel)

#취소데이터의 범위 확인

can_sel %>% 
  summarise(max = max(canceldate),
            min = min(canceldate))

#23년 12월 한달간의 기록이다


#전화기록 불러오기
call <- read_excel("call.xlsx")

glimpse(call)

#Change column names to lowercase

colnames(call) <- tolower(colnames(call))

#전화데이터의 범위 확인

call %>% 
  summarise(max = max(`task closed date`),
            min = min(`task closed date`))

#23년 7월부터 24년 1월까지의 기록이다
#필요한 컬럼 선별

#키 컬럼의 이름을 동일하게 변경
colnames(call)[2] <- "serialno"

call_sel <-
  call %>% 
  select(serialno, user, `task group` ,`task closed date`)

glimpse(call_sel) 

#시리얼번호와 날짜 기준으로 가장 최근의 기록만 남길것

call_sel <- 
  call_sel %>% 
  arrange(desc(`task closed date`)) %>%
  distinct(serialno, .keep_all = TRUE)

#시리얼번호 별 하나의 기록만 가진 것을 확인 
call_sel %>% 
  group_by(serialno) %>% 
  summarise(n = n()) %>% 
  arrange(n)

#날짜와 시간을 두개의 컬럼으로 나눠서 사용
call_sel <- 
  call_sel %>% 
  mutate(date = as.Date(`task closed date`),
         time =  format(strptime(`task closed date`, format="%Y-%m-%d %H:%M:%S"), "%H:%M:%S")) %>% 
  select(- `task closed date`)

call_sel$time <- hms(call_sel$time)

glimpse(call_sel)

#각 전화 그룹별 평균 시간대 확인
call_sel %>% 
  mutate(hour = hour(time),
         mins = minute(time)) %>% 
  group_by(`task group`) %>% 
  summarise(avg_h = median(hour),
            avg_m = median(mins))


#시리얼번호를 키값으로 취소와 전화 데이터 병합

can_call <- 
  can_sel %>% 
  left_join(call_sel, by = "serialno")

glimpse(can_call)  


#task별로 취소 후원자 숫자와 평균 시간대를 확인
can_call %>% 
  mutate(hour = hour(time),
         minu = minute(time)) %>% 
  group_by(`task group`) %>% 
  summarise(count = n(),
            avg_h = median(hour),
            avg_m = median(minu)) %>% 
  arrange(desc(count))
 
#인바운드로 해지 전화가 압도적이며 후원권유나 추가 후원 요청전화에도 취소가 많다
#인바운드로 해지한 후원자의 평균 정보 확인
#TV로 인입된 후원자와 홈페이지의 총 후원금 가장 높았다

can_call %>% 
  filter(`task group` == "Inbound") %>% 
  group_by(sourcecode) %>%
  summarise(median = median(totalcollected))

#TV로 인인된 후원자와 홈페이지로 들어온 사람들의 취소 사유 확인

can_call %>%
  filter(sourcecode == "Homepage" | sourcecode == "LLF") %>% 
  group_by(cancelreasondescription) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

#자동으로 취소처리 된 사유를 제외하면 경제적 요인이 가장 많았다.
#경제적 요인으로 취소한 후원자의 전화 온 시간을 확인해보자


can_call %>% 
  filter(cancelreasondescription == "Financial constraints" | 
           cancelreasondescription == "Donor refuse to provide reason") %>%
  mutate(hour = hour(time),
         minu = minute(time)) %>% 
  summarise(mean = mean(donationamount),
            avg_h = median(hour),
            avg_m = median(minu))
  
#취소후원자의 계좌사용 정지와 기간지정, 그리고 경제적 어려움이 가장 많았으며
#취소목적으로 전화 하여 취소한 후원자가 많았으며 
#컨택이 이루어지면 취소가 발생할수 있다는 정보를 확인

#컨택이 이루어지면 취소가 많이 발생하는지
#인바운드 중 취소로 이어지는 비율은 어느 정도인지
#USER별 취소위 차이가 있는지 확인이 필요






