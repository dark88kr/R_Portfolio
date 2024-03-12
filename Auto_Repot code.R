---
  title: "Transaction analysis"
author: "Yonggi_Yeom"
date: "2023-08-18"
output:
  html_document:
  df_print: paged
---
  ```{r include=FALSE}
library(tidyverse)
library(lubridate)
library(readxl)
```

```{r include=FALSE}
a <- Sys.Date()
b <- month(a)-1 
```

# `r b`월 Transaction Report

### 단체 `r b`월의 Transation Report 기반 개발사별 APP, REJ의 % 비교
```{r include=FALSE} 
#데이터 불러오기
tran <- read_excel("tran.xlsx")

#데이터 필요 컬럼만 선정
tran_se <- 
  tran %>% select(SourceCode, SerialNo, DonationAmount, AuthorisationType, DateAuthorised, RejectReasonType, NetCollected,Frequency, TrnxBank)

```


```{r include=FALSE}
#내림차순 정렬은 desc()이나 arrange 와 함께 사용하기 위해서는 arrange(desc())로 구현해야함
#distinct로 중복 제거, .keep_all = T 옵션을 사용해야 나머지 컬림이 남는다
#distinct(컬럼, 컬럼2, 컬럼3, .keep_all = T)로 사용

tran_no_dup <- 
  tran_se %>% 
  arrange(desc(DateAuthorised)) %>% 
  distinct(SerialNo, .keep_all = T)

#데이터 전처리 완료
```


#### 각 개발사 별 성공 비율

```{r echo=FALSE, message=FALSE}
tran_no_dup %>% 
  group_by(SourceCode, AuthorisationType) %>% 
  summarise( n = n()) %>% 
  data.frame %>% 
  group_by(SourceCode) %>% 
  mutate(per = (n/ sum(n)) * 100)
```

##### 성공실패 비율 그래프 
```{r echo=FALSE, message=FALSE}
app_rej <- tran_no_dup %>% 
  group_by(SourceCode, AuthorisationType) %>% 
  summarise( n = n()) %>% 
  data.frame %>% 
  group_by(SourceCode) %>% 
  mutate(per = round((n/ sum(n)) * 100),3) %>% 
  data.frame


app_rej <- app_rej %>% mutate_if(is.character,as.factor)

str(app_rej)

ggplot(data = app_rej, aes(x=AuthorisationType, y = per)) + geom_col(aes(fill =AuthorisationType )) + facet_wrap(.~ SourceCode, ncol = 3 )+
  geom_label(aes(label = per))+ theme_bw()
```

#### 각 개발사 별 DNH, NDHN 비율

```{r echo=FALSE, message=FALSE}
tran_no_dup %>% 
  filter(AuthorisationType == "REJ" ) %>% 
  group_by(SourceCode, RejectReasonType) %>% 
  summarise(n = n()) %>% 
  data.frame %>% 
  group_by(SourceCode) %>% 
  mutate(per = (n/sum(n))*100)
```


#### 각 개발사 별 DNH, NDHN 비율 그레프
```{r echo=FALSE, message=FALSE}
tran_no_dup %>% 
  filter(AuthorisationType == "REJ") %>% 
  group_by(SourceCode, RejectReasonType) %>% 
  summarise(n = n()) %>% 
  data.frame %>% 
  mutate_if(is.character, as.factor) %>% 
  group_by(SourceCode) %>% 
  mutate(per = round((n/ sum(n)) * 100),3) %>% 
  ggplot(aes(x = RejectReasonType , y = per))+
  geom_col(aes(fill = RejectReasonType))+ 
  facet_wrap(.~SourceCode, ncol = 3)+
  geom_label(aes(label = per))+ theme_bw()

```




