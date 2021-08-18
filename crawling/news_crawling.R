
###### 쿠팡 관련 뉴스 크롤링 (10.01.01-21.07.31) ######
#### 연합뉴스 ####
## 패키지 설치 -----------------------------------------------------------------
library(rvest)
library(dplyr)
library(tidyverse)

## 변수 입력하기(test) ---------------------------------------------------------
QUERY <- "쿠팡" # 검색키워드
DATE  <- as.Date(as.character(20100101),
                 format="%Y%m%d") # 검색시작날짜 & 검색종료날짜
DATE1 <- format(DATE, "%Y.%m.%d")
DATE2 <- format(DATE, "%Y%m%d")
PAGE <- 1

naver_url_1 <- "https://search.naver.com/search.naver?where=news&query="
naver_url_2 <- "&sm=tab_opt&sort=2&photo=0&field=0&pd=3&ds="
naver_url_3 <- "&de="
naver_url_4 <- "&docid=&related=0&mynews=1&office_type=1&office_section_code=2&news_office_checked=1001&nso=so%3Ar%2Cp%3Afrom" # 연합뉴스 코드 -> 1001
naver_url_5 <- "to"
naver_url_6 <- "&start="

naver_url <- paste0(naver_url_1, QUERY, naver_url_2, DATE1, naver_url_3, DATE1, naver_url_4, DATE2, naver_url_5, DATE2, naver_url_6, PAGE)

naver_url 

## 날짜 리스트 만들기 ----------------------------------------------------------
DATE_START <- as.Date(as.character(20100101), format = "%Y%m%d")
DATE_END <- as.Date(as.character(20210731), format = "%Y%m%d")
DATE <- DATE_START:DATE_END
DATE <- as.Date(DATE, origin = "1970-01-01") # origin은 임의의 값 
DATE

## 게시물 번호 -----------------------------------------------------------------
PAGE<- seq(from = 1, to = 21, by = 10) #최초 30개 기사까지 검색 (보통 1-2개)

## 네이버 검색결과 url 리스트 만들기 -------------------------------------------
naver_url_list <- c()
for (date_i in DATE){
  for (page_i in PAGE){
    dt1 <- format(as.Date(date_i, origin = "1970-01-01"), "%Y.%m.%d")
    dt2 <- format(as.Date(date_i, origin = "1970-01-01"), "%Y%m%d")
    naver_url <- paste0(naver_url_1, QUERY, naver_url_2, dt1, naver_url_3, dt1, naver_url_4, dt2, naver_url_5, dt2, naver_url_6, PAGE) 
    naver_url_list <- c(naver_url_list, naver_url)
  }
}
head(naver_url_list, 5)

## url 읽어오기 ----------------------------------------------------------------
news_url <- c()
news_date <- c()
for (date_i in DATE){
  for (page_i in PAGE){
    dt1 <- format(as.Date(date_i, origin = "1970-01-01"), "%Y.%m.%d")
    dt2 <- format(as.Date(date_i, origin = "1970-01-01"), "%Y%m%d")
    naver_url <- paste0(naver_url_1, QUERY, naver_url_2, dt1, naver_url_3, dt1, naver_url_4, dt2, naver_url_5, dt2, naver_url_6, page_i) 
    html <- read_html(naver_url)
    temp <- unique(html_nodes(html, '#main_pack') %>%
                     html_nodes('a.info') %>%
                     html_attr('href'))
    news_url <- c(news_url, temp)
    news_date <- c(news_date, rep(dt1, length(temp)))
  }
  print(dt1) # 생략 가능
}

## 불필요한 url 처리 -----------------------------------------------------------
NEWS0 <- as.data.frame(cbind(date=news_date, url=news_url, query=QUERY))
NEWS1 <- NEWS0[which(grepl("news.naver.com",NEWS0$url)),] # 네이버뉴스만 대상
NEWS1 <- NEWS1[which(!grepl("sports.news.naver.com",NEWS1$url)),] # 스포츠 제외
NEWS2 <- NEWS1[!duplicated(NEWS1), ] # 중복 링크 제거 

## 뉴스 기사 및 제목 추출 ------------------------------------------------------
NEWS2$news_title   <- ""
NEWS2$news_content <- ""

for (i in 1:dim(NEWS2)[1]){
  html <- read_html(as.character(NEWS2$url[i]))
  temp_news_title   <- repair_encoding(html_text(html_nodes(html,'#articleTitle')),from = 'utf-8')
  temp_news_content <- repair_encoding(html_text(html_nodes(html,'#articleBodyContents')),from = 'utf-8')
  if (length(temp_news_title)>0){
    NEWS2$news_title[i]   <- temp_news_title
    NEWS2$news_content[i] <- temp_news_content
  }
}

NEWS2$news_content <- gsub("// flash 오류를 우회하기 위한 함수 추가\nfunction _flash_removeCallback()", "", NEWS2$news_content) # 중복된 문자열 제거
NEWS <- NEWS2 # 최종 결과 저장 

write.csv(NEWS, "YH_news.csv")

#### 동아일보 ####
## 변수 입력하기(test) ---------------------------------------------------------
QUERY <- "쿠팡" # 검색키워드
DATE  <- as.Date(as.character(20100101),
                 format="%Y%m%d") # 검색시작날짜 & 검색종료날짜
DATE1 <- format(DATE, "%Y.%m.%d")
DATE2 <- format(DATE, "%Y%m%d")
PAGE <- 1

naver_url_1 <- "https://search.naver.com/search.naver?where=news&query="
naver_url_2 <- "&sm=tab_opt&sort=2&photo=0&field=0&pd=3&ds="
naver_url_3 <- "&de="
naver_url_4 <- "&docid=&related=0&mynews=1&office_type=1&office_section_code=2&news_office_checked=1020&nso=so%3Ar%2Cp%3Afrom" # 동아일보 코드 -> 1020
naver_url_5 <- "to"
naver_url_6 <- "&start="

naver_url <- paste0(naver_url_1, QUERY, naver_url_2, DATE1, naver_url_3, DATE1, naver_url_4, DATE2, naver_url_5, DATE2, naver_url_6, PAGE)

naver_url 

## 날짜 리스트 만들기 ----------------------------------------------------------
DATE_START <- as.Date(as.character(20100101), format = "%Y%m%d")
DATE_END <- as.Date(as.character(20210731), format = "%Y%m%d")
DATE <- DATE_START:DATE_END
DATE <- as.Date(DATE, origin = "1970-01-01") # origin은 임의의 값 
DATE

## 게시물 번호 -----------------------------------------------------------------
PAGE<- seq(from = 1, to = 11, by = 10) #최초 20개 기사까지 검색

## 네이버 검색결과 url 리스트 만들기 -------------------------------------------
naver_url_list <- c()
for (date_i in DATE){
  for (page_i in PAGE){
    dt1 <- format(as.Date(date_i, origin = "1970-01-01"), "%Y.%m.%d")
    dt2 <- format(as.Date(date_i, origin = "1970-01-01"), "%Y%m%d")
    naver_url <- paste0(naver_url_1, QUERY, naver_url_2, dt1, naver_url_3, dt1, naver_url_4, dt2, naver_url_5, dt2, naver_url_6, PAGE) 
    naver_url_list <- c(naver_url_list, naver_url)
  }
}
head(naver_url_list, 5)

## url 읽어오기 ----------------------------------------------------------------
news_url <- c()
news_date <- c()
for (date_i in DATE){
  for (page_i in PAGE){
    dt1 <- format(as.Date(date_i, origin = "1970-01-01"), "%Y.%m.%d")
    dt2 <- format(as.Date(date_i, origin = "1970-01-01"), "%Y%m%d")
    naver_url <- paste0(naver_url_1, QUERY, naver_url_2, dt1, naver_url_3, dt1, naver_url_4, dt2, naver_url_5, dt2, naver_url_6, page_i) 
    html <- read_html(naver_url)
    temp <- unique(html_nodes(html, '#main_pack') %>%
                     html_nodes('a.info') %>%
                     html_attr('href'))
    news_url <- c(news_url, temp)
    news_date <- c(news_date, rep(dt1, length(temp)))
  }
  print(dt1) # 생략 가능
}

## 불필요한 url 처리 -----------------------------------------------------------
NEWS0 <- as.data.frame(cbind(date=news_date, url=news_url, query=QUERY))
NEWS1 <- NEWS0[which(grepl("news.naver.com",NEWS0$url)),] # 네이버뉴스만 대상
NEWS1 <- NEWS1[which(!grepl("sports.news.naver.com",NEWS1$url)),] # 스포츠 제외
NEWS2 <- NEWS1[!duplicated(NEWS1), ] # 중복 링크 제거 

## 뉴스 기사 및 제목 추출 ------------------------------------------------------
NEWS2$news_title   <- ""
NEWS2$news_content <- ""

for (i in 1:dim(NEWS2)[1]){
  html <- read_html(as.character(NEWS2$url[i]))
  temp_news_title   <- repair_encoding(html_text(html_nodes(html,'#articleTitle')),from = 'utf-8')
  temp_news_content <- repair_encoding(html_text(html_nodes(html,'#articleBodyContents')),from = 'utf-8')
  if (length(temp_news_title)>0){
    NEWS2$news_title[i]   <- temp_news_title
    NEWS2$news_content[i] <- temp_news_content
  }
}

NEWS2$news_content <- gsub("// flash 오류를 우회하기 위한 함수 추가\nfunction _flash_removeCallback()", "", NEWS2$news_content) # 중복된 문자열 제거
NEWS <- NEWS2 # 최종 결과 저장 

write.csv(NEWS, "DA_news.csv")


#### 경향신문 ####
## 변수 입력하기(test) ---------------------------------------------------------
QUERY <- "쿠팡" # 검색키워드
DATE  <- as.Date(as.character(20100101),
                 format="%Y%m%d") # 검색시작날짜 & 검색종료날짜
DATE1 <- format(DATE, "%Y.%m.%d")
DATE2 <- format(DATE, "%Y%m%d")
PAGE <- 1

naver_url_1 <- "https://search.naver.com/search.naver?where=news&query="
naver_url_2 <- "&sm=tab_opt&sort=2&photo=0&field=0&pd=3&ds="
naver_url_3 <- "&de="
naver_url_4 <- "&docid=&related=0&mynews=1&office_type=1&office_section_code=2&news_office_checked=1032&nso=so%3Ar%2Cp%3Afrom" # 경향신문 코드 -> 1032
naver_url_5 <- "to"
naver_url_6 <- "&start="

naver_url <- paste0(naver_url_1, QUERY, naver_url_2, DATE1, naver_url_3, DATE1, naver_url_4, DATE2, naver_url_5, DATE2, naver_url_6, PAGE)

naver_url 

## 날짜 리스트 만들기 ----------------------------------------------------------
DATE_START <- as.Date(as.character(20100101), format = "%Y%m%d")
DATE_END <- as.Date(as.character(20210731), format = "%Y%m%d")
DATE <- DATE_START:DATE_END
DATE <- as.Date(DATE, origin = "1970-01-01") # origin은 임의의 값 
DATE

## 게시물 번호 -----------------------------------------------------------------
PAGE<- seq(from = 1, to = 21, by = 10) #최초 30개 기사까지 검색 (보통 1-2개)

## 네이버 검색결과 url 리스트 만들기 -------------------------------------------
naver_url_list <- c()
for (date_i in DATE){
  for (page_i in PAGE){
    dt1 <- format(as.Date(date_i, origin = "1970-01-01"), "%Y.%m.%d")
    dt2 <- format(as.Date(date_i, origin = "1970-01-01"), "%Y%m%d")
    naver_url <- paste0(naver_url_1, QUERY, naver_url_2, dt1, naver_url_3, dt1, naver_url_4, dt2, naver_url_5, dt2, naver_url_6, PAGE) 
    naver_url_list <- c(naver_url_list, naver_url)
  }
}
head(naver_url_list, 5)

## url 읽어오기 ----------------------------------------------------------------
news_url <- c()
news_date <- c()
for (date_i in DATE){
  for (page_i in PAGE){
    dt1 <- format(as.Date(date_i, origin = "1970-01-01"), "%Y.%m.%d")
    dt2 <- format(as.Date(date_i, origin = "1970-01-01"), "%Y%m%d")
    naver_url <- paste0(naver_url_1, QUERY, naver_url_2, dt1, naver_url_3, dt1, naver_url_4, dt2, naver_url_5, dt2, naver_url_6, page_i) 
    html <- read_html(naver_url)
    temp <- unique(html_nodes(html, '#main_pack') %>%
                     html_nodes('a.info') %>%
                     html_attr('href'))
    news_url <- c(news_url, temp)
    news_date <- c(news_date, rep(dt1, length(temp)))
  }
  print(dt1) # 생략 가능
}

## 불필요한 url 처리 -----------------------------------------------------------
NEWS0 <- as.data.frame(cbind(date=news_date, url=news_url, query=QUERY))
NEWS1 <- NEWS0[which(grepl("news.naver.com",NEWS0$url)),] # 네이버뉴스만 대상
NEWS1 <- NEWS1[which(!grepl("sports.news.naver.com",NEWS1$url)),] # 스포츠 제외
NEWS2 <- NEWS1[!duplicated(NEWS1), ] # 중복 링크 제거 

## 뉴스 기사 및 제목 추출 ------------------------------------------------------
NEWS2$news_title   <- ""
NEWS2$news_content <- ""

for (i in 1:dim(NEWS2)[1]){
  html <- read_html(as.character(NEWS2$url[i]))
  temp_news_title   <- repair_encoding(html_text(html_nodes(html,'#articleTitle')),from = 'utf-8')
  temp_news_content <- repair_encoding(html_text(html_nodes(html,'#articleBodyContents')),from = 'utf-8')
  if (length(temp_news_title)>0){
    NEWS2$news_title[i]   <- temp_news_title
    NEWS2$news_content[i] <- temp_news_content
  }
}

NEWS2$news_content <- gsub("// flash 오류를 우회하기 위한 함수 추가\nfunction _flash_removeCallback()", "", NEWS2$news_content) # 중복된 문자열 제거
NEWS <- NEWS2 # 최종 결과 저장 

write.csv(NEWS, "GH_news.csv")


## 데이터 불러오기 -----------------------------------------------------------
GH_news <- read.csv("GH_news.csv") %>%
  mutate(news = "경향신문")
YH_news <- read.csv("YH_news.csv") %>%
  mutate(news = "연합뉴스")
DA_news <- read.csv("DA_news.csv") %>%
  mutate(news = "동아일보")

## 데이터 합치기 -------------------------------------------------------------

coupang_news <- rbind(GH_news, YH_news, DA_news) %>%
  arrange(date) %>%
  select(date, news_title, url, news_content, news)

write.csv(coupang_news, "coupang_news.csv")
