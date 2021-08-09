library(rvest)
library(dplyr)

## 변수 입력 -------------------------------------------------------------------
QUERY <- "쿠팡"
PAGE <- 1 

naver_url_1 <- "https://search.naver.com/search.naver?where=news&sm=tab_pge&query="
naver_url_2 <- "&sort=2&photo=0&field=0&pd=3&ds=2005.08.06&de=2021.08.06&mynews=1&office_type=1&office_section_code=1&news_office_checked=1020&nso=so:r,p:from20050806to20210806,a:all&start="

naver_url <- paste0(naver_url_1, QUERY, naver_url_2, PAGE)
PAGE <- seq(from = 1, to = 1361, by = 10)

## 네이버 검색결과 url 리스트 만들기 -------------------------------------------
naver_url_list <- c()
for(page_i in PAGE){
  naver_url <- paste0(naver_url_1, QUERY, naver_url_2, page_i)
  naver_url_list <- c(naver_url_list, naver_url)
}
head(naver_url_list, 5)

## 하이퍼링크 확인 -------------------------------------------------------------
naver_url <- "https://search.naver.com/search.naver?where=news&sm=tab_pge&query=쿠팡&sort=2&photo=0&field=0&pd=3&ds=2005.08.06&de=2021.08.06&mynews=1&office_type=1&office_section_code=1&news_office_checked=1020&nso=so:r,p:from20050806to20210806,a:all&start=1"
html <- read_html(naver_url)
temp <- unique(html_nodes(html, '#main_pack') %>%
                 html_nodes('a.info') %>%
                 html_attr('href'))
head(temp, 5)

## 네이버 관련 기사 리스트 만들기 ----------------------------------------------
news_url <- c()
news_date <- c()
for(page_i in PAGE) {
  naver_url <- paste0(naver_url_1, QUERY, naver_url_2, page_i)
  html <- read_html(naver_url)
  temp <- unique(html_nodes(html, '#main_pack') %>%
                    html_nodes('a.info') %>%
                    html_attr('href'))
  news_url <- c(news_url, temp)
}

NEWS0 <- as.data.frame(cbind(url=news_url, query = QUERY))
NEWS1 <- NEWS0[which(grepl("news.naver.com", NEWS0$url)),]
NEWS2 <- NEWS1[!duplicated(NEWS1), ]

## 뉴스 페이지에 있는 기사 제목 본문 크롤링 ------------------------------------
NEWS2$news_title <- ""
NEWS2$news_content <- ""
NEWS2$news_date <- "" 

for (i in 1:dim(NEWS2)[1]){
  html <- read_html(as.character(NEWS2$url[i]))
  temp_news_title   <- repair_encoding(html_text(html_nodes(html,'#articleTitle')),from = 'utf-8')
  temp_news_content <- repair_encoding(html_text(html_nodes(html,'#articleBodyContents')),from = 'utf-8')
  if (length(temp_news_title)>0){
    NEWS2$news_title[i]   <- temp_news_title
    NEWS2$news_content[i] <- temp_news_content
  }
}

# 날짜 시도 
for (i in 1:dim(NEWS2)[1]){
  html <- read_html(as.character(NEWS2$url[i]))
  temp_news_title   <- repair_encoding(html_text(html_nodes(html,'#articleTitle')),from = 'utf-8')
  temp_news_content <- repair_encoding(html_text(html_nodes(html,'#articleBodyContents')),from = 'utf-8')
  temp_date <- repair_encoding(html_text(html_nodes(html, '#main_content')%>% html_nodes(css = '.t11')                                          ))
  if (length(temp_news_title)>0){
    NEWS2$news_title[i]   <- temp_news_title
    NEWS2$news_content[i] <- temp_news_content
    NEWS2$news_date[i] <- temp_date
  }
}

NEWS2$news_content <- gsub("// flash 오류를 우회하기 위한 함수 추가\nfunction _flash_removeCallback()", "", NEWS2$news_content)
NEWSdonga <- NEWS2 
write.csv(NEWSdonga, "donga.csv")
