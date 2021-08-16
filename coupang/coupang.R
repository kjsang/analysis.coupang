
# 패키지 로드 ----------------------------------------------

if (!require("pacman")) (install.packages("pacman"))
pacman::p_load(
  tidyverse, magrittr,
  readxl,
  tidytext,
  KoNLP, lubridate, tidylo
)

# 인용 (패키지 및 R 버전)
citation()
citation("tidyverse")
citation("readxl")
citation("lubridate")
citation("tidylo")
citation("tidytext")
citation("KoNLP")
citation("NIADic")

# R Studio 버전 
RStudio.Version()

# 폰트 설정 -----------------------------------------------



# 사전 불러오기 ---------------------------------------------

useNIADic()
my_dic <- data.frame(
  c("쿠팡", "이커머스", "소셜네트워크서비스"),
  c("nqq", "nqq", "nqq")
)
buildDictionary(ext_dic = "NIADic",
                user_dic = my_dic)

stopping_ko_end=regex("입니다$|이다$")
stopping_ko=tibble(단어=c('이','가','은','는'))



read_csv("coupang_news.csv") -> coupang
coupang %>% 
  select(date, news, news_title, news_content, url) %>% 
  mutate(news_content = news_content %>% 
           str_replace_all("\\\n", "") %>% 
           str_replace_all("\\\t", "") %>% 
           str_replace_all("\\(|\\)|\\{|\\}", "") %>% 
           str_replace_all("\\[동아일보]", "") %>% 
           str_replace_all("\\[동아닷컴]", "") %>% 
           str_replace_all("무단전재 및 재배포 금지", "") %>% 
           str_replace_all("최신 뉴스  ▶ 두고 두고 읽는 뉴스", "") %>% 
           str_replace_all("▶ 인기 무료만화©경향신문www.khan.co.kr,", "") %>% 
           str_replace_all("[\\‘\\,\\’\\“\\”\\ⓒ\\:]","") %>% 
           str_replace_all("SNS", "소셜네트워크서비스") %>% # 영문명칭 변경
           str_replace_all("〈경향닷컴은 한국온라인신문협회www.kona.or.kr의 디지털뉴스이용규칙에 따른 저작권을 행사합니다.〉","") %>% 
           str_replace_all("경향신문 & 경향닷컴www.khan.co.kr","") %>%
           str_replace_all("서울 연합뉴스", "") %>% 
           str_replace_all("서울 연합비즈뉴스", "") %>% 
           str_replace_all("연합뉴스", "") %>% 
           str_replace_all("관련기사", "") %>% 
           str_replace_all("[가-힣]{2,4}\\s기자", "") %>% # 기자 이름 제거
           str_replace_all("www.","") %>% # 웹사이트 제거.. 근데 사실 필요 없을지도...
           str_replace_all("[^가-힣0-9.]", " ") %>% # 한글, 숫자, 구두점 제외한 모든 문자를 제거
           str_replace_all("\\s{2,10}", "") %>% 
           str_replace_all("무단 전재 및 재배포 금지", "") %>% 
           str_replace_all("본 자료는 해당기관에서 제공한 보도 자료입니다", "")
         ) -> raw1
raw1 %>% 
  filter(news %in% c("경향신문", "연합뉴스", "동아일보")) %>% 
  mutate(news_title = news_title %>% 
           str_replace_all("\\(종합\\)|\\(종합2보\\)|\\(종합3보\\)", "") %>% 
           str_replace_all("\\s{2,10}", "") %>% 
           str_replace_all("\\[뉴스테이션\\]|\\[뉴스테이션\\/동아논평\\]", "") %>% 
           str_replace_all("\\[게시판\\]|\\<게시판\\>", "") %>% 
           str_replace_all("\\[뉴스룸\\/+[가-힣]{3}+\\]|\\[@뉴스룸\\/+[가-힣]{3}+\\]|\\[김현수의 뉴스룸\\]", "") %>% 
           str_replace_all("\\[+[a-zA-Z가-힣0-9-:\\/\\(\\)\\?\\!\\,·&+~‘’新④ ]{1,100}+\\]", "") %>% 
           str_replace_all("\\<+[a-zA-Z가-힣0-9-:\\/\\(\\)\\?\\!\\,·&+~‘’新④ …\\\"\\`']{1,100}+\\>", "") %>% 
           str_replace_all("①", "")) %>%
  filter(!news_title %>% str_detect("기획전|출시|행사|할인")) %>% 
  filter(!news_title %>% str_detect("최상급|키워드|이벤트|딜|론칭|런칭")) %>% 
  filter(!news_title %>% str_detect("캠페인|패션|사설|헤드라인|관심주|톱뉴스|조간|석간")) %>% 
  filter(!news_title %>% str_detect("선물세트|추석 선물|추석선물|설 선물|명절선물|어린이날 선물|연말 선물|성탄선물|가정의 달 선물")) %>% 
  filter(!news_title %>% str_detect("상품권|새상품|신상품|예약|품절|쇼핑정보|\\<표\\>")) %>% 
  filter(!news_title %>% str_detect("일본군|여행|검색어|경제계 인사|땅콩|소자본창업|뉴스")) %>%
  filter(!news_title %>% str_detect("김태희|데코|신제품|반려|갤럭시 폴드|비스포크|카톡|절반값|- 1|- 2|- 0|반값|사세요")) %>% 
  filter(!news_title %>% str_detect("전성시대|미얀마|곤충|열차|별세|부고")) %>% 
  filter(!news_title %>% str_detect("쿠폰차트|커피 쿠폰|소셜 쿠폰|미사용 쿠폰|쿠폰 서비스|커머스 쿠폰|쿠폰모아|커피|먹거리|맛집")) %>% 
  select(date,  news, news_title, news_content, url) %>%  
  distinct(news_title, .keep_all = T) -> data # 중복값 제거
  # select(date, news_title) %>% 
  # filter(news_title %>% str_detect(""))
  # write_excel_csv("coupang_news_rev.csv") # 7356 건의 기사 중 전처리를 통해 4145 건의 기사 데이터셋 생성
  # select(news_title)


readxl::read_xlsx("coupang_news_rev_filter.xlsx") -> data
data %>% 
  filter(!구분 == 1) %>% 
  # filter(주요사건 == 1) %>% 
  # write_excel_csv("event.csv")
  select(-주요사건, -구분) -> data
data

par(family = "AppleGothic")
theme_set(theme_gray(base_family = 'AppleGothic'))

data %>% 
  mutate(date = as_date(date),
         id = 1:length(news_content)) %>% 
  filter(!year(date) == 2010) %>% 
  select(id, date, news, news_title, news_content) %>% 
  mutate(분기 = paste0(as.character(quarter(date, with_year = T)), "분기")) %>% 
  mutate(시기 = ifelse(year(date) %in% c(2011:2015) | year(date) == 2016 & month(date) < 9, "1기", # 로켓배송 합법화 시기 기준으로 1기 설정
                     ifelse(year(date) %in% c(2016:2019) | year(date) == 2020 & month(date) <=6, "2기", "3기")) # 공정위에서 온라인 플랫폼법 제정 추진 발표 기점으로 2기, 그리고 그 이후부터 현재까지가 3기
  ) -> data_coupang
# data_coupang %>% write_excel_csv("coupang_news_rev.csv")
# 전처리 이후 수작업으로 데이터 선별, 4117 건의 기사 수집

# 시각화 준비 ----------------------------------------------

par(family = "AppleGothic")
theme_set(theme_gray(base_family = 'AppleGothic'))


# 시기별 기사 빈도 시각화 ---------------------------------------

data_coupang %>%
  as_tibble() %>% 
  count(시기) %>%
  ggplot(aes(x =  시기, y = n, fill =  시기)) +
  geom_col(fill = c("#67D5B5","#EE7785","#C89EC4")) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  ylab("기사빈도수") +
  ggtitle("시기별 기사 빈도수 (검색어: 쿠팡)") +
  geom_text(aes(label = n), vjust = -1) +
  ylim(0, 2200)

# 기사빈도 시각화 --------------------------------------------

data_coupang %>% 
  count(분기) %>% 
  ggplot(aes(x = 분기, y = n, fill = 분기)) +
  geom_col() +
  ylab("기사빈도수") +
  xlab("분기 (2011년 1분기 ~ 2021년 7월 기준)") +
  ggtitle("분기별 기사 빈도수 (검색어: 쿠팡)") +
  theme_minimal(base_family = "AppleGothic") +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  ylim(0, 850) +
  annotate("rect", xmin = 0, xmax = 23, ymin = 0, ymax = 130, alpha = .3, fill="#67D5B5") +
  annotate("rect", xmin = 23, xmax = 40, ymin = 0, ymax = 780, alpha = .3, fill="#EE7785") +
  annotate("rect", xmin = 40, xmax = 44, ymin = 0, ymax = 660, alpha = .3, fill="#C89EC4") +
  annotate("text", x = 12, y = 160, size = 5, label = "1기", family = "AppleGothic") +
  annotate("text", x = 32, y = 810, size = 5, label = "2기", family = "AppleGothic") +
  annotate("text", x = 42, y = 690, size = 5, label = "3기", family = "AppleGothic") +
  annotate("text", x = 22, y = 100, size = 3, label = "80", family = "AppleGothic") +
  annotate("text", x = 38, y = 750, size = 3, label = "730", family = "AppleGothic") +
  annotate("text", x = 42, y = 635, size = 3, label = "616", family = "AppleGothic")



data_coupang %>% 
  as_tibble() %>% 
  group_by(id) %>% 
  mutate(news_content = SimplePos09(news_content) %>% 
              unlist() %>% 
              paste(collapse = " ") %>% 
              str_extract_all(regex('[^\\s]+/N')) %>%
              paste(collapse = ' ') %>% 
              str_remove_all('/N') %>% 
              str_remove_all(stopping_ko_end)
  ) %>% 
  ungroup() %>%
  unnest_tokens(단어, news_content) %>% 
  anti_join(stopping_ko) %>% 
  filter(str_length(단어) > 1) -> data_word

data_word %>% 
  group_by(시기) %>% 
  count(단어) %>% 
  arrange(desc(n)) %>% 
  slice_max(n, n = 20, with_ties = F) %>% 
  ggplot(aes(x = fct_reorder(단어, n), y = n, fill = 시기)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~시기, drop = F, scales = "free_y") +
  ggtitle("시기별 단어 빈도분석")
