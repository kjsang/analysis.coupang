
# 패키지 로드 ----------------------------------------------

if (!require("pacman")) (install.packages("pacman"))
pacman::p_load(
  tidyverse, magrittr,
  readxl,
  tidytext,
  KoNLP, lubridate, tidylo, rvest,
  tm
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
citation("RSelenium")
citation("rvest")

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
data_coupang %>% write_excel_csv("coupang_news_4107.csv")
# 전처리 이후 수작업으로 데이터 선별, 4117 건의 기사 수집
# 2011년 1월 1일부터 2021년 7월 31일까지 데이터 4107 건

read_csv("coupang_news_4107.csv") -> data_coupang
data_coupang %>% 
  mutate(경쟁상황 = 0,
           고용및노동이슈 = 0,
           사회보장정책 = 0,
           조세정책 = 0,
           소비자안전 = 0,
           소비자동맹 = 0,
           투자자동맹 = 0,
           기존규제 = 0,
           정치적동원 = 0,
           코로나 = 0,
           갑질상황 = 0) %>% 
  write_excel_csv("coupang_news_4107_rev.csv")


data_coupang
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


# 명사 토크나이징 작업 
data_coupang %>% 
  as_tibble() %>% 
  unnest_tokens(input = news_content,
                output = words,
                token = extractNoun,
                drop = F) %>% 
  select(-news_content) -> data_word # 토크나이징 추출

data_word %>% write_excel_csv("coupang_word.csv") # 중간 저장
read_csv("coupang_word.csv") -> data_word 


# 형태소 분석 후 전처리 ----------------------------------------

data_word %>% # 1,176,189 데이터 추출
  filter(!words %>% str_detect("[:digit:]")) %>%  # 숫자 제거 1,077,777
  filter(words %>% str_length() > 1) %>% # 한 글자 단어 제거
  filter(words %>% str_length() <= 10) %>% # 열 글자를 초과하는 단어 제거
  mutate(words = words %>% 
           str_replace_all("\\.", "")
  ) -> data_word_prep

# 빈도가 2 이하인 단어는 추출하여 제외
data_word_prep %>% 
  count(words) %>% 
  filter(!n >= 3) -> anti_word
data_word_prep %<>% anti_join(anti_word) # anti_join()으로 제외 (788,068 건)

data_word_prep %>% 
  filter(!words %>% str_detect("경향|경향신문|연합인포맥스|동아일보|동아|경향|신문|뉴스|제보하기")) %>% 
  filter(!words %>% str_detect("지난해|이상|기준|올해|때문|경우|바탕|보기|소개|기획|선별|처리|반면|단계|오늘|여부|방침|선정|아이|사이|대비|전체|포함|생각|거리|누적|지난해|하기|들이|서울|경기")) %>% 
  filter(!words %>% str_detect("   ")) %>% 
  mutate(
         words = ifelse(words %>%  str_detect("네이버"), "네이버", words),
         words = ifelse(words %>%  str_detect("카카오"), "카카오", words),
         words = ifelse(words %>%  str_detect("번가"), "11번가", words),
         words = ifelse(words %>%  str_detect("코로나"), "코로나바이러스", words),
         words = ifelse(words %>%  str_detect("확진자"), "확진자", words),
         words = ifelse(words %>%  str_detect("백신"), "백신", words),
         words = ifelse(words %>%  str_detect("산업재해보험"), "산재보험", words),
         words = ifelse(words %>%  str_detect("노동자"), "노동자", words),
         words = ifelse(words %>%  str_detect("노조"), "노동조합", words),
         words = ifelse(words %>%  str_detect("택배(업|사|업계)|한진|롯데택배|로젠택배|[가-힣]{2}택배"), "택배업계", words),
         words = ifelse(words %>%  str_detect("플랫폼"), "플랫폼", words),
         words = ifelse(words %>%  str_detect("불공정"), "불공정거래", words),
         words = ifelse(words %>%  str_detect("경쟁사|경쟁업체|경쟁자|경쟁회사|경쟁상대"), "경쟁사", words),
         words = ifelse(words %>%  str_detect("보건당국|보건기구|보건복지부"), "보건당국", words),
         words = ifelse(words %>%  str_detect("보건교육|보건의료|공중보건|안전보건"), "보건", words),
         words = ifelse(words %>%  str_detect("덕평물류센터"), "쿠팡덕평물류센터", words),
         words = ifelse(words %>%  str_detect("부천물류센터"), "쿠팡부천물류센터", words),
         words = ifelse(words %>%  str_detect("쿠팡+물류센터"), "쿠팡물류센터", words),
         
         words = ifelse(words %>%  str_detect("배달의민족|배민"), "배달의민족", words),
         words = ifelse(words %>%  str_detect("쿠팡맨"), "쿠팡맨", words),
         words = ifelse(words %>%  str_detect("쿠친"), "쿠친", words),
         words = ifelse(words %>%  str_detect("배달앱"), "배달어플리케이션", words),
         words = ifelse(words %>%  str_detect("라이더유니온"), "라이더유니온", words),
         words = ifelse(words %>%  str_detect("공정위|공정거래위"), "공정거래위원회", words),
         words = ifelse(words %>%  str_detect("쿠팡이츠"), "쿠팡이츠", words),
         words = ifelse(words %>%  str_detect("쿠팡이츠"), "쿠팡이츠", words),
         words = ifelse(words %>%  str_detect("쿠팡플렉스"), "쿠팡플렉스", words),
         words = ifelse(words %>%  str_detect("쿠팡(은|는|이|가)$"), "쿠팡", words),
         words = ifelse(words %>%  str_detect("티몬|티켓몬스터"), "티켓몬스터", words),
         words = ifelse(words %>%  str_detect("페이스북"), "페이스북", words),
         words = ifelse(words %>%  str_detect("티몬|티켓몬스터"), "티켓몬스터", words),
         words = ifelse(words %>%  str_detect("그루폰"), "그루폰코리아", words),
         words = ifelse(words %>%  str_detect("위메이크프라이스|위메프|위메이크"), "위메프", words),
         ) %>% 
  filter(!words %>% str_detect("쿠팡")) -> data_word_prep2
data_word_prep2 %>% 
  filter(words %>% str_detect("위메이크프라이스|위메프|위메이크")) %>% 
  count(words) %>% arrange(desc(n)) %>% as.data.frame()
# 단어빈도표 도출 --------------------------------------------

data_word_prep2 %>% 
  filter(시기 == "1기") %>% 
  count(words) %>% 
  slice_max(n, n = 50,  with_ties = F) %>% 
  ggplot(aes(x = fct_reorder(words, n), y = n)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = n), hjust = 1) +
  xlab("단어") + ylab("빈도") +
  ggtitle("시기별 출현단어 빈도분석: 1기")

data_word_prep2 %>% 
  filter(시기 == "2기") %>% 
  count(words) %>% 
  slice_max(n, n = 50,  with_ties = F) %>% 
  ggplot(aes(x = fct_reorder(words, n), y = n)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = n), hjust = 1) +
  xlab("단어") + ylab("빈도") +
  ggtitle("시기별 출현단어 빈도분석: 2기")

data_word_prep2 %>% 
  filter(시기 == "3기") %>% 
  count(words) %>% 
  slice_max(n, n = 50,  with_ties = F) %>% 
  ggplot(aes(x = fct_reorder(words, n), y = n)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = n), hjust = 1) +
  xlab("단어") + ylab("빈도") +
  ggtitle("시기별 출현단어 빈도분석: 3기")

figure_1_1
figure_1_2
figure_1_3


# TF-IDF 값 --------------------------------------------

# tf_idf 값 뽑기
data_word_prep2 %>% 
  count(시기, words, sort = T) %>% 
  bind_tf_idf(words, 시기, n) -> coupang_tf_idf
coupang_tf_idf

# tf_idf log_odds_weighted 
coupang_tf_idf %>% 
  bind_log_odds(set = 시기, 
                feature = words, 
                n = n) %>%
  rename(log_odds = "log_odds_weighted") -> coupang_tf_idf

# 단어 중심으로 통합: 1기
coupang_tf_idf %>%
  filter(시기  == "1기") %>%
  group_by(words) %>%
  summarise(
    n = sum(n, na.rm = TRUE),
    tf_idf = sum(tf_idf, na.rm = TRUE),
    log_odds = sum(log_odds, na.rm = TRUE)
  ) %>%
  arrange(desc(n)) %>%
  ungroup() -> coupang_count_1기

coupang_count_1기 %>% # 단순빈도분석
  mutate(words = reorder(words, n)) %>%
  slice_max(n, n = 50,  with_ties = F) %>% 
  ggplot(aes(x = fct_reorder(words, n), y = n)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = n), hjust = 1) +
  xlab("단어") + ylab("빈도") +
  ggtitle("시기별 출현단어 빈도분석: 1기")

coupang_count_1기 %>% # tf-idf 값 상위 50
  mutate(word = reorder(words, tf_idf)) %>%
  slice_max(tf_idf, n = 50, with_ties = F) %>%
  ggplot(aes(x=tf_idf, y=fct_reorder(words, tf_idf))) +
  geom_col() +
  geom_text(aes(label = round(tf_idf, digit = 5)), hjust = 1) +
  xlab("단어") + ylab("빈도") +
  ggtitle("시기별 출현단어 빈도분석: 1기")

coupang_count_1기 %>%
  slice_max(log_odds, n = 50, with_ties = F) %>%
  ggplot(mapping = aes(x=log_odds, 
                       y=fct_reorder(words, log_odds))) +
  geom_col() +
  geom_text(aes(label = round(log_odds, digit = 5)), hjust = 1) +
  xlab("단어") + ylab("빈도") +
  ggtitle("시기별 출현단어 빈도분석: 1기")

# 단어 중심으로 통합: 2기
coupang_tf_idf %>%
  filter(시기  == "2기") %>%
  group_by(words) %>%
  summarise(
    n = sum(n, na.rm = TRUE),
    tf_idf = sum(tf_idf, na.rm = TRUE),
    log_odds = sum(log_odds, na.rm = TRUE)
  ) %>%
  arrange(desc(n)) %>%
  ungroup() -> coupang_count_2기

coupang_count_2기 %>% # 단순빈도분석
  mutate(words = reorder(words, n)) %>%
  slice_max(n, n = 50,  with_ties = F) %>% 
  ggplot(aes(x = fct_reorder(words, n), y = n)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = n), hjust = 1) +
  xlab("단어") + ylab("빈도") +
  ggtitle("시기별 출현단어 빈도분석: 2기")

coupang_count_2기 %>% # tf-idf 값 상위 50
  mutate(word = reorder(words, tf_idf)) %>%
  slice_max(tf_idf, n = 50, with_ties = F) %>%
  ggplot(aes(x=tf_idf, y=fct_reorder(words, tf_idf))) +
  geom_col() +
  geom_text(aes(label = tf_idf), hjust = 1) +
  xlab("단어") + ylab("빈도") +
  ggtitle("시기별 출현단어 빈도분석: 2기")

coupang_count_2기 %>%
  slice_max(log_odds, n = 50, with_ties = F) %>%
  ggplot(mapping = aes(x=log_odds, 
                       y=fct_reorder(words, log_odds))) +
  geom_col() +
  geom_text(aes(label = round(log_odds, digit = 5)), hjust = 1) +
  xlab("단어") + ylab("빈도") +
  ggtitle("시기별 출현단어 빈도분석: 2기")


# 단어 중심으로 통합: 3기
coupang_tf_idf %>%
  filter(시기  == "3기") %>%
  group_by(words) %>%
  summarise(
    n = sum(n, na.rm = TRUE),
    tf_idf = sum(tf_idf, na.rm = TRUE),
    log_odds = sum(log_odds, na.rm = TRUE)
  ) %>%
  arrange(desc(n)) %>%
  ungroup() -> coupang_count_3기

coupang_count_3기 %>% # 단순빈도분석
  mutate(words = reorder(words, n)) %>%
  slice_max(n, n = 50,  with_ties = F) %>% 
  ggplot(aes(x = fct_reorder(words, n), y = n)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = n), hjust = 1) +
  xlab("단어") + ylab("빈도") +
  ggtitle("시기별 출현단어 빈도분석: 3기")

coupang_count_3기 %>% # tf-idf 값 상위 50
  mutate(word = reorder(words, tf_idf)) %>%
  slice_max(tf_idf, n = 50, with_ties = F) %>%
  ggplot(aes(x=tf_idf, y=fct_reorder(words, tf_idf))) +
  geom_col() +
  geom_text(aes(label = tf_idf), hjust = 1) +
  xlab("단어") + ylab("빈도") +
  ggtitle("시기별 출현단어 빈도분석: 3기")

coupang_count_3기 %>%
  slice_max(log_odds, n = 50, with_ties = F) %>%
  ggplot(mapping = aes(x=log_odds, 
                       y=fct_reorder(words, log_odds))) +
  geom_col() +
  geom_text(aes(label = round(log_odds, digit = 5)), hjust = 1) +
  xlab("단어") + ylab("빈도") +
  ggtitle("시기별 출현단어 빈도분석: 3기")

# 단어문서행렬만들기 
coupang_tf_idf %>% 
  cast_dtm(document = 시기,
           term = words,
           value = n) -> coupang_dtm
tm::inspect(coupang_dtm)
coupang_tf_idf %>%
  cast_dtm(document =  시기,
           term = words,
           value = tf_idf) -> coupang_dtm_tf_idf
tm::inspect(coupang_dtm_tf_idf)