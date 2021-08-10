
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
  select(date, news, news_title, news_content) %>% 
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
           str_replace_all("본 자료는 해당기관에서 제공한 보도 자료입니다", "") %>% 
           str_replace_all("\\..", ".") %>% 
           str_replace_all("\\.", ". ") # 공백 2개 이상 제거
         ) -> raw1
raw1 %>% 
  filter(news == "동아일보") %>% 
  head(10) %>% 
  as.data.frame()

raw1 %>% 
  mutate(id = 1:length(news_content)) %>% 
  group_by(id) %>% 
  mutate(word = SimplePos09(news_content) %>% 
              unlist() %>% 
              paste(collapse = " ") %>% 
              str_extract_all(regex('[^\\s]+/N')) %>%
              paste(collapse = ' ') %>% 
              str_remove_all('/N') %>% 
              str_remove_all(stopping_ko_end)
  ) %>% 
  ungroup() %>%
  unnest_tokens(word, news_content) %>% 
  anti_join(stopping_ko) %>% 
  filter(str_length(word) > 1) -> data_tb


data_tb %>% 
  count(word) %>% 
  arrange(desc(n)) %>% 
  slice_max(n, n = 10, with_ties = F) %>% 
  ggplot(aes(x = fct_reorder(word, n), y = n)) +
  geom_col() +
  coord_flip()
