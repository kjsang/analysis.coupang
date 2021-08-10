
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


# 데이터 불러오기 --------------------------------------------
readxl::read_xlsx("기사.xlsx") -> keyword
readxl::read_xlsx("인용문.xlsx") -> char
keyword %<>%  rename(id = `뉴스 식별자`)
char %<>%  rename(id = `뉴스 식별자`)
char %<>%  select(id, 정보원, 인용문)
keyword %>% 
  left_join(char, by = "id") %>% 
  select(id, 일자, 언론사, 제목, 인물, 키워드, 정보원, 인용문) -> rawdata

read_csv("coupang_news.csv") -> coupang
coupang %>% 
  select(date, news, news_title, news_content)

# 사전 불러오기 ---------------------------------------------

useNIADic()
my_dic <- data.frame(
  c("쿠팡", "이커머스"),
  c("nqq", "nqq")
)
buildDictionary(ext_dic = "NIADic",
                user_dic = my_dic)

stopping_ko_end=regex("입니다$|이다$")
stopping_ko=tibble(단어=c('이','가','은','는'))

# 데이터 전처리 ---------------------------------------------

rawdata %<>% 
  filter(!정보원 %in% c("unknownactor", NA)) %>%
  select(일자, 정보원, 인용문) %>% 
  mutate(일자 = lubridate::as_date(일자)) %>% 
  distinct() # 중복값 제거

rawdata %>%
  mutate(
    일자 = lubridate::as_date(일자),
    정보원 = 정보원 %>%
      str_replace_all("e-commerce", "이커머스")
  ) -> predata_ver1

predata_ver1 %>% 
  mutate(
    정보원 = ifelse(정보원 %>%  str_detect("문재인"), "문재인대통령", 정보원),
  ) %>% 
  filter(!정보원 %>%  str_detect("김 씨")) -> predata_ver2


# 데이터 확인 ----------------------------------------------

predata_ver2 %>% 
  select(정보원) %>% 
  count(정보원) %>% 
  arrange(desc(n))

# 포함 인용문 확인
predata_ver2 %>% 
  filter(정보원 %>%  str_detect("쿠팡")) %>% 
  count(정보원) %>% 
  arrange(desc(n)) %>% 
  as.data.frame()

predata_ver2 %>% 
  filter(정보원 %>%  str_detect("제기"))

predata_ver2 %>% 
  select(정보원) %>% 
  count(정보원) %>% 
  arrange(desc(n)) %>% 
  filter(n >= 5) %>% 
  select(정보원) -> data_upper5

data_upper5 %>% 
  as.data.frame()

data_upper5 %>% 
  left_join(predata_ver2, by = "정보원") -> data_tb

par(family = "AppleGothic")
theme_set(theme_gray(base_family = 'AppleGothic'))

# 기사 수 추이 분석
data_tb %>% 
  arrange(일자) %>% 
  mutate(id = 1:length(일자)) %>% 
  count(일자) %>% 
  mutate(날짜 = paste0(year(일자),"년")) %>%
  ggplot(aes(x = 날짜, y = n)) +
  geom_col() 
  

data_tb %>%
  select(일자,  정보원,  인용문) %>%
  rename(일자 = 일자) %>% 
  mutate(시기 = ifelse(month(일자) == 3 & day(일자) < 18, "초기",
                     ifelse(month(일자) == 3 | month(일자) == 4 & day(일자) < 15, "중기_총선이전",
                            ifelse(month(일자) == 4 & day(일자) < 30, "중기_총선이후", "후기"))) %>% 
             as.factor()) %>% 
  mutate(id = 1:length(인용문)) %>% 
  select(id, 일자, 시기,  정보원,  인용문) %>% 
  mutate(인용문 = 인용문 %>% 
              str_replace_all("코로나19", "코로나") %>% 
              str_replace_all("100만원", "백만원") %>% 
              str_replace_all("50만원", "오십만원") %>% 
              str_replace_all("투자하", "투자") %>% 
              str_replace_all("포퓰리즘에", "포퓰리즘") %>% 
              str_remove_all("\\d+")
            ) -> data_tb