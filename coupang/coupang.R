pacman::p_load(
  tidyverse, readxl,
  magrittr, tidytext,
  KoNLP
)

useNIADic()
my_dic <- data.frame(
  c("100만원", "재난지원금", "재난기본소득", "긴급재난지원금", "코로나19", "코로나 19", "문재인"),
  c("nqq", "nqq", "nqq", "nqq", "nqq", "nqq", "nqpc")
)
buildDictionary(ext_dic = "NIADic",
                user_dic = my_dic)

stopping_ko_end=regex("입니다$|이다$")
stopping_ko=tibble(단어=c('이','가','은','는',
                          "정부", "국민", "우리", "이번", "논의", "억원", "오늘", "이후", "취지"))



read_csv("donga_v1.csv") -> raw
raw %>% 
  select(news_title, news_content) %>% 
  mutate(news_content = news_content %>% 
           str_replace_all("\\\n", "") %>% 
           str_replace_all("\\\t", "") %>% 
           str_replace_all("\\(|\\)|\\{|\\}", "") %>% 
           str_replace_all("\\[동아일보]", "") %>% 
           str_replace_all("\\[동아닷컴]", "")) -> raw1

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
  ggplot(aes(x = fct_reorder(word, n), y = n) +
  geom_col() +
  coord_flip()