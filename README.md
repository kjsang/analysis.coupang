# Title
플랫폼 기업의 구조적 권력과 규제정정책변동: 쿠팡(Coupang) 사례를 중심으로   
Platform Business Regulatory Response Strategies and Policy Changes: Focusing on Coupang

## Abstract

### English
 This study focuses on the `investor-platform-consumer alliance` with `flashpoint` as a factor that affects policy changes in regulations towards platform business. In order to analyze the regulatory response strategy of platform business and the policy change process of regulation toward platform business, this study conducts a text mining technique, `word appearance frequency`, `weighted log Odds ratio`, and a `topic modeling` analysis. The analysis confirmed that as the business grows with the support of patient capital, the alliance between platform business and consumers is strengthened to a certain level. However, the frequent advent of `flashpoint` has since loosened the alliance with consumers and broken the `Permissive Consensus`. In addition, the specificity of the platform business regulation policy was considered as a national context, and the Korean context, which emphasized fairness and equality, limited the path of platform business regulation policy.


Keywords: platform business regulation, policy change, text mining, topic modeling, Coupang

### Korean
본 연구는 한국적 맥락에서 플랫폼 기업의 규제대응전략과 규제 변동과정에 미치는 요인을 탐구하는 단초가 되는 연구이다. 먼저 플랫폼 기업규제 정책변동에 영향을 주는 요인으로 Thelen이 제시한 다섯 가지 플래시포인트와 투자자-플랫폼-소비자 동맹관계에 주목하였다. 본 연구에서는 이를 확인하기 위해 국내 대표적인 플랫폼 기업인 쿠팡을 중심으로, 신문기사 자료를 사용하여 텍스트마이닝 기업인 단어빈도분석, 가중로그승산비(Weighted Log Odds ratio) 분석과 토픽모델링(Topic Modeling) 분석을 실시하였다. 연구결과 참을성 있는 자본의 지원을 받아 성장할수록 플랫폼기업과 소비자와의 동맹관계는 일정 수준까지 강화됨을 확인하였으나, 플래시포인트의 빈번한 출현은 소비자와의 동맹과 관대한 합의의 결렬을 이끌었고, 나아가 공정과 평등을 중시하는 한국적 맥락의 특수성을 고려하여 플랫폼 규제정책의 경로가 제약됨을 확인했다.본 연구는 플랫폼 기업 규제와 대응과정을 한국적 맥락에서 이론적으로 발전시켰다는 점에서 의의가 있다.

주제어: 플랫폼 규제, 정책변동, 텍스트마이닝, 토픽모델링, 쿠팡  


## Authors

김주상(Kim, Ju Sang) `First Co-Author`   
김하늘(Kim, Ha Neul) `First Co-Author`   
송가영(Song, Gayoung) `First Co-Author`   
오태성(Oh, Tae Sung) `First Co-Author`   
박아름(Park, Arum) `Corresponding Author`  


## Requirements
본 분석은 `MacOS Big Sur(11.5.1 version)`에서 `R(4.1.1 version)` 및 `Rstudio(1.4.1717 version)`으로 진행되었음을 알립니다.
Windows 환경의 경우 font 등의 문제로 코드상 문제가 발생할 수 있습니다. `MacOS` 환경에서 구동해주시면 감사하겠습니다.
분석결과에 대한 자세한 내용은 `논문 원본`을 참조해주세요.

### 크롤링 분석
크롤링 코드 실행을 위해 우선적으로 `analysis.coupang/crawling` 폴더의 `coopangnews.Rproj`를 실행한 후 `news_crawling.R` 을 실행해주세요.

### 본 분석
본 분석의 코드 실행을 위해 우선적으로 `analysis.coupang/coupang` 폴더의 `coupang.Rproj`를 실행한 후 `coupang.R` 을 실행해주세요.


### 패키지 로드

```{r}
# 패키지 로드 ----------------------------------------------

if (!require("pacman")) (install.packages("pacman"))
pacman::p_load(
  tidyverse, magrittr,
  readxl,
  tidytext,
  KoNLP, lubridate, tidylo, rvest,
  tm, furrr, topicmodels
)
```

> 분석에는 `tidyverse`를 포함한 11종의 패키지가 사용되었습니다.

### 패키지 인용정보 확인

```{r}
# 인용 (패키지 및 R 버전)
citation()
citation("tidyverse")
citation("readxl")
citation("lubridate")
citation("tidylo")
citation("tidytext")
citation("KoNLP")
citation("NIADic")
citation("rvest")
citation("topicmodels")
citation("tm")
citation("furrr")

# R Studio 버전 
RStudio.Version()
```

## 사전탑재

```{r}
# 사전 불러오기 ---------------------------------------------

# 사전 불러오기
useNIADic()

# 사용자 사전을 NIAdic과 통합
my_dic <- data.frame(
  c("쿠팡", "이커머스", "소셜네트워크서비스", "오프라인", "티켓몬스터",
  "위메프", "네이버", "카카오톡", "카카오페이", "네이버페이",
  "배민라이더스", "배달의민족", "배민커넥터", "쿠팡이츠", "우아한형제들",
  "로켓배송", "카카오쇼핑", "네이버쇼핑", "쓱배송", "이마트",
  "공정거래위원회", "공정위", "과학기술정보통신부", "과기부", "과기정통부",
  "유니콘기업", "스타트업", "정규직", "계약직", "특수고용",
  "그루폰", "소셜커머스", "배달어플리케이션", "코로나", "이베이",
  "이베이코리아", "산재보험", "고용보험", "국민연금", "4대보험",
  "건강보험", "소프트뱅크", "손정의", "김범석", "불공정거래"), 
  c("nqq", "nqq", "nqq",  "nqq", "nqq",
    "nqq", "nqq", "nqq",  "nqq", "nqq",
    "nqq", "nqq", "nqq",  "nqq", "nqq",
    "nqq", "nqq", "nqq",  "nqq", "nqq",
    "nqq", "nqq", "nqq",  "nqq", "nqq",
    "nqq", "nqq", "nqq",  "nqq", "nqq",
    "nqq", "nqq", "nqq",  "nqq", "nqq",
    "nqq", "nqq", "nqq",  "nqq", "nqq",
    "nqq", "nqq", "nqq",  "nqq", "nqq")
)
buildDictionary(ext_dic = "NIADic",
                user_dic = my_dic)

stopping_ko_end=regex("입니다$|이다$")
stopping_ko=tibble(단어=c('이','가','은','는'))
```
형태소사전은 `NIADic`을 사용하였습니다. 사용자 사전은 `my_dic`에 탑재하였으며, 불용어사전의 경우 전처리 코드를 통해 불용어를 처리했기에 초기 불용어사전은 기본적인 조사와 어미만 탑재하였습니다.

## 데이터 불러오기
```{r}
read_csv("coupang_news.csv") -> coupang
```

## 데이터 전처리
```{R}
coupang %>% 
  mutate(news_content = news_content %>% 
           
           ...
           
           str_replace_all("[가-힣]{2,4}\\s기자", "")) # 기자 이름 제거 
           ...

raw1 %>% 
  filter(news %in% c("경향신문", "연합뉴스", "동아일보")) %>% 
  mutate(news_title = news_title %>% 
           
           ...
           
           str_replace_all("\\(종합\\)|\\(종합2보\\)|\\(종합3보\\)", "") %>% 
           str_replace_all("\\s{2,10}", "") %>% 
           str_replace_all("\\[뉴스테이션\\]|\\[뉴스테이션\\/동아논평\\]", "") %>% 
           str_replace_all("\\[게시판\\]|\\<게시판\\>", "") %>% 
           str_replace_all("\\[뉴스룸\\/+[가-힣]{3}+\\]|\\[@뉴스룸\\/+[가-힣]{3}+\\]|\\[김현수의 뉴스룸\\]", "")) %>% 
           
           ...
           
  filter(!news_title %>% str_detect("기획전|출시|행사|할인")) %>% 
  select(date,  news, news_title, news_content, url) %>%  
  distinct(news_title, .keep_all = T)
```

데이터 전처리 과정에서 기사에 포함된 광고글과 신문사 이름, 머릿말 등을 제거하였습니다. 이후 기사제목을 기준으로 광고기사 및 연관성이 낮은 기사를 제거하였습니다. 그 결과 `7,356` 건의 기사 중 전처리를 통해 `4,145` 건의 기사를 추출하였고, 전처리 이후 내용분석을 통해 필터링 하여 총 `4,110` 건의 데이터 셋을 구축할 수 있었습니다.

## 형태소 분석
`KoNLP` 패키지 내장함수 `extractNoun`과 `tidytext` 내장함수 `unnest_tokens` 를 활용해 형태소분석을 실시, 형태소 분석 이후 2차 전처리 과정을 수행하였습니다.
형태소 분석 결과 `1,169,158` 건의 데이터를 추출하였고, 이 중 숫자, 한 글자 단어, 열 글자 초과 단어를 제거하여 `817,388` 건의 명사를 필터링, 이후 빈도 2 이하 단어를 분석에서 제외한 후 불용어 처리 및 관련단어 통합 등을 진행하였습니다. "쿠팡"이라는 키워드를 분석에서 제외한 후 출현 빈도 10 이하의 단어를 필터링 하였습니다. 결과적으로 `659,773` 건의 명사를 추출할 수 있었습니다.

### 전처리 과정 도식화
![전처리과정](./Diagram.jpg)

## 분석결과

### 출현빈도, tf_idf, weighted log odds ratio (1기 기준)

```{R}
# A tibble: 17,856 x 7
   시기  words      n      tf   idf  tf_idf log_odds
   <chr> <chr>  <int>   <dbl> <dbl>   <dbl>    <dbl>
 1 2기   확진자  2931 0.0112  0.405 0.00455   32.3  
 2 3기   기업    2444 0.00810 0     0          0.191
 3 3기   투자    2188 0.00725 0     0          3.63 
 4 3기   배달    2183 0.00724 0     0          8.99 
 5 3기   서비스  2080 0.00689 0     0         -1.63 
 6 2기   감염    2072 0.00793 0     0         11.8  
 7 2기   기업    1945 0.00745 0     0         -1.41 
 8 3기   배송    1831 0.00607 0     0         -4.04 
 9 2기   배송    1799 0.00689 0     0         -1.11 
10 3기   온라인  1717 0.00569 0     0         -2.27 
# … with 17,846 more rows
```

### LDA 분석 (토픽모델링, 1기 기준)

```{R}
Formal class 'LDA_VEM' [package "topicmodels"] with 14 slots
  ..@ alpha          : num 0.0326
  ..@ call           : language LDA(x = coupang_dtm_1기, k = 9, control = list(seed = 1234))
  ..@ Dim            : int [1:2] 672 5212
  ..@ control        :Formal class 'LDA_VEMcontrol' [package "topicmodels"] with 13 slots
  .. .. ..@ estimate.alpha: logi TRUE
  .. .. ..@ alpha         : num 5.56
  .. .. ..@ seed          : int 1234
  .. .. ..@ verbose       : int 0
  .. .. ..@ prefix        : chr "/var/folders/r_/93tf9z_547vf8m34lh14_vfw0000gn/T//RtmpiKAMuT/filedfa3f16921b"
  .. .. ..@ save          : int 0
  .. .. ..@ nstart        : int 1
  .. .. ..@ best          : logi TRUE
  .. .. ..@ keep          : int 0
  .. .. ..@ estimate.beta : logi TRUE
  .. .. ..@ var           :Formal class 'OPTcontrol' [package "topicmodels"] with 2 slots
  .. .. .. .. ..@ iter.max: int 500
  .. .. .. .. ..@ tol     : num 1e-06
  .. .. ..@ em            :Formal class 'OPTcontrol' [package "topicmodels"] with 2 slots
  .. .. .. .. ..@ iter.max: int 1000
  .. .. .. .. ..@ tol     : num 1e-04
  .. .. ..@ initialize    : chr "random"
  ..@ k              : int 9
  ..@ terms          : chr [1:5212] "광고" "택배" "고객" "기사" ...
  ..@ documents      : chr [1:672] "253" "481" "346" "277" ...
  ..@ beta           : num [1:9, 1:5212] -54.58 -6.23 -9.49 -5.47 -8.48 ...
  ..@ gamma          : num [1:672, 1:9] 8.27e-02 3.52e-05 3.30e-05 3.67e-02 3.79e-05 ...
  ..@ wordassignments:List of 5
  .. ..$ i   : int [1:58201] 1 1 1 1 1 1 1 1 1 1 ...
  .. ..$ j   : int [1:58201] 1 4 5 9 10 11 13 15 17 25 ...
  .. ..$ v   : num [1:58201] 9 9 5 5 9 9 9 9 9 9 ...
  .. ..$ nrow: int 672
  .. ..$ ncol: int 5212
  .. ..- attr(*, "class")= chr "simple_triplet_matrix"
  ..@ loglikelihood  : num [1:672] -5751 -5989 -6367 -4085 -6403 ...
  ..@ iter           : int 45
  ..@ logLiks        : num(0) 
  ..@ n              : int 95149
```

### 갑질, 불공정거래 단어빈도 추이

```{r}
data_word_prep2 %>% 
  filter(words %>% str_detect("갑질|불공정")) %>%
  mutate(words = ifelse(words %>%  str_detect("갑질"), "갑질", words)) %>% 
  count(분기, words) %>% 
  ggplot2::ggplot(aes(x = 분기, y = n)) +
  theme_minimal(base_family = "AppleGothic") +
  theme(legend.position = "none") +
  ylab("출현 빈도") +
  xlab("2010년 ~ 2021년 7월") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  geom_col() +
  geom_abline(slope = 1) +
  ggplot2::annotate("rect", xmin = 22, xmax = 28, ymin = 0, ymax = 50, alpha = .3, fill="#C89EC4") +
  ggplot2::annotate("text", x = 25, y = 45, size = 7, label = "3기", family = "AppleGothic")
```


## Results

논문 원본 참조



## 감사의 말

본 연구는 한국데이터산업진흥원 주관 2021 데이터 청년 캠퍼스 프로젝트를 기반으로 수행되었습니다.

논문 완성에 애정어린 소중한 조언 주신 경희대학교 이새봄 교수님과 한국외국어대학교 장현주 교수님, 그리고 경찰대학 정지수 교수님께 한없는 감사의 마음을 전합니다.

또한 텍스트 전처리 과정에서 아낌없는 조언을 해주신 SK Innovation의 [정병기 박사](https://byeongkijeong.github.io/aboutme/)님과 [김승욱 RLOHA 대표](https://github.com/encaion)님께 감사의 뜻을 전합니다. 
