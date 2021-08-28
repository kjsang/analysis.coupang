# analysis.coupang

This study focuses on the `investor-platform-consumer alliance` with `Flashpoint` as a factor that affects policy changes in regulations towards platform business. In order to analyze the regulatory response strategy of platform business and the policy change process of regulation toward platform business, this study conducts a text mining technique, `word appearance frequency`, `weighted log Odds ratio`, and a `topic modeling` analysis. The analysis confirmed that as the company grows with the support of patient capital, the alliance between platform business and consumers is strengthened to a certain level. However, the frequent advent of `Flashpoint` has since loosened the alliance with consumers and broken the `Permissive Consensus`. In addition, the specificity of the platform business regulation policy was considered as a national context, and the Korean context, which emphasized fairness and equality, limited the path of platform business regulation policy.

본 연구는 플랫폼 기업규제 정책변동에 영향을 주는 요인으로 `플래시포인트(Flashpoint)`와 투자자-플랫폼-소비자 동맹관계에 주목한다. 본 연구는 플랫폼 기업의 규제대응전략과 플랫폼 기업규제 변동과정을 분석하기 위해 텍스트마이닝 기법인 `단어출현빈도분석`, `가중 로그 승산비(Weighted Log Odds ratio) 분석`과 `토픽모델링(Topic Modeling)` 분석을 실시한다. 분석 결과 참을성 있는 자본의 지원을 받아 성장할수록 플랫폼기업과 소비자와의 동맹관계는 일정 수준까지 강화됨을 확인하였다. 하지만 이후 플래시포인트의 빈번한 출현에 따라 소비자와의 동맹관계는 느슨해지고 `관대한 합의(Permissive Consensus)`는 결렬되었다. 더불어 플랫폼 규제정책에는 국가적 맥락이라는 특수성이 고려되었다. 공정과 평등을 중시하는 한국적 맥락은 플랫폼 규제정책의 경로를 제약하였다.

주제어: 플랫폼 규제, 정책변동, 텍스트마이닝, 토픽모델링, 쿠팡


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


## 데이터 수집

## 데이터 전처리
