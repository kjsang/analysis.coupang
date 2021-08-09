pacman::p_load(
  tidyverse, readxl,
  magrittr, tidytext,
  KoNLP
)

read_csv("donga_v1.csv") -> raw
raw
