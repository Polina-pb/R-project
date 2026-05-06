
# install.packages('readxl')
# install.packages('tidyverse')

library(readxl)
library(tidyverse)


# АКРА --------------------------------------------------------------------


AKRA_2023 <- read_excel('raw\\additional_raw\\AKRA_2023.xlsx')

#количество кредитных организаций-клиентов
AKRA_credit_org <- AKRA_2023 %>%
  mutate(`Объект рейтинга` = tolower(str_replace_all(`Объект рейтинга`, "\"|»|«|  ", ""))) %>%
  mutate(`Объект рейтинга` = str_replace_all(`Объект рейтинга`, "- ", "-")) %>%
  filter(!(str_detect(`Объект рейтинга`, 'облигации|ноты,'))) %>%
  distinct(`Объект рейтинга`) %>%
  filter(str_detect(`Объект рейтинга`, 'банк')) %>%
  pull(`Объект рейтинга`) %>%
  length()

#количество клиентов по кредитным рейтингам выпусков
AKRA_bonds <- AKRA_2023 %>%
  mutate(`Объект рейтинга` = tolower(str_replace_all(`Объект рейтинга`, "\"|»|«|  ", ""))) %>%
  mutate(`Объект рейтинга` = str_replace_all(`Объект рейтинга`, "- ", "-")) %>%
  filter(str_detect(`Объект рейтинга`, 'облигации|ноты,')) %>%
  separate_wider_delim(cols = `Объект рейтинга`, delim = ", ", names = c("Объект", "Эмитент", "Номер 1", "Номер 2", "Номер 3"), too_few = "align_start") %>%
  distinct(`Эмитент`) %>%
  pull(`Эмитент`) %>%
  length()


# НКР ---------------------------------------------------------------------


NKR_bonds <- 8
NKR_credit_org <- 29
