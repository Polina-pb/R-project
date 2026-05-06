
#Динамика количества клиентов кредитного рейтингового агентства
#ООО "НРА" за 2020-2025 гг. в разрезе видов объекта рейтинга

# Загрузка библиотек ------------------------------------------------------

# install.packages('readxl') #для чтения файлов Excel
# install.packages('writexl') #для создания файлов Excel
# install.packages('tidyverse') #для работы с таблицами и графиками
# install.packages('png') #для создания и чтения изображений
# install.packages('ggimage') #для добавления изображений в графики

library(readxl)
library(writexl)
library(tidyverse)
library(png)
library(ggimage)


# Функции -----------------------------------------------------------------


`%notin%` <- function(x, y) !(x %in% y)


# Импорты -----------------------------------------------------------------

#---для подсчета клиентов НРА---
NRA2020 <- read_excel('raw\\NRA2020_y.xlsx')
NRA2021 <- read_excel('raw\\NRA2021_y.xlsx')
NRA2022 <- read_excel('raw\\NRA2022_y.xlsx')
NRA2023 <- read_excel('raw\\NRA2023_y.xlsx')
NRA2024 <- read_excel('raw\\NRA2024_y.xlsx')
NRA2025 <- read_excel('raw\\NRA2025_y.xlsx')

#---для проверки подсчетов клиентов НРА---
current_ratings <- read_excel('raw\\current-ratings.xlsx')

#---для подсчета клиентов Эксперт РА (для сравнения)---
ExpertRA_2023 <- read_excel('raw\\ExpertRA_2023.xlsx')


# 1. СБОР ДАННЫХ ----------------------------------------------------------


# 1.1. Предварительная обработка -----------------------------------------------


list_of_tibbles <- list(NRA2020, NRA2021, NRA2022, NRA2023, NRA2024, NRA2025)

names(list_of_tibbles) <- c('NRA2020', 'NRA2021', 'NRA2022', 'NRA2023', 'NRA2024', 'NRA2025')

#убираем кавычки и приводим к нижнему регистру
clean_list <- map(list_of_tibbles, ~ mutate(.x, `Наименование рейтингуемого лица` = tolower(str_replace_all(`Наименование рейтингуемого лица`, "\"|»|«", ""))))

list2env(clean_list, envir = .GlobalEnv)


# 1.2. Объединение таблиц ------------------------------------------------------


#---2020-2021---

combined_tibbles <- bind_rows(NRA2020, NRA2021)

combined_tibbles_clean <- combined_tibbles %>%
  distinct(`Наименование рейтингуемого лица`, .keep_all = TRUE)


#---2022---

dup_clients_2020_2022 <- intersect(NRA2020$`ИНН`, NRA2022$`ИНН`)
dup_clients_2021_2022 <- intersect(NRA2021$`ИНН`, NRA2022$`ИНН`)

dup_2022 <- c(dup_clients_2021_2022, dup_clients_2020_2022)

NRA2022_new_clients <- NRA2022 %>%
  filter(`ИНН` %notin% unique(dup_2022))

combined_tibbles_2022 <- bind_rows(combined_tibbles_clean, NRA2022_new_clients)

#---2023---

dup_clients_2020_2023 <- intersect(NRA2020$`ИНН`, NRA2023$`ИНН`)
dup_clients_2021_2023 <- intersect(NRA2021$`ИНН`, NRA2023$`ИНН`)
dup_clients_2022_2023 <- intersect(NRA2022$`ИНН`, NRA2023$`ИНН`)

dup_2023 <- c(dup_clients_2020_2023, dup_clients_2021_2023, dup_clients_2022_2023)

NRA2023_new_clients <- NRA2023 %>%
  filter(`ИНН` %notin% unique(dup_2023))

combined_tibbles_2023 <- bind_rows(combined_tibbles_2022, NRA2023_new_clients)

#---2024---

dup_clients_all_2024 <- intersect(combined_tibbles_2023$ИНН, NRA2024$ИНН)

NRA2024_new_clients <- NRA2024 %>%
  filter(`ИНН` %notin% unique(dup_clients_all_2024))

combined_tibbles_2024 <- bind_rows(combined_tibbles_2023, NRA2024_new_clients)


#---2025---

dup_clients_all_2025 <- intersect(combined_tibbles_2024$ИНН, NRA2025$ИНН)

NRA2025_new_clients <- NRA2025 %>%
  filter(`ИНН` %notin% unique(dup_clients_all_2025))

combined_tibbles_2025 <- bind_rows(combined_tibbles_2024, NRA2025_new_clients)


# 1.3. Проверка данных ---------------------------------------------------------

#--нет повторов---
NRA2022_new_clients %>%
  count(`ИНН`) %>%
  arrange(desc(n)) %>%
  View()

NRA2023_new_clients %>%
  count(`ИНН`) %>%
  arrange(desc(n)) %>%
  View()

NRA2024_new_clients %>%
  drop_na(`Наименование рейтингуемого лица`) %>%
  count(`ИНН`) %>%
  arrange(desc(n)) %>%
  View()

NRA2025_new_clients %>%
  count(`ИНН`) %>%
  arrange(desc(n)) %>%
  View()

#--сверка количества новых клиентов с другой таблицей--

length(NRA2022_new_clients$`ИНН`)
current_ratings %>%
  filter(str_detect(`Дата опубликования пресс-релиза`, '2022')) %>%
  filter(str_detect(`Вид рейтинга`, 'Кредитный рейтинг')) %>%
  filter(str_detect(`Статус рейтинга`, 'Присвоен')) %>%
  pull(`ИНН`) %>%
  length()

#рейтинги разных выпусков, но одного эмитента, учитываем как
#1 клиента по выпуску как объекту рейтинга, поэтому разница в 1
length(NRA2023_new_clients$`ИНН`)
current_ratings %>%
  filter(str_detect(`Дата опубликования пресс-релиза`, '2023')) %>%
  filter(str_detect(`Вид рейтинга`, 'Кредитный рейтинг')) %>%
  filter(str_detect(`Статус рейтинга`, 'Присвоен')) %>%
  pull(`ИНН`) %>%
  length()
  # %>% setdiff(NRA2023_new_clients$`ИНН`)

#разницы нет, потому что 2 NA в NRA2024_new_clients$`ИНН`,
#а в current_ratings - 2 выпуска клиента из 2023 (не считаются в NRA2024_new_clients)

length(NRA2024_new_clients$`ИНН`)
to_compare <- current_ratings %>%
  filter(str_detect(`Дата опубликования пресс-релиза`, '2024')) %>%
  filter(str_detect(`Вид рейтинга`, 'Кредитный рейтинг')) %>%
  filter(str_detect(`Статус рейтинга`, 'Присвоен')) %>%
  pull(`ИНН`) %>%
  length()
  # setdiff(NRA2024_new_clients$`ИНН`)
to_compare[to_compare %notin% NRA2024_new_clients$`ИНН`]
NRA2024_new_clients$`ИНН`[NRA2024_new_clients$`ИНН` %in% to_compare]

#корректировка на 7725594308 будет произведена при подсчете cum
#(у компании был отозван рейтинг в 2024, но в 2025 она вернулась)
# 1402047184 - рейтинги 2 выпусков 1 клиента считаем как 1
#поэтому на 2 меньше
length(NRA2025_new_clients$`ИНН`)
current_ratings %>%
  filter(str_detect(`Дата опубликования пресс-релиза`, '2025')) %>%
  filter(str_detect(`Вид рейтинга`, 'Кредитный рейтинг')) %>%
  filter(str_detect(`Статус рейтинга`, 'Присвоен')) %>%
  pull(`ИНН`) %>%
  length()
  # setdiff(NRA2025_new_clients$`ИНН`)


# 1.4. Подсчет новых клиентов по видам объектов рейтинга --------------------------------------------------------------------

#---таблица с наименованиями новых клиентов---

#трансформируем полную дату публикации в индекс месяца
#и добавляем для цифр до 10 в начале 0
combined_tibbles_2025$`Дата публикации` <- sprintf("%02d", month(ymd(combined_tibbles_2025$`Дата публикации`)))

#добавляем колонку с видом компании
#информацию берем из колонки "Вида рейтинга"
combined_tibbles_2025$`Вид компании` <- case_when(
  str_detect(combined_tibbles_2025$`Вид рейтинга`, 'страховым|страховых') ~ 'страховая компания',
  str_detect(combined_tibbles_2025$`Вид рейтинга`, 'нефинансовым|нефинансовых') ~ 'нефинансовая компания',
  str_detect(combined_tibbles_2025$`Вид рейтинга`, 'инвестиционно-финансовым|инвестиционно-финансовых') ~ 'инвестиционная компания',
  str_detect(combined_tibbles_2025$`Вид рейтинга`, 'лизинговым|лизинговых') ~ 'лизинговая компания',
  str_detect(combined_tibbles_2025$`Вид рейтинга`, 'кредитным|кредитных') ~ 'кредитная компания',
  str_detect(combined_tibbles_2025$`Вид рейтинга`, 'выпускам|выпусков') ~ 'рейтинг выпуска',
)

combines_tibbles_selected <- combined_tibbles_2025 %>%
  select(!c(`Вид рейтинга`, `Запрошенный (незапрошенный)`, `№`)) %>%
  drop_na(`Наименование рейтингуемого лица`)

# write_xlsx(combines_tibbles_selected, "tibbles\\tibble_all_2020_2025_with_names_.xlsx")


#---таблица с количеством и видом компаний - новых клиентов---

count_combined_tibbles <- combines_tibbles_selected %>%
  group_by(`Год`, `Дата публикации`) %>%
  count(`Вид компании`) %>%
  pivot_wider(names_from = `Вид компании`, values_from = n, values_fill = 0)

months_numbers <- sprintf("%02d", c(1:12))

#новые компании приходили не в каждом месяце
#но строки с "пустыми" месяцами тоже хотелось бы видеть в таблице
#поэтому добавляем их
tibble_joining <- tibble(
  `Дата публикации` = c(rep(months_numbers, 6)),
  `Год` = c(rep(c(2020, 2021, 2022, 2023, 2024, 2025), c(12, 12, 12, 12, 12, 12)))
)

tibble_new_companies <- tibble_joining %>%
  full_join(count_combined_tibbles) %>%
  mutate(across(everything(), ~replace_na(., 0))) %>%
  mutate(`всего новых компаний` = rowSums(across(where(is.integer))))

# write_xlsx(tibble_new_companies, "tibbles\\tibble_new_companies_2020-2025_without_names_.xlsx")


# 1.5. Подсчет общего количества клиентов по видам объектов рейтинга --------


for_cum_2020_2021 <- tibble_new_companies %>%
  filter(`Год` %in% 2020:2021)

for_cum_2022 <- tibble_new_companies %>%
  filter(`Год` == 2022)

for_cum_2023 <-  tibble_new_companies %>%
  filter(`Год` == 2023)

for_cum_2024 <-  tibble_new_companies %>%
  filter(`Год` == 2024)

for_cum_2025 <-  tibble_new_companies %>%
  filter(`Год` == 2025)

#считаем кумулятивную сумму по столбцам в разрезе видов компаний
#далее вносим корректировки исходя из случаев отзыва рейтингов
cum_2020_2021 <- for_cum_2020_2021 %>%
  mutate(across(`страховая компания`:`рейтинг выпуска`, cumsum, .names = "{.col} (всего клиентов)")) %>%
  mutate(`страховая компания (всего клиентов)` = case_when(
    (`Год` == 2021) & (`Дата публикации` %notin% months_numbers[1:4]) ~ `страховая компания (всего клиентов)`-1,
    .default = `страховая компания (всего клиентов)`)) %>%
  mutate(`нефинансовая компания (всего клиентов)` = case_when(
    (`Год` == 2021) & (`Дата публикации` == months_numbers[12]) ~ `нефинансовая компания (всего клиентов)`-1,
    .default = `нефинансовая компания (всего клиентов)`)) %>%
  mutate(`Всего компаний-клиентов` = rowSums(across(`страховая компания (всего клиентов)`:`рейтинг выпуска (всего клиентов)`)))

cum_2022 <- for_cum_2022 %>%
  mutate(across(`страховая компания`:`рейтинг выпуска`, cumsum, .names = "{.col} (всего клиентов)")) %>%
  mutate(`страховая компания (всего клиентов)` = `страховая компания (всего клиентов)`+8) %>%
  mutate(`нефинансовая компания (всего клиентов)` = `нефинансовая компания (всего клиентов)`+15) %>%
  mutate(`инвестиционная компания (всего клиентов)` = `инвестиционная компания (всего клиентов)`+1) %>%
  mutate(`лизинговая компания (всего клиентов)` = `лизинговая компания (всего клиентов)`+1) %>%
  mutate(`кредитная компания (всего клиентов)` = `кредитная компания (всего клиентов)`+7) %>%
  mutate(`рейтинг выпуска (всего клиентов)` = `рейтинг выпуска (всего клиентов)`+1) %>%
  mutate(`нефинансовая компания (всего клиентов)` = case_when(
    (`Дата публикации` == months_numbers[8]) ~ `нефинансовая компания (всего клиентов)`-1,
    (`Дата публикации` %in% months_numbers[9:12]) ~ `нефинансовая компания (всего клиентов)`-2,
    .default = `нефинансовая компания (всего клиентов)`)) %>%
  mutate(`инвестиционная компания (всего клиентов)` = case_when(
    (`Дата публикации` %in% months_numbers[7:12]) ~ `инвестиционная компания (всего клиентов)`-1,
    .default = `инвестиционная компания (всего клиентов)`)) %>%
  mutate(`страховая компания (всего клиентов)` = case_when(
    (`Дата публикации` == months_numbers[12]) ~ `страховая компания (всего клиентов)`-1,
    .default = `страховая компания (всего клиентов)`)) %>%
  mutate(`Всего компаний-клиентов` = rowSums(across(`страховая компания (всего клиентов)`:`рейтинг выпуска (всего клиентов)`)))

cum_2023 <- for_cum_2023 %>%
  mutate(across(`страховая компания`:`рейтинг выпуска`, cumsum, .names = "{.col} (всего клиентов)")) %>%
  mutate(`страховая компания (всего клиентов)` = `страховая компания (всего клиентов)`+10) %>%
  mutate(`нефинансовая компания (всего клиентов)` = `нефинансовая компания (всего клиентов)`+16) %>%
  mutate(`инвестиционная компания (всего клиентов)` = `инвестиционная компания (всего клиентов)`+0) %>%
  mutate(`лизинговая компания (всего клиентов)` = `лизинговая компания (всего клиентов)`+3) %>%
  mutate(`кредитная компания (всего клиентов)` = `кредитная компания (всего клиентов)`+14) %>%
  mutate(`рейтинг выпуска (всего клиентов)` = `рейтинг выпуска (всего клиентов)`+1) %>%
  mutate(`нефинансовая компания (всего клиентов)` = case_when(
    (`Дата публикации` %in% months_numbers[4:9]) ~ `нефинансовая компания (всего клиентов)`-1,
    (`Дата публикации` %in% months_numbers[10:12]) ~ `нефинансовая компания (всего клиентов)`-2,
    .default = `нефинансовая компания (всего клиентов)`)) %>%
  mutate(`страховая компания (всего клиентов)` = case_when(
    (`Дата публикации` == months_numbers[2]) ~ `страховая компания (всего клиентов)`-1,
    (`Дата публикации` %in% months_numbers[3:12]) ~ `страховая компания (всего клиентов)`-2,
    .default = `страховая компания (всего клиентов)`)) %>%
  mutate(`Всего компаний-клиентов` = rowSums(across(`страховая компания (всего клиентов)`:`рейтинг выпуска (всего клиентов)`)))

cum_2024 <- for_cum_2024 %>%
  mutate(across(`страховая компания`:`рейтинг выпуска`, cumsum, .names = "{.col} (всего клиентов)")) %>%
  mutate(`страховая компания (всего клиентов)` = `страховая компания (всего клиентов)`+10) %>%
  mutate(`нефинансовая компания (всего клиентов)` = `нефинансовая компания (всего клиентов)`+26) %>%
  mutate(`инвестиционная компания (всего клиентов)` = `инвестиционная компания (всего клиентов)`+0) %>%
  mutate(`лизинговая компания (всего клиентов)` = `лизинговая компания (всего клиентов)`+4) %>%
  mutate(`кредитная компания (всего клиентов)` = `кредитная компания (всего клиентов)`+28) %>%
  mutate(`рейтинг выпуска (всего клиентов)` = `рейтинг выпуска (всего клиентов)`+1) %>%
  mutate(`нефинансовая компания (всего клиентов)` = case_when(
    (`Дата публикации` %in% months_numbers[4:9]) ~ `нефинансовая компания (всего клиентов)`-1,
    (`Дата публикации` %in% months_numbers[10:12]) ~ `нефинансовая компания (всего клиентов)`-2,
    .default = `нефинансовая компания (всего клиентов)`)) %>%
  mutate(`кредитная компания (всего клиентов)` = case_when(
    (`Дата публикации` %in% months_numbers[7:12]) ~ `кредитная компания (всего клиентов)`-1,
    .default = `кредитная компания (всего клиентов)`)) %>%
  mutate(`лизинговая компания (всего клиентов)` = case_when(
    (`Дата публикации` %in% months_numbers[4:12]) ~ `лизинговая компания (всего клиентов)`-1,
    .default = `лизинговая компания (всего клиентов)`)) %>%
  mutate(`Всего компаний-клиентов` = rowSums(across(`страховая компания (всего клиентов)`:`рейтинг выпуска (всего клиентов)`)))

cum_2025 <- for_cum_2025 %>%
  mutate(across(`страховая компания`:`рейтинг выпуска`, cumsum, .names = "{.col} (всего клиентов)")) %>%
  mutate(`страховая компания (всего клиентов)` = `страховая компания (всего клиентов)`+11) %>%
  mutate(`нефинансовая компания (всего клиентов)` = `нефинансовая компания (всего клиентов)`+39) %>%
  mutate(`инвестиционная компания (всего клиентов)` = `инвестиционная компания (всего клиентов)`+0) %>%
  mutate(`лизинговая компания (всего клиентов)` = `лизинговая компания (всего клиентов)`+7) %>%
  mutate(`кредитная компания (всего клиентов)` = `кредитная компания (всего клиентов)`+39) %>%
  mutate(`рейтинг выпуска (всего клиентов)` = `рейтинг выпуска (всего клиентов)`+1) %>%
  mutate(`нефинансовая компания (всего клиентов)` = case_when(
    (`Дата публикации` == months_numbers[5]) ~ `нефинансовая компания (всего клиентов)`-1,
    (`Дата публикации` == months_numbers[6]) ~ `нефинансовая компания (всего клиентов)`-2,
    (`Дата публикации` == months_numbers[7]) ~ `нефинансовая компания (всего клиентов)`-4,
    (`Дата публикации` == months_numbers[8]) ~ `нефинансовая компания (всего клиентов)`-5,
    (`Дата публикации` == months_numbers[9]) ~ `нефинансовая компания (всего клиентов)`-6,
    (`Дата публикации` == months_numbers[10]) ~ `нефинансовая компания (всего клиентов)`-8,
    (`Дата публикации` %in% months_numbers[11:12]) ~ `нефинансовая компания (всего клиентов)`-11,
    .default = `нефинансовая компания (всего клиентов)`)) %>%
  mutate(`кредитная компания (всего клиентов)` = case_when(
    (`Дата публикации` %in% months_numbers[5:9]) ~ `кредитная компания (всего клиентов)`-1,
    (`Дата публикации` %in% months_numbers[10:12]) ~ `кредитная компания (всего клиентов)`-2,
    .default = `кредитная компания (всего клиентов)`)) %>%
  mutate(`лизинговая компания (всего клиентов)` = case_when(
    # (`Дата публикации` %in% months_numbers[10:12]) ~ `лизинговая компания (всего клиентов)`-1,
    (`Дата публикации` %in% months_numbers[7:9]) ~ `лизинговая компания (всего клиентов)`+1, #корректировка на Нефтепромлизинг
    .default = `лизинговая компания (всего клиентов)`)) %>%
  mutate(`страховая компания (всего клиентов)` = case_when(
    (`Дата публикации` %in% months_numbers[4:12]) ~ `страховая компания (всего клиентов)`-1,
    .default = `страховая компания (всего клиентов)`)) %>%
  mutate(`рейтинг выпуска (всего клиентов)` = case_when(
    (`Дата публикации` %in% months_numbers[7:12]) ~ `рейтинг выпуска (всего клиентов)`-1,
    .default = `рейтинг выпуска (всего клиентов)`)) %>%
  mutate(`Всего компаний-клиентов` = rowSums(across(`страховая компания (всего клиентов)`:`рейтинг выпуска (всего клиентов)`)))

cum_2020_2025_years <- bind_rows(cum_2020_2021, cum_2022, cum_2023, cum_2024, cum_2025)

cum_2020_2025_years$`Дата публикации` <- factor(cum_2020_2025_years$`Дата публикации`, levels = months_numbers)

# write_xlsx(cum_2020_2025_years, "tibbles\\cum_2020_2025_.xlsx")


# 1.6. Подсчет клиентов Эксперт РА за 2023 г. и подготовка доп.графика -----------------------------

#таблица с кредитными рейтингами выпусков ценных бумаг и эмитентами выпусков
bonds <- ExpertRA_2023 %>%
  filter(`Запрошенный` == 'Да' & !(is.na(`Эмитент`))) %>%
  mutate(`Эмитент` = tolower(str_replace_all(`Эмитент`, "\"|»|«|  ", ""))) %>%
  pull(`Эмитент`)

#считаем количество клиентов по рейтингам выпусков ценных бумаг
bonds_clients_count <- length(unique(bonds))

#таблица с кредитными рейтингами по иным объектам
not_bonds <- ExpertRA_2023 %>%
  filter(`Запрошенный` == 'Да' & is.na(`Эмитент`)) %>%
  mutate(`Объект` = tolower(str_replace_all(`Объект`, "\"|»|«|  ", ""))) %>%
  select(`Объект`, `Тип рейтинга`)

#смотрим виды объектов рейтинга по шкалам
unique(not_bonds$`Тип рейтинга`)

#выделяем виды объектов рейтинга в отдельную колонку
not_bonds$`Вид объекта рейтинга` <- case_when(
  str_detect(not_bonds$`Тип рейтинга`, 'страховых|страхованию') ~ 'страховая организация',
  str_detect(not_bonds$`Тип рейтинга`, 'шкале РБ|Беларусь|Казахстан') ~ 'компании из Беларуси, Казахстана',
  str_detect(not_bonds$`Тип рейтинга`, 'нефинансовых') ~ 'нефинансовая компания',
  str_detect(not_bonds$`Тип рейтинга`, 'холдинговых') ~ 'холдинговая компания',
  str_detect(not_bonds$`Тип рейтинга`, 'лизинговой') ~ 'лизинговая компания',
  str_detect(not_bonds$`Тип рейтинга`, 'факторинговой') ~ 'факторинговая компания',
  str_detect(not_bonds$`Тип рейтинга`, 'банка') ~ 'кредитная организация',
  str_detect(not_bonds$`Тип рейтинга`, 'пенсионных') ~ 'негосударственный пенсионный фонд',
  str_detect(not_bonds$`Тип рейтинга`, ' финансовых') ~ 'финансовая компания',
  str_detect(not_bonds$`Тип рейтинга`, 'микрофинансовой') ~ 'микрофинансовая организация',
  str_detect(not_bonds$`Тип рейтинга`, 'проектных') ~ 'проектная компания',
  str_detect(not_bonds$`Тип рейтинга`, 'депозитариев') ~ 'депозитарий',
  str_detect(not_bonds$`Тип рейтинга`, 'гарантийной') ~ 'региональная гарантийная организация',
  str_detect(not_bonds$`Тип рейтинга`, 'региона|муниципалитета') ~ 'публично-правовое образование'
)

#считаем количество клиентов по видам объекта рейтинга
count_ExpertRa_2023 <- not_bonds %>%
  distinct(`Объект`, .keep_all = TRUE) %>%
  count(`Вид объекта рейтинга`) %>%
  add_row(`Вид объекта рейтинга` = 'выпуск ценных бумаг', n = 102) %>%
  add_row(`Вид объекта рейтинга` = 'пропуск', n = 0) %>%
  mutate(`Год` = '2023')

#определяем последовательность видов объекта рейтинга для графика
count_ExpertRa_2023$`Вид объекта рейтинга` <- factor(count_ExpertRa_2023$`Вид объекта рейтинга`,
                                                     levels = c('кредитная организация',
                                                                'нефинансовая компания',
                                                                'страховая организация',
                                                                'лизинговая компания',
                                                                'выпуск ценных бумаг',
                                                                'пропуск',
                                                                'публично-правовое образование',
                                                                'микрофинансовая организация',
                                                                'финансовая компания',
                                                                'холдинговая компания',
                                                                'негосударственный пенсионный фонд',
                                                                'компании из Беларуси, Казахстана',
                                                                'проектная компания',
                                                                'факторинговая компания',
                                                                'депозитарий',
                                                                'региональная гарантийная организация'
                                                     ))


#создаем таблиццу для лейбла "иные виды объектов рейтинга"
#(лейбл заменит подробное раскрытие всех видов объекта рейтинга,
#кроме интересующих нас в контексте деятельности НРА)
other_companies_tibble <- tibble(
  x = '2023',
  y = 60,
  lab = '       Иные виды
  объектов рейтинга'
)

#рисуем график
ExpertRa <- ggplot() +
  geom_col(data = count_ExpertRa_2023, aes(x=`Год`, y=n, fill = `Вид объекта рейтинга`),
           position = "dodge",
           width = 0.5) +
  scale_y_continuous(breaks = seq(0, 190, by = 10),
                     limits = c(-0.5,190)) +
  scale_fill_manual(values = c('кредитная организация' = '#D1705C',
                               'нефинансовая компания' = '#BDD15C',
                               'лизинговая компания' = '#D590AD',
                               'страховая организация' = '#5CBDD1',
                               'выпуск ценных бумаг' = '#B8C1C1',
                               'пропуск' = '#00000000',
                               'публично-правовое образование' = '#DEB57D',
                               'микрофинансовая организация' = '#6D9CB0',
                               'финансовая компания' = '#84B179',
                               'холдинговая компания' = '#B77466',
                               'негосударственный пенсионный фонд' = '#C8AAAA',
                               'компании из Беларуси, Казахстана' = '#9BB4C0',
                               'проектная компания' = '#E1D0B3',
                               'факторинговая компания' = '#574964',
                               'депозитарий' = '#957C62',
                               'региональная гарантийная организация' = '#A8BBA3')) +
  theme_light() +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 30),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        plot.background = element_rect(colour = "black", fill = 'white', linewidth = 2)) +
  geom_label(data = other_companies_tibble,
             aes(x = x, y = y, label = lab),
             size = 7,
             fill = 'white',
             color = "darkgrey",
             lineheight = 0.8,
             hjust = 0.2) +
  labs(title = 'Клиенты АО "Эксперт РА" в 2023 г.')

#сохраняем его как изображение в формате png
# ggsave("draft_graphs\\ExpertRA_final_.png", plot = ExpertRa, width = 297, height = 210, units = "mm", dpi = 300)


# 2. Подготовка материалов для графика ------------------------------------

#---ОСНОВНАЯ ТАБЛИЦА---

#отбираем нужные строки
#переименовываем колонки
#переводим в длинный формат
tibble_for_separate_bars <- cum_2020_2025_years %>%
  select(`Дата публикации`, `Год`, `страховая компания (всего клиентов)`:`рейтинг выпуска (всего клиентов)`) %>%
  rename('страховая организация' = `страховая компания (всего клиентов)`,
         'кредитная организация' = `кредитная компания (всего клиентов)`,
         'нефинансовая компания' = `нефинансовая компания (всего клиентов)`,
         'инвестиционная компания' = `инвестиционная компания (всего клиентов)`,
         'лизинговая компания' = `лизинговая компания (всего клиентов)`,
         'выпуск ценных бумаг' = `рейтинг выпуска (всего клиентов)`) %>%
  pivot_longer(cols = c(`страховая организация`:`выпуск ценных бумаг`),
               names_to = "Вид объекта рейтинга",
               values_to = "Количество компаний")

# write_xlsx(tibble_for_separate_bars, "tibbles\\tibble_for_separate_bars_2025_.xlsx")

#задаем порядок столбцов на графике с помощью фактора
tibble_for_separate_bars$`Вид объекта рейтинга` <- factor(tibble_for_separate_bars$`Вид объекта рейтинга`,
                                                          levels = c('кредитная организация',
                                                                     'нефинансовая компания',
                                                                     'страховая организация',
                                                                     'лизинговая компания',
                                                                     'инвестиционная компания',
                                                                     'выпуск ценных бумаг'))

#---ДОПОЛНИТЕЛЬНЫЕ ТАБЛИЦЫ---

#для geom_segment - чтобы показать наличие/отсутствие методологии
#в определенном месяце

metogology_banks_2021 <- tibble(
  x = 06, xend = 12,
  y = 56.6, yend = 56.6, #57
  `Год` = 2021
)

metogology_banks_2022 <- tibble(
  x = 01, xend = 12,
  y = 56.6, yend = 56.6,
  `Год` = c(2022, 2023, 2024, 2025)
)

metogology_nonfinanсial <- tibble(
  x = 01, xend = 12,
  y = 55.8, yend = 55.8,
  `Год` = c(2020, 2021, 2022, 2023, 2024, 2025)
)

metogology_insurance <- tibble(
  x = 01, xend = 12,
  y = 55, yend = 55,
  `Год` = c(2020, 2021, 2022, 2023, 2024, 2025)
)

metogology_leasing <- tibble(
  x = 01, xend = 12,
  y = 54.2, yend = 54.2,
  `Год` = c(2021, 2022, 2023, 2024, 2025)
)

metogology_investment_2020 <- tibble(
  x = 10, xend = 12,
  y = 53.4, yend = 53.4,
  `Год` = 2020
)

metogology_investment_2021_2022 <- tibble(
  x = 01, xend = 12,
  y = 53.4, yend = 53.4,
  `Год` = c(2021, 2022)
)

metogology_investment_2023 <- tibble(
  x = 01, xend = 10,
  y = 53.4, yend = 53.4,
  `Год` = 2023
)

metogology_issue <- tibble(
  x = 01, xend = 12,
  y = 52.6, yend = 52.6,
  `Год` = c(2020, 2021, 2022, 2023, 2024, 2025)
)

#для geom_segment - чтобы сделать на графике небольшие
#"платформы", на которых будут стоять столбцы.
#Без них, когда определенного вида компаний в месяце нет,
#одиночные столбцы странно выглядят, располагаясь не по центру
#отметки на X
for_bars_tibble <- tibble(
  x = rep(01:12, 6), xend = rep(01:12, 6),
  y = rep(-0.2, 72), yend = rep(0.1, 72),
  `Год` = rep(c(2020, 2021, 2022, 2023, 2024, 2025), each = 12)
)

#таблица для geom_vline, она будет показывать,
#в каком месяце рейтинги НРА были внесены в регулирование
#деятельности различных участников фин. рынка
tibble_all_reg <- read_excel('raw\\Regulation.xlsx')

#для графика по АО "Эксперт РА"
for_comparison <- tibble(
  x = '05',
  y = 30,
  `Год` = 2020,
  image = "draft_graphs\\ExpertRA_final_.png"
)

# 3. График ---------------------------------------------------------------

plot_NRA  <- ggplot()+
  geom_col(data = tibble_for_separate_bars,
           aes(x = `Дата публикации`, y = `Количество компаний`, fill = `Вид объекта рейтинга`),
           position = "dodge") +
  scale_y_continuous(breaks = seq(0, 60, by = 10),
                     limits = c(-0.5,60),
                     sec.axis = sec_axis(transform = ~.*1,
                                         breaks = 55,
                                         labels = c("55" = "Наличие\n методологии")
                     )) +
  scale_fill_manual(values = c('кредитная организация' = '#D1705C',
                               'нефинансовая компания' = '#BDD15C', ##BDD15C
                               'лизинговая компания' = '#D590AD', ##D590AD  #5CBDD1
                               'страховая организация' = '#5CBDD1', ##5CBDD1
                               'инвестиционная компания' = '#6F5CD1', ##6F5CD1
                               'выпуск ценных бумаг' = '#B8C1C1')) +
  theme_light() +
  theme(axis.text.y.right = element_text(angle = -90, hjust = 0.5, vjust = 3),
        legend.position = "top", legend.direction = "horizontal",
        legend.key.size = unit(0.5, "cm"),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(vjust = 3.5),
        axis.title.x = element_text(vjust = -0.5)) +
  guides(fill = guide_legend(nrow = 1, title = 'Объект рейтинга:')) +
  labs(
    title = "Количество клиентов ООО «Национальное Рейтинговое Агентство» за 2020-2025 гг.",
    caption = "Источник данных: https://www.ra-national.ru/
    *Вертикальные пунктирные линии означают включение кредитных рейтингов
    в регулирование, лейблы - предмет регулирования, '+' - дополнение регулирования.",
    x = "Месяц",
    y = "Количество клиентов, шт."
  ) +
  facet_wrap(~`Год`, scales = "free_x") +
  geom_image(data = for_comparison, aes(image = image, x = x, y = y), size = 0.8) +
  geom_vline(
    data = tibble_all_reg,
    aes(xintercept = x),
    inherit.aes = FALSE,
    color = "grey",
    linewidth = 0.3,
    linetype = 'dashed'
  ) +
  geom_label(data = tibble_all_reg,
            aes(x = x, y = y, label = lab),
            size = 2.5,
            fill = 'white',
            color = "darkgrey",
            lineheight = 0.8) +
  geom_segment(
    data = for_bars_tibble,
    aes(x = x, xend = xend, y = y, yend = yend),
    inherit.aes = FALSE,
    color = "darkgrey",
    linewidth = 9
  ) +
  geom_segment(
    data = metogology_banks_2021,
    aes(x = x, xend = xend, y = y, yend = yend),
    inherit.aes = FALSE,
    color = "#D1705C",
    linewidth = 1.2
  ) +
  geom_segment(
    data = metogology_banks_2022,
    aes(x = x, xend = xend, y = y, yend = yend),
    inherit.aes = FALSE,
    color = "#D1705C",
    linewidth = 1.2
  ) +
  geom_segment(
    data = metogology_nonfinanсial,
    aes(x = x, xend = xend, y = y, yend = yend),
    inherit.aes = FALSE,
    color = "#BDD15C",
    linewidth = 1.2
  ) +
  geom_segment(
    data = metogology_insurance,
    aes(x = x, xend = xend, y = y, yend = yend),
    inherit.aes = FALSE,
    color = "#5CBDD1",
    linewidth = 1.2
  ) +
  geom_segment(
    data = metogology_issue,
    aes(x = x, xend = xend, y = y, yend = yend),
    inherit.aes = FALSE,
    color = "#B8C1C1",
    linewidth = 1.2
  ) +
  geom_segment(
    data = metogology_investment_2020,
    aes(x = x, xend = xend, y = y, yend = yend),
    inherit.aes = FALSE,
    color = "#6F5CD1",
    linewidth = 1.2
  ) +
  geom_segment(
    data = metogology_investment_2021_2022,
    aes(x = x, xend = xend, y = y, yend = yend),
    inherit.aes = FALSE,
    color = "#6F5CD1",
    linewidth = 1.2
  ) +
  geom_segment(
    data = metogology_investment_2023,
    aes(x = x, xend = xend, y = y, yend = yend),
    inherit.aes = FALSE,
    color = "#6F5CD1",
    linewidth = 1.2
  ) +
  geom_segment(
    data = metogology_leasing,
    aes(x = x, xend = xend, y = y, yend = yend),
    inherit.aes = FALSE,
    color = "#D590AD",
    linewidth = 1.2
  )

# ggsave("draft_graphs\\NRA_final_2020_2025_caption.png", plot = plot_NRA, width = 297, height = 210, units = "mm", dpi = 300)

