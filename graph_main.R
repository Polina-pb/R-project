#3. ОСНОВНОЙ ГРАФИК

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

cum_2020_2025_years <- read_excel('tibbles\\cum_2020_2025_.xlsx')

# Подготовка материалов для графика ------------------------------------

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
#можно было их сложить в один тиббл, как я сделала в других случаях (например, для geom_vline)
#но так как я наполняла график постепенно, тибблы получились отдельные

metogology_banks_2021 <- tibble(
  x = 06, xend = 12,
  y = 56.6, yend = 56.6,
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

