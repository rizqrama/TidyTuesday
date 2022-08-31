# load packages ----
library(tidyverse)
library(scales)
library(hrbrthemes)

# load fonts ----
library(sysfonts)
library(showtext)
font_add_google("Playfair Display")
font_add_google("Roboto Condensed")
font_add_google("Open Sans")
font_add_google("Fira Mono")
showtext_auto()
showtext_opts(dpi = 300)

# load data ----
colony <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/colony.csv')

stressor <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/stressor.csv')
stressor[is.na(stressor)] <- 0

s1 <- stressor %>%
  filter(state!=c("United States","Other States")) %>% #47 states 
  filter(stressor %in% c("Varroa mites","Other pests/parasites")) %>%
  mutate(stress_pct = stress_pct/100) %>%
  pivot_wider(names_from = "stressor", values_from="stress_pct") %>%
  group_by(state) %>%
  mutate(number= row_number()) %>%
  ungroup()

# Trial A: wrangling ----

pesticide_us <- 
  colony %>% 
  dplyr::filter(state == "United States") %>% 
  select(1:3, 6) %>% 
  left_join(
    stressor,
    by = c("year", "months", "state")
  ) %>% 
  dplyr::filter(
    stressor %in% c(
      "Varroa mites", 
      "Other pests/parasites",
      "Pesticides"
    )
  ) %>% 
  pivot_wider(
    names_from = "stressor",
    values_from = "stress_pct"
  ) %>% 
  janitor::clean_names() %>% 
  mutate(
    parasites = (varroa_mites + other_pests_parasites) * colony_lost,
    pesticide_n = pesticides * colony_lost
  ) %>% 
  select(1,2,8,9) %>% 
  rename(Pesticides = pesticide_n,
         "Pests/Parasites" = parasites
  ) %>% 
  mutate(time_stamp= row_number()) %>% 
  pivot_longer(
    c(3:4),
    names_to = "stressors",
    values_to = "colony_lost"
  )

# Trial A: plot ----
week2_plot <- pesticide_us %>% 
  ggplot(
    aes(
      x = time_stamp,
      y = colony_lost,
      color = stressors
    )
  ) +
  geom_line(
    alpha = .8,
    size = .9
  )

# Trial B: wrangling ----
b_season_df <- colony %>% 
  select(
    1:4, 6, 9
  ) %>% 
  left_join(
    stressor %>% 
      mutate(stress_pct = stress_pct/100) %>% 
      pivot_wider(
        names_from = "stressor",
        values_from = "stress_pct"
      ),
    by = c("year", "months", "state")
  ) %>% 
  mutate(
    colony_lost_pct = colony_lost/colony_n,
    colony_reno_pct = colony_reno/colony_n,
    season = case_when(
      months == "January-March" ~ "Winter",
      months == "April-June" ~ "Spring",
      months == "July-September" ~ "Summer",
      months == "October-December" ~ "Autumn"
    ),
    season_type = case_when(
      months == "January-March" ~ "Winter",
      TRUE ~ "Other seasons"
    ),
    "Other and Unknown causes" = Disesases + Other + Unknown
  ) %>% 
  select(-9, -11, -12)

# trial B: plot 1 - scatter plot ----
b_plot_1 <- b_season_df %>% 
  dplyr::filter(
    !is.na(colony_n),
    !is.na(colony_lost_pct),
    colony_lost_pct <= 1,
    !is.na(colony_reno_pct),
    colony_reno_pct <= 1,
    state != "United States"
    ) %>% 
  ggplot(
    aes(
      x = colony_reno_pct,
      y = colony_lost_pct,
      size = colony_n,
      fill = season_type,
      color = season_type
    )
  ) +
  geom_point(
    alpha = 0.6
  )

# trial B: plot 2 - line chart ----
stressors <- c("Varroa mites", "Other pests/parasites", "Pesticides", "Other and Unknown causes")

seasons <- c( "Summer", "Autumn","Winter", "Spring")

b_plot_2_df <- b_season_df %>% 
  dplyr::filter(state == "United States") %>% 
  dplyr::filter(season %in% c("Summer", "Winter")) %>% 
  pivot_longer(
    cols = c(7:9, 14),
    names_to = "stressor_type",
    values_to = "stressor_pct"
  ) %>% 
  mutate(
    stressor_n = colony_n * stressor_pct,
    stressor_type = factor(stressor_type, levels = stressors),
    season = factor(season, levels = seasons)
  )

b_plot_2 <- b_plot_2_df %>% 
  ggplot(
    aes(
      x = year,
      y = stressor_n
    )
  ) +
  geom_line(
    aes(
      color = season
    ),
    alpha = .6,
    size = 1.1
  ) +
  facet_wrap(~stressor_type, nrow = 1) +
  labs(
    x = NULL,
    y = NULL,
    title  = "Bee-gone in Summer",
    subtitle = "regardless of stressor types, bee colonies plummet more in summer rather than winter",
    caption = str_glue(
      "Data: USDA | Graphic: Ramadhan, R."
    )
  ) +
  scale_color_manual(
    values = c("#3A3637", "#FCB43A")
  ) +
  scale_x_continuous(
    breaks = c(2015, 2018, 2021),
    labels = c("'15", "'18", "'21")
  ) +
  theme_ipsum_rc(
    grid = FALSE,
    ticks = FALSE
  ) +
  theme(
    plot.background = element_rect(fill = "#FFFFFA", colour = "#FFFFFA"),
    panel.background = element_rect(fill = NA, color = NA),
    panel.border = element_rect(fill = NA, color = "grey90"),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(
      family = "Fira Mono",
      colour = "#090740",
      size = 10
    ),
    axis.text.y = element_text(
      family = "Fira Mono",
      colour = "#090740",
      size = 10
    ),
    plot.title = element_text(
      hjust = 0, 
      family = "Playfair Display",
      colour = "#FCB43A",
      size = 20,
      margin = margin(5, 0, 5, 0)),
    plot.subtitle = element_text(
      hjust = 0.0, family = "Open Sans",
      colour = "#3A3637",
      size = 14,
      margin = margin(5, 0, 5, 0)),
    plot.caption = element_text(
      family = "Roboto Condensed",
      size = 10,
      color = "grey70",
      margin = margin(7, 0, 5, 0)
    ),
    legend.title = element_blank(),
    legend.text = element_text(
      family = "Roboto Condensed",
      color = "grey20",
      size = 8),
    legend.position = "bottom", 
    legend.key = element_rect(
      fill = NA, colour = NA),
    legend.key.width = unit(2, "lines"),
    legend.background = element_rect(
      fill = "#FFFFFA", colour = NA),
    plot.margin = margin(10, 20, 10, 20)
  )
  
ggsave(
  b_plot_2,
  filename = "Outfile/2022/TidyTuesday_w-02.png",
  width = 13,
  height = 8
)
