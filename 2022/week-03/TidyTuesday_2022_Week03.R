# load packages ----
library(tidyverse)
library(janitor)
library(ggtext)
library(waffle)
library(ggforce)
library(patchwork)
library(countrycode)
library(glue)
library(hrbrthemes)

# load fonts ----
library(sysfonts)
library(showtext)
font_add_google("Playfair Display")
font_add_google("Roboto Condensed")
font_add_google("Open Sans")
font_add_google("Fira Mono")
font_add_google("Bebas Neue")
showtext_auto()
showtext_opts(dpi = 300)

# load data ----

choc_raw <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-18/chocolate.csv')

# wrangle data ----

## convert Indonesia's islands name to "Indonesia"
choc_country <- data.frame(unique(choc_raw$country_of_bean_origin)) %>% 
  janitor::clean_names() %>% 
  rename(country = unique_choc_raw_country_of_bean_origin) %>% 
  mutate(
    country_of_bean_origin = case_when(
      country %in% c("Sumatra", "Sulawesi") ~ "Indonesia",
      TRUE ~ country
    )
  )

## add continent to identify countries
choc_country$continent <- countrycode(sourcevar = choc_country[, 2],
                            origin = "country.name",
                            destination = "continent")


## > filter only beans from Asia ----
choc_asia_df <- choc_raw %>% 
  left_join(
    choc_country %>% select(-1)
  ) %>% 
  dplyr::filter(
    continent == "Asia"
  ) %>% 
  unique()

choc_asia_df %>% 
  count(country_of_bean_origin, name = "n_of_country") %>% 
  arrange(desc(n_of_country)) %>% 
  mutate(country_of_bean_origin = factor(country_of_bean_origin,
                                         levels = unique(country_of_bean_origin))
         ) %>% 
  ggplot() +
  geom_bar(
    aes(x = country_of_bean_origin,
        y = n_of_country),
    stat = "identity"
  )

## extract each characteristics
choc_asia_df <- choc_asia_df %>% 
  mutate(char = str_split(most_memorable_characteristics, ",")) %>% 
  select(-most_memorable_characteristics) %>% 
  unnest(char) %>% 
  mutate(
    char = str_trim(char),
    one = 1
  )

choc_asia_df %>% 
  count(char) %>%
  arrange(desc(n)) %>% 
  mutate(char = factor(char, levels = unique(char))) %>% 
  as.data.frame() -> choc_asia_char

choc_asia_char %>% 
  ggplot() +
  geom_bar(
    aes(
      x = char,
      y = n
    ),
    stat = "identity"
  ) +
  coord_flip()

df_choc <- choc_raw |>
  mutate(char = str_split(most_memorable_characteristics, ",")) |>
  select(-most_memorable_characteristics) |>
  unnest(char) |>
  mutate(
    char = str_trim(char),
    char = fct_lump(char, 20),
    one = 1
  ) |>
  distinct()



## > filter only beans from Indonesia ----
choc_ina_df <- choc_raw %>% 
  left_join(
    choc_country %>% select(-1)
  ) %>% 
  dplyr::filter(
    country_of_bean_origin == "Indonesia"
  ) %>% 
  unique()

## extract characteristics
choc_ina_df <- choc_ina_df %>% 
  mutate(char = str_split(most_memorable_characteristics, ",")) %>% 
  select(-most_memorable_characteristics) %>% 
  unnest(char) %>% 
  mutate(
    char = str_trim(char)
  )

## categorizing characteristics (from https://www.barry-callebaut.com/)
char_cocoa <- c("cocoa", "high acidity")
char_dairy <- c("creamy", "fatty", "sticky")
char_floral <- c("floral")
char_fruit <- c("fruit", "banana", "berry", "dried fruit (fig)", "fruity",
                "grape", "raspberries", "red fruit", "some fruit")
char_nutty <- c("nutty", "nut")
char_prime <- c("sour", "harsh", "intense", "late sour", "medicinal", "salt")
char_spicy <- c("black licorice", "black pepper and banana", "cardamom",
                "hay", "off spicey", "spice")

choc_ina_df <- choc_ina_df %>% 
  mutate(
    char_category = case_when(
      char %in% char_cocoa ~ "cocoa",
      char %in% char_dairy ~ "dairy",
      char %in% char_floral ~ "floral",
      char %in% char_fruit ~ "fruity",
      char %in% char_nutty ~ "nutty",
      char %in% char_prime ~ "primary taste",
      char %in% char_spicy ~ "spicy",
      TRUE ~ "earthy"
    )
  ) %>% 
  select(-char) %>% 
  mutate(
    char_category = str_to_title(as.character(char_category)),
    char_category = fct_lump(char_category, 5),
    one = 1
  ) %>% 
  distinct()

choc_ina_df %>% 
  count(char_category) %>%
  arrange(desc(n)) %>% 
  mutate(char_category = factor(char_category, 
                                levels = unique(char_category))) %>% 
  as.data.frame() %>% 
  ggplot() +
  geom_bar(
    aes(
      x = char_category,
      y = n
    ),
    stat = "identity"
  ) +
  coord_flip()

# plotting ----
## we are gonna make waffle plot

# > prepare dataframe
choc_plot_df <- choc_ina_df %>% 
  count(char_category) %>%
  arrange(desc(n)) %>% 
  mutate(char_category = factor(char_category, 
                                levels = unique(char_category))) %>% 
  as.data.frame() 

choc_plot <- choc_plot_df %>% 
  dplyr::filter(char_category != "Other") %>% 
  ggplot(aes(fill = char_category,
             values = n)) +
  geom_waffle(
    n_rows = 5,
    color = "#FFFFFA",
    size = 1.5,
    radius = unit(5, "pt")
  ) +
  scale_fill_manual(values = c("#1A0E02", "#533C28","#824003", 
                               "#D16E11", "#FAB06B")) +
  labs(
    title = "<span style = 'color:#C82D38;'>Indonesian</span> cocoa tastes <span style = 'color:#1A0E02;'>earthy</span>, <span style = 'color:#533C28;'>fruity</span>, <span style = 'color:#824003;'>bitter and sour</span>, a bit of <span style = 'color:#D16E11;'>spicy</span>, and some <span style = 'color:#FAB06B;'>dairy</span>",
    subtitle = "20 samples of chocolate bars made from Indonesian cocoa beans are rated for their most memorable characteristics\nOne box represents one chocolate bar that has the characteristic category as described by Barry Callebaut",
    fill = NULL, colour = NULL,
    caption = "Data: Flavors of Cacao by way of Georgios and Kelsey | Graphic: @rrrizqiramadhan"
  ) +
  coord_equal() +
  theme_ipsum_rc(grid = "") +
  theme_enhance_waffle() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#FFFFFA", colour = "#FFFFFA"),
    panel.background = element_rect(fill = NA, color = NA),
    panel.border = element_rect(fill = NA, color = NA),
    plot.title = element_markdown(
      hjust = 0.5, 
      family = "Playfair Display",
      face = "bold",
      size = 20,
      margin = margin(0, 0, 5, 0)),
    plot.subtitle = element_text(
      hjust = 0.5, 
      family = "Open Sans",
      colour = "#0D0905",
      face = "italic",
      size = 11,
      margin = margin(2, 0, 2, 0)),
    plot.caption = element_text(
      family = "Roboto Condensed",
      hjust = .5,
      size = 9.5,
      color = "grey70",
      margin = margin(5, 0, 5, 0)
    ),
    plot.margin = margin(5, 20, 5, 20),
    legend.position = "none"
  )

ggsave(
  choc_plot,
  filename ="Outfile/2022/TidyTuesday_w-03.png",
  width = 12,
  height = 8,
  dpi = 300)
  