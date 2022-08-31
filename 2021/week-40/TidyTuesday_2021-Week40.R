# TidyTuesday: Week 40 ----
# 2021/09/28
# Data from the National Bureau of Economic Research [NBER](https://www2.nber.org/RePEc/nbr/nberwo/) by way of the [nberwp package by Ben Davies](https://github.com/bldavies/nberwp).
# code pasted from Lee Olney's visualization here https://pastebin.pl/view/3ef6b47f

# load packages ----

library(tidyverse)
library(glue)
library(gt)
library(gtExtras)

# load datasets ----

papers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/papers.csv')
authors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/authors.csv')
programs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/programs.csv')
paper_authors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/paper_authors.csv')
paper_programs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/paper_programs.csv')

# Preparing Datasets ----

## joining dataframes
paper_df <- papers %>% 
  left_join(paper_authors) %>%
  left_join(authors) %>%
  left_join(paper_programs) %>%
  left_join(programs)%>%
  select(-5,-7,-8,-11) %>% 
  rename(author = name) %>% 
  mutate(
    catalogue_group = str_sub(paper, 1, 1),
    catalogue_group = case_when(
      catalogue_group == "h" ~ "Historical",
      catalogue_group == "t" ~ "Technical",
      catalogue_group == "w" ~ "General"
    )
  )

## changing NA values with "Others"
paper_df <- paper_df %>% 
  replace_na(list(program = "Others", 
                  program_desc = "Others")) 

## summarise the dataset
grouped_year <-  paper_df %>% group_by(program, program_desc, year) %>%
  summarise(n=n_distinct(paper)) %>%
  arrange(year) %>%
  mutate(
    decade = case_when(
      between(year, 1970, 1979) ~ "1970s",
      between(year, 1980, 1989) ~ "1980s",
      between(year, 1990, 1999) ~ "1990s",
      between(year, 2000, 2009) ~ "2000s",
      between(year, 2010, 2019) ~ "2010s"
    )) %>%
  drop_na()

grouped_prgm <- grouped_year %>% 
  mutate(program=glue::glue("{program_desc} ({program})")) %>%
  group_by(program) %>% mutate(total=sum(n)) %>% 
  arrange(year, program) %>%
  mutate(trend=list(n)) %>%
  select(program, trend, total) %>%
  distinct()

decade_fct <- c("1970s", "1980s", "1990s", "2000s", "2010s")

grouped_decade <- grouped_year %>% 
  mutate(program=glue::glue("{program_desc} ({program})")) %>%
  group_by(program, decade) %>% 
  tally(n) %>%
  ungroup() %>%
  mutate(decade = factor(decade, levels = decade_fct)) %>% 
  pivot_wider(names_from = decade, values_from=n) %>%
  mutate_if(is.numeric, list(~replace_na(., 0)))

smmry_df <- grouped_decade %>% 
  inner_join(grouped_prgm, by="program") %>%
  select(program, total,"1970s", "1980s","1990s","2000s","2010s", trend) %>%
  arrange(desc(total)) 

# visualization ----
## plotting
plot <- smmry_df %>% 
  gt() %>%
  gt_theme_espn() %>%
  cols_align(program, align="left") %>%
  gt_plt_dot(
    total, program,
    palette = "rcartocolor::ag_GrnYl", max_value=5287) %>%
  gtExtras::gt_sparkline(trend) %>%
  tab_options(table.font.size = 12.5,
              heading.subtitle.font.size = 14) %>%
  gt_color_box(`1970s`, domain = 1 : 215) %>% 
  gt_color_box(`1980s`, domain = 2 : 786) %>%
  gt_color_box(`1990s`, domain = 2 : 797) %>%
  gt_color_box(`2000s`, domain = 9 : 1647) %>%
  gt_color_box(`2010s`, domain = 200 : 2424) %>%
  tab_header(title="Economic Papers", subtitle="Working papers count by program and decade") %>%
  tab_source_note(source_note="TidyTuesday Week 40 | Data source: National Bureau of Economic Research (NBER) by way of the nberwp package by Ben Davies")

## saving to png
gtsave(
  data = plot,
  filename = "Outfile/2021/TidyTuesday_Week40.png"
)
