# TidyTuesday 2020 Week 15 ----
## This visualization is reproduced from Ben Novak's https://github.com/BjnNowak/TidyTuesday/blob/main/SC_TdF_Table.R

# install packages ----
library(tidyverse)
library(gt)
library(gtExtras)

# load data ---- 
## Whole race winners
tdf_winners <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-07/tdf_winners.csv')
## Stages winners
tdf_stages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-07/tdf_stages.csv') %>%
  janitor::clean_names()

head(tdf_stages)

# A. 1961 Tour de France's Stage Winners ----
## here we will try to make gt for the stage winners of 1961 Tour de France

# > preparing dataset ----
## filtering the 1961 Tour de France's stages winners

tdf_61 <- tdf_stages %>% 
  dplyr::filter(lubridate::year(date) == 1961)

# > make gt objects ----

tab_61 <- tdf_61 %>% 
  head(8) %>% ## choose only top 8
  gt() ## make table
  
# > format and customize the table ----

## format columns of date, number, and even renaming columns
tab_61 <- tab_61 %>% 
  tab_header(
    title = "Stage Winners",
    subtitle = md("*Tour de France* 1961") ## use markdown syntax with md()
  ) %>% 
  fmt_date(
    columns = date,
    date_style = 9 ## format date without year information
  ) %>%  
  fmt_number(
    columns = distance,
    decimals = 0,
    pattern = "{x} km" ## add 'km' as suffix
  ) %>% 
  cols_label(
    winner_country = "nationality" ## rename column
  )

## add themes and other vis customization

tab_61 <- tab_61 %>%
  gtExtras::gt_theme_nytimes() %>% ## adding predefined themes
  tab_style(
    locations = cells_title(groups = 'title'),
    style = list(
      cell_text(
        font = google_font(name = 'Bebas Neue'),
        size = 'xx-large',
        color = 'indianred'
      ) ## select cells to modify and add font customization
    )
    
  )

## highlight row(s)
  
tab_61 <- tab_61 %>% 
  gtExtras::gt_highlight_rows(
    rows = 5, ## row to highlight
    fill = "lightgrey", ## background color
    bold_target_only = TRUE, ## bold for target column only
    target_col = winner
  )

# B. Tour de France most winners ----
## here we will visualize TdF's most winners using gt and gtExtra, including iamge addition to the table object

# > preparing data set ----

tdf_most <- tdf_winners %>% 
  dplyr::filter(winner_name != "Lance Armstrong") %>% ## drugs convicted
  mutate(winner_name = case_when(
    winner_name == 'Miguel Induráin'~'Miguel Indurain',
    TRUE ~ winner_name
  )) %>% 
  mutate(ct = 1) %>% ## add variables to count title
  group_by(winner_name) %>%  ## group by winner name
  summarise(
    titles = sum(ct), ## counting titles
    country = nationality[1], ## add nationality
    nickname = nickname[1] ## add nicknames
  ) %>% 
  dplyr::filter(titles > 2) %>% ## choose winners with titles more than 2
  arrange(desc(titles)) %>% ## sort by descending order
  select(
    rider = winner_name,
    nickname,
    country,
    titles
  ) %>% ## reordering table
  mutate(
    nickname = case_when(
      str_detect(rider, "Hinault") ~ "The Badger",
      str_detect(rider, "Anquetil") ~ "Monsieur Chrono",
      str_detect(rider, "Indurain") ~ "Big Mig",
      str_detect(rider, "LeMond") ~ "LeMonster",
      str_detect(rider, "Bobet") ~ "Zonzon",
      str_detect(rider, "Thys") ~ "The Basset Hound",
      TRUE ~ nickname
    )
  )

# customize nationality with pictures
tdf_most <- tdf_most %>% 
  mutate(country = case_when(
    str_detect(country,'France') ~ 'https://raw.githubusercontent.com/BjnNowak/TdF/main/fr.png',
    str_detect(country,'Belgium') ~ 'https://raw.githubusercontent.com/BjnNowak/TdF/main/be.png',
    str_detect(country,'Great Britain') ~ 'https://raw.githubusercontent.com/BjnNowak/TdF/main/uk.png',
    str_detect(country,'Spain') ~ 'https://raw.githubusercontent.com/BjnNowak/TdF/main/sp.png',
    str_detect(country,'United States') ~ 'https://raw.githubusercontent.com/BjnNowak/TdF/main/us.png'
  ))

# try to plot
tdf_most %>% 
  gt() %>% 
  tab_header(
    title = md("Most Successful Riders in *Tour de France*")
  ) %>% 
  gtExtras::gt_theme_nytimes() %>% 
  ## merge two columns
  gtExtras::gt_merge_stack(
    col1 = rider,
    col2 = nickname
  ) %>% 
  gtExtras::gt_img_rows(
    columns = country, ## add lag images
    height = 20
  ) %>% 
  gtExtras::gt_fa_repeats(
    column = titles,
    palette = "orange",
    name = "tshirt",
    align = "left"
  )

## add trend/timeline of winning titles
names_most_wins<- tdf_most %>%
  pull(rider) ## reate a vector with names of riders with most wins

tdf_most_year <- tdf_winners %>% 
  mutate(rider = case_when(
    winner_name == 'Miguel Induráin'~'Miguel Indurain',
    TRUE ~ winner_name
  )) %>% 
  mutate(ct = 1) %>% 
  ## create new rows with ct = 0 if didn't win that year
  complete(rider, edition, fill = list(ct = 0)) %>% 
  group_by(rider) %>% 
  summarise(timeline = list(ct)) %>% ## create win list for every rider
  dplyr::filter(rider %in% names_most_wins)

## try to plot with sparkline
tdf_most_year %>% 
  gt %>% 
  gtExtras::gt_sparkline(
    timeline, ## select column
    range_colors = c("#67686b","indianred"), ## create range for min max
    line_color = "lightgrey"
  ) %>% 
  tab_header(
    title = "titles timeline"
  ) %>% 
  gtExtras::gt_theme_nytimes()

## examine stages winning for each riders
tdf_most_stage <- tdf_stages %>% 
  mutate(rider = case_when(
    winner == 'Miguel Induráin'~'Miguel Indurain',
    TRUE ~ winner
)) %>% 
  dplyr::filter(rider %in% names_most_wins) %>% 
  ## keep only 3 types of stages: time trial, mountain, plain
  mutate(types = case_when(
    str_detect(type, "trial") ~ "Time trial",
    str_detect(type, "mountain") ~ "Mountain stage",
    str_detect(type, "Mountain") ~ "Mountain stage",
    str_detect(type, "Hilly") ~ "Mountain stage",
    TRUE ~ "Plain stage"
  )) %>% 
  group_by(rider, types) %>% 
  mutate(ct = 1) %>% 
  summarise(
    wins = sum(ct)
  ) %>% 
  ungroup() %>% 
  ## complete with NA for empty winning stage (rider * type of stages)
  complete(rider, types, fill = list(wins = NA)) %>% 
  group_by(rider) %>% 
  summarise(stages = list(wins))

## try to plot
clr_stages <- c('#264653','#e9c46a','#e76f51')

tdf_most_stage %>% 
  gt %>% 
  gt_plt_bar_stack(
    column = stages, # column with data
    position = "stack", # choose stacked barplot
    labels = c("Mountain stage", "Plain stage", "Time trial"),
    palette = clr_stages,
    width = 60, ## barplot width
    trim = TRUE ## same size for all labels
    ) %>% 
  tab_header(
    title = "stages won"
  ) %>% 
  gt_theme_nytimes()

## joining all three datasets
tdf_most_joined <- tdf_most %>% 
  left_join(tdf_most_year) %>% 
  left_join(tdf_most_stage)

# > visualization ----

# make table
tab_most <- tdf_most_joined %>% 
  gt() %>% 
  # gtExtras::gt_theme_nytimes() %>% 
  gtExtras::gt_merge_stack(
    col1 = rider,
    col2 = nickname
  ) %>% 
  gtExtras::gt_img_rows(
    columns = country,
    height = 20
  ) %>% 
  gtExtras::gt_fa_repeats(
    column = titles,
    name = "tshirt",
    palette = "orange",
    align = "left"
  ) %>% 
  gtExtras::gt_sparkline(
    timeline,
    range_colors =  c("#67686b","indianred"), 
    line_color = "lightgrey"
  ) %>% 
  gt_plt_bar_stack(
    column = stages, # column with data
    position = "stack", # choose stacked barplot
    labels = c("Mountain stage", "Plain stage", "Time trial"),
    palette = clr_stages,
    width = 60, 
    trim = TRUE 
  ) %>% 
  ## formatting labels
  cols_label(
    rider = "Rider",
    titles = md("Number of titles"),
    country = "Country",
    timeline = md("Titles<br/>timeline")
  ) %>%
  cols_align(
    align = "center",
    columns = c(country,titles)
  ) %>%
  tab_spanner(
    label = "Stages won",
    columns = c(stages)
  ) %>%
  tab_header(
    title = md("Most successful riders in *Tour de France*"),
    subtitle = md("*Tour de France* is a bicycle race event touring across France. The leader is awarded with **the yellow jersey**. The first race was organized in 1903 and within 108 editions **only eight riders have won three or more title**")
  )%>%
  tab_source_note(
    source_note = md("**Data:** Alastair Rushworth & TidyTuesday | **Table:** @rrrizqiramadhan")
  )%>%
  tab_footnote(
    footnote = md("Race not contested from *1915 to 1918* and *1940 to 1946* due to World Wars.<br>Lance Armstrong's wins from 1999 to 2005 were removed due to drugs use, with no alternative winners for those years."),
    locations = cells_title(groups = "subtitle")
  )%>%
  ## Style options
  # Title
  tab_style(
    style = list(
      cell_text(
        font=google_font(name = "Playfair Display"), 
        weight='800',
        align = "left",
        color='#203B46')),
    locations = cells_title(groups = "title")
  )%>%
  # Subtitle
  tab_style(
    style = list(
      cell_text(
        font=google_font(name = "Roboto"), 
        align = "left")
      ),
    locations = cells_title(groups = "subtitle")
  ) %>%
  # Header
  tab_style(
    style = list(
      cell_text(
        font=google_font(name = "Roboto Condensed"), 
        align = "left",
        v_align = "middle",
        color = "black")),
    locations = cells_column_labels(
      columns = c(
        rider)
    )
  )%>%
  tab_style(
    style = list(
      cell_text(
        font=google_font(name = "Roboto Condensed"), 
        align = "center",
        v_align = "middle",
        color = "black")),
    locations = cells_column_labels(
      columns = c(
        country,
        titles,
        timeline)
    )
  )%>%
  tab_style(
    style = list(
      cell_text(
        font=google_font(name = "Roboto Condensed"), 
        align = "center",
        size='small')),
    locations = cells_column_labels(
      columns = c(stages)
    )
  )%>%
  # Spanner
  tab_style(
    style = list(
      cell_text(
        font=google_font(name = "Roboto Condensed"), 
        align = "center"
      )),
    locations = cells_column_spanners()
  ) %>%
  # Body
  tab_style(
    style = list(
      cell_text(font=google_font(name = "Muli"),
                align = 'left'
      )),
    locations = cells_body(
      columns = c(rider, titles, stages)
    )
  )%>%
  # Footnote
  tab_style(
    style = list(
      cell_text(
        font=google_font(name = "Roboto Condensed"),
        style = "italic")),
    locations = cells_footnotes()
  ) %>%
  # source note
  tab_style(
    style = list(
      cell_text(font=google_font(name = "Roboto Condensed"))),
    locations = cells_source_notes()
  )%>%
  # Borders
  tab_options(
    table.border.top.style = "hidden",
    table.border.bottom.style = "hidden",
    # table_body.hlines.color = "lightgrey"
  )

gtsave_extra(
  data = tab_most,
  filename = "Outfile/TidyTuesday_2020_W15_1.png"
)

gtsave_extra(
  data = tab_61,
  filename = "Outfile/TidyTuesday_2020_W15_2.png"
)
