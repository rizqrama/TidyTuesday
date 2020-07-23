# load library ----
library(tidyverse)
library(here)
library(readr)
library(janitor)
library(lubridate)
library(zoo)
library(ggtext)
library(gganimate)

# import data set ----
tuesdata <- tidytuesdayR::tt_load(2020, week = 30)

animal_complaints <- tuesdata$animal_complaints %>% clean_names()

rm(tuesdata) # removing tuesdata

# Main Idea ----
# I want to know when the animals are reported most for wandering
# and which animal is most reported


# Animal Complaints Data ----

animal_wandering <- animal_complaints %>% 
  # Pull out the years/months from date_received column and convert it to Date format
  mutate(year = parse_number(`date_received`), # separating year "number"
         month=str_extract(`date_received`, "[A-Za-z]+" ), # extract month as character utilizing regular expressions (selecting all lowercase and uppercase characters)
         mth = match(month, month.name), # assigning month name with month number
         mth_yr = paste0(mth,"/",year), # concatenating month and year column
         mth_yr = zoo::as.yearmon(mth_yr, "%m/%Y"), # converting into yearmon type
         date = paste0("1","/",mth, "/", year), # concatenating month and year column and adding artificial date
         date = lubridate::dmy(date)) %>% # converting into Date type
  
  # count monthly cases of complaints and select only the wandering reports
  group_by(date, mth_yr, animal_type, complaint_type) %>% # group complaints based on year, month, animal types, and complaint types
  count(name = "n_animals") %>% # count monthly complaints type
  filter(complaint_type == "Wandering") # filtering the wandering reports
  
  # count the relative percentages
  wander_pct <- animal_wandering %>% 
    group_by(mth_yr) %>% 
    mutate(total = sum(n),
           percent = 100*n/total, 
           total_pct = sum(percent),
           percent = round(percent))

  # Plotting Time! ----
  
  # Plot A: Standard ----
  plot_a <-  animal_wandering %>% 
    # data source and begin plotting
    ggplot(aes(x = mth_yr, y = n_animals, color = animal_type, group = animal_type, shape = animal_type)) +
    # geoms
    geom_point(size = 3) +
    geom_line(size = 1) +
    geom_textbox(
      inherit.aes = F,
      data = tibble(x = 2015.3,
                    y = 121,
                    label = "<b style='font-size:18pt;'>Animal Crossing in Townsville City,</b><br><b style='font-size:18pt;'>Queensland, Australia</b><br><br> <b style ='color:#004C56'>Wandering Dogs</b> are more likely to be reported than <b style ='color:#f4841a'>Wandering Cats</b>."),
      aes(x = x, y = y, label = label),
      fill = NA,
      box.color = NA,
      size = 3,
      width = unit(4.5, "inch")
    ) +
    
    # scales
    scale_shape_manual(values = c(19, 15)) +
    scale_color_manual(values = c("#f4841a", "#004C56")) +
    scale_y_continuous(
      breaks = c(0, 20, 40, 60, 80, 100, 120),
      limits = c(0, 130)
    ) +
    
    # themes and labels
    theme_minimal() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      axis.text.y = element_text(color = "grey50", face = "bold"),
      aspect.ratio = 0.75
    ) +
    labs(x = "", y = "",
         caption = "plotted by Rama. Source: Townsville City Council Animal Complaints")

  # save the plot
ggsave("animal_crossing.png", width = 361.421, height = 203.2, units = "mm", dpi =300)

      

# Plot B: Animated ----
  plot_b <-  animal_wandering %>% 
    # data source and begin plotting
    ggplot(aes(x = date, y = n_animals, color = animal_type, group = animal_type)) +
    # geoms
    geom_point() +
    geom_line() +
    ggtitle("Reported Wandering Pets in Townsville, Australia") +
    xlab("Year") + ylab("") +
    transition_reveal(date) +
     # scales
    scale_color_manual(values = c("#f4841a", "#004C56")) +

    # themes and labels
    theme_minimal() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      axis.text.x = element_text(face = "bold",angle = 40),
      axis.text.y = element_text(color = "grey50", face = "bold")
    ) +
    labs(x = "", y = "", subtitle = "Between October 2013 - June 2020", color = "Animal")
    
  
  # save as gif  
  animate(plot_b,width=1000,height=1000,res=170,renderer=gifski_renderer("animated_wander.gif"))
  
  