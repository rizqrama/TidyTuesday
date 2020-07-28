# ~ Week 31_2020/07/28: Penguins! ~ -----

# Loading packages ----
library(tidyverse)
library(here)
library(readr)
library(ggthemes)
library(ggtext)

# Load dataset ----
penguins <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-28/penguins.csv')
penguins <- penguins %>% 
  mutate(mass_kg = body_mass_g/1000)
## I just wanna be simple this week


# Plotting ----
penguinsplot <- penguins %>% 
  # base data
  ggplot(aes(x = flipper_length_mm, y = bill_length_mm, color = species, fill = species, size = mass_kg)) +
  
  # geoms
  geom_point(alpha = 0.7) +
  geom_textbox(
    inherit.aes = F,
    data = tibble(x =183, 
                  y = 64,
                  label = "<b style='font-size:20pt;'>Palmer Penguins!</b><br> 2007 - 2009 Physical Measurements Data for <b style ='color:#B8E3EA'>Adelie</b>, <b style ='color:#A9BCC6'>Chinstrap</b>, and <b style ='color:#02334A'>Gentoo</b> Penguins at Palmer Station LTER."),
    aes(x = x, y = y, label = label),
    fill = NA,
    box.color = NA,
    size = 4,
    width = unit(4.5, "inch")
  ) +
  
  # labels
  labs(
    x = "Flipper Length (mm)",
    y = "Bill Length (mm)",
    caption = "Source: Gorman, Williams and Fraser, 2014. Visualization: @rizqrama"
  ) +
  
  # scales
  scale_fill_manual(values = c("#B8E3EA", "#A9BCC6", "#02334A"), guide = FALSE) +
  scale_color_manual(values = c("#B8E3EA", "#A9BCC6", "#02334A"), guide = FALSE) +
  scale_size(range = c(4, 10), name="Body Mass (kg)") +
  scale_y_continuous(
    breaks = c(40, 50, 60),
    limits = c(32, 65)
  ) +
  
  # themes
  theme_economist_white() +
  theme(
    panel.grid.minor = element_blank(),
    plot.caption = element_text(size = 8, face = "italic"),
    axis.text.x = element_text(color = "grey50", face = "bold"),
    axis.text.y = element_text(color = "grey50", face = "bold"),
    legend.position = "bottom",
    legend.justification = c("right", "bottom"),
    legend.box.just = "bottom",
    legend.margin = margin(6, 6, 6, 6),
    legend.text = element_text(size = 8, colour = "grey50"),
    legend.title = element_text(size = 8, colour = "grey50"),
    aspect.ratio = 203.2/361.421)
  

ggsave("penguins.png", width = 361.421, height = 203.2, units = "mm", dpi =320)
