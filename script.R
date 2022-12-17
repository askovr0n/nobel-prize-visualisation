library(tidyverse)
library(tidyr)
library(lubridate)
library(ggplot2)
library(ggrepel)
library(plyr)

#install.packages('showtext', dependencies = TRUE)
library(showtext)
font_add_google("IBM Plex Serif", "IBM Plex Serif") # https://fonts.google.com/
#font_families()
showtext_auto()


# Data ----

nobel_data <- read.csv('data/nobel_data.csv')


# Settings ----

## ggplot custom theme ----

# nobel_price_custom_theme <- theme_minimal(base_size = 10) + 
#   theme(plot.title = element_text(face="bold", family="Quicksand"),
#         text = element_text(family="Quicksand"))

theme_set(theme_minimal(#base_size = 10
  ) + 
            theme(plot.title = element_text(face="bold", family="IBM Plex Serif", size = 50),
                  text = element_text(family="IBM Plex Serif", size = 20),
                  legend.title=element_text(size=20, face="bold"),
                  axis.text=element_text(size=20),
                  legend.text=element_text(size=20),
                  axis.title=element_text(size=20, face="bold"),
                  strip.text.x = element_text(size = 25),
                  legend.direction = "vertical",
                  legend.box = "vertical",
            )
)


## color ----

nobelgold = "#d1c700" # gold

# male =  "#97e5ef" # blue
# female = "#efa8e4" # pink
# 
# ind = '#ffa5b0' # smoke
# org = '#45046a' # purple
# 
# rec = '#b9cced' # light blue-violet
# dec = '#f64b3c' # red
# res = '#434e52' # black
# 
# phy = "#fe91ca" # light pink
# che = "#7fdbda" # light cyan
# med = "#ade498" # light green
# pea = "#ede682" # light yellow
# lit = "#abc2e8" # light blue
# ecn = "#febf63" # light orange
# 
# ## palette ----
# 
# # category palette
# cat_pal <- c(phy, che ,med ,pea ,lit ,ecn)
# names(cat_pal) <- c('Physics', 'Chemistry', 'Physiology or Medicine',
#                     'Peace', 'Literature', 'Economic Sciences')


# Pre processing ----

nobel_data <- mutate(nobel_data, birth_date = ifelse(birth_date == "1943-00-00", "1943-11-07", birth_date))

nobel_data$laureate_or_org = if_else(is.na(nobel_data$knownName), "Organization", "Individual")
nobel_data$laureate_or_org = factor(nobel_data$laureate_or_org, levels = c("Individual", "Organization"))

nobel_data$category = factor(nobel_data$category, levels = c('Physics', 'Chemistry', 'Physiology or Medicine',
                                             'Peace', 'Literature', 'Economic Sciences'))

nobel_data$prizeStatus <- str_to_title(nobel_data$prizeStatus)
nobel_data$prizeStatus <- factor(nobel_data$prizeStatus, levels = c('Received', 'Declined', 'Restricted'))

nobel_data <- nobel_data %>%
  mutate(awardDate = as.Date(ISOdate(awardYear, 10, 01)), # approximating 
         age_days = ifelse(birth_date == "", "unnknown", awardDate - as.Date(birth_date)),
         age_years = round(interval(as.Date(birth_date), awardDate) / years(1)), 
         life_span_days = as.Date(death_date) - as.Date(birth_date), 
         life_span_years = round(interval(as.Date(birth_date), death_date) / years(1)), 
         is_alive = laureate_or_org == 'Laureate' & is.na(death_date))



# 1st plot (Age of laureates at the time of receiving the Nobel Prize) - Szymon ----

age_data <- nobel_data %>% 
  drop_na(age_years) %>%
  select(awardYear, category, age_days, age_years, name)

ggplot(nobel_data %>% drop_na(age_years),
       aes(x = awardYear, y = age_years)) +
  geom_point(aes(colour = ifelse(age_years <= 30 | age_years >= 90, "red", "#d1c700")),
             size = 5,
             alpha = 0.5) + 
  geom_smooth(formula = y ~ x, method = 'loess', se = FALSE, size = 2, color = "#3d3d3d",
              alpha=0.1) +
  scale_color_manual(name = "Age",
                     values = c(nobelgold, "red"),
                     labels = c("between 30 and 90", "less than 30 or more than 90")) +
  labs(title = 'Age of Nobel laureates', 
       x = 'Year', 
       y = 'Age', 
       col='') +
  geom_text_repel(aes(label = ifelse(age_years <= 30 | age_years >= 90, as.character(name), "")),
                  size = 10,
                  family = "IBM Plex Serif") +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal")
ggsave("plots/plot1.png", width = 16, height = 9, dpi=150, bg='white')
  
  
# 2nd plot (Age of Nobel Laureates over the years) - Szymon ----

median_age <- ddply(age_data, .(category), summarise, median = median(age_years))

ggplot(transform(age_data, category=factor(category, levels = c("Chemistry", "Economic Sciences", "Literature", "Peace", "Physics", "Physiology or Medicine"))), aes(x = awardYear, y = age_years, col = category)) +
  geom_point(size = 4, alpha= 0.7) +
  geom_smooth(formula = y ~ x, method = 'loess', se = FALSE, size = 2) +
  labs(title = 'Age of Nobel Laureates over the years', x = 'Year', y = 'Age') +
  facet_wrap(vars(category), nrow = 2) + 
  theme(legend.position = "none") + 
  scale_colour_brewer(palette="Paired") +
  geom_hline(aes(yintercept = median, colour = category), data = median_age, linetype = "dashed", size = 1)
ggsave("plots/plot2.png", width = 16, height = 9, dpi=150, bg='white')


# 3rd plot - Szymon ----

country_count <- nobel_data %>%
  drop_na(birth_countryNow) %>%
  select(birth_countryNow) %>%
  group_by(birth_countryNow) %>%
  mutate(birth_countryNow = str_replace(birth_countryNow, 'United Kingdom', 'UK'), 
         birth_countryNow = str_replace(birth_countryNow, 'Scotland', 'UK'),            
         birth_countryNow = str_replace(birth_countryNow, 'Northern Ireland', 'Ireland'), 
         birth_countryNow = str_replace(birth_countryNow, 'the Netherlands', 'Netherlands')) %>%
  dplyr::summarise(n = n()) %>%
  mutate(m = case_when(n > 100  ~ "100 +",
                       n < 100 & n >= 50 ~ "99 - 50",
                       n < 50 & n >= 20 ~ "49 - 20",
                       n < 20 & n >= 10 ~ "19 - 10",
                       n < 10  ~ "< 10")) %>%
  mutate(m = factor(m, levels = c("< 10", "19 - 10", "49 - 20", "99 - 50", "100 +")))

map_data <- country_count %>% 
  full_join(map_data("world"), by = c('birth_countryNow' = 'region')) 

ggplot(map_data, aes(x = long, y = lat, group = group, fill = m)) +
  geom_polygon(colour = "#616161") + 
  labs(title = 'Number of laureates born in a given country', x = '', y = '', fill = 'Number') +
  scale_fill_brewer(palette="OrRd", na.value = 'lightgray') +
  guides(fill = guide_legend(reverse = TRUE))+
  theme(legend.position=c(0.12, 0.37),
        legend.justification = "top",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()
  )
ggsave("plots/plot3.png", width = 16, height = 9, dpi=150, bg='white')


# 4th plot - Artur ----

# 5th plot - Artur ----

