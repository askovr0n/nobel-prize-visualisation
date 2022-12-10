library(tidyverse)
library(tidyr)
library(lubridate)
library(ggplot2)

nobel_data <- read.csv('data/nobel_data.csv')


# Pre processing

nobel_data <- mutate(nobel_data, birth_date = ifelse(birth_date == "1943-00-00", "1943-11-07", birth_date))

nobel_data$laureate_or_org = if_else(is.na(nobel_data$knownName), "Organization", "Individual")
nobel_data$laureate_or_org = factor(nobel_data$laureate_or_org, levels = c("Individual", "Organization"))

nobel_data$category = factor(nobel_data$category, levels = c('Physics', 'Chemistry', 'Physiology or Medicine',
                                             'Peace', 'Literature', 'Economic Sciences'))

nobel_data$prizeStatus <- str_to_title(nobel_data$prizeStatus)
nobel_data$prizeStatus <- factor(nobel_data$prizeStatus, levels = c('Received', 'Declined', 'Restricted'))

nobel_data$gender <- str_to_title(nobel_data$gender)
nobel_data$gender <- factor(nobel_data$gender, levels = c('Male', 'Female'))

nobel_data$gen_org <- if_else(nobel_data$gender == 'Male', "Male", "Female", "Organization")
nobel_data$gen_org <- factor(nobel_data$gen_org, levels = c("Male", "Female", "Organization"))

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
  select(awardYear, category, gender, age_days, age_years, name)


age_stats %>%
  drop_na(age_years) %>%
  group_by(category) %>%
  summarize(mean_age = mean(age_years), 
            median_age = median(age_years))


ggplot(age_data, aes(x = awardYear, y = age_years, col = cut(age_years, 6))) +
  geom_point(size = 7, alpha = 0.5) + 
  #     scale_x_continuous(limits = c(1899, 2021), breaks = seq(1900, 2020, 20), expand = c(0, 0)) +
  #     scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20), expand = c(0, 0)) +
  labs(title = 'Age of laureates at the time of receiving the Nobel Prize', x = '', y = '', col='') +
  #geom_text_repel(aes(label = ifelse(age_years <= 30 | age_years >= 90, as.character(name), "")), size = 6) +
  theme(legend.position='top', legend.justification = "left") + 
  guides(colour = guide_legend(nrow = 1))


# 2nd plot - Artur ----

# 3rd plot - Szymon ----

# 4rd plot - Artur ----