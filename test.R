library(ggplot2)
library(dplyr)
library(scales)
library(RColorBrewer)
library(lubridate)
library(stringr)
library(readr)

nobel <- read_csv('complete.csv')

# Preprocessing

nobel <- mutate(nobel, birth_date = ifelse(birth_date == "1943-00-00", "1943-11-07", birth_date))

nobel$laureate_or_org = if_else(is.na(nobel$knownName), "Organization", "Individual")
nobel$laureate_or_org = factor(nobel$laureate_or_org, levels = c("Individual", "Organization"))

nobel$category = factor(nobel$category, levels = c('Physics', 'Chemistry', 'Physiology or Medicine',
                                                             'Peace', 'Literature', 'Economic Sciences'))

nobel$prizeStatus <- str_to_title(nobel$prizeStatus)
nobel$prizeStatus <- factor(nobel$prizeStatus, levels = c('Received', 'Declined', 'Restricted'))

nobel$gender <- str_to_title(nobel$gender)
nobel$gender <- factor(nobel$gender, levels = c('Male', 'Female'))

nobel$gen_org <- if_else(nobel$gender == 'Male', "Male", "Female", "Organization")
nobel$gen_org <- factor(nobel$gen_org, levels = c("Male", "Female", "Organization"))

nobel <- nobel %>%
  mutate(awardDate = as.Date(ISOdate(awardYear, 10, 01)), # approximating 
         age_days = ifelse(birth_date == "", "unnknown", awardDate - as.Date(birth_date)),
         age_years = round(interval(as.Date(birth_date), awardDate) / years(1)), 
         life_span_days = as.Date(death_date) - as.Date(birth_date), 
         life_span_years = round(interval(as.Date(birth_date), death_date) / years(1)), 
         is_alive = laureate_or_org == 'Laureate' & is.na(death_date))

 # Visualisations - from lectures

# Plot 1

nobel$awardYear_cut <- cut(nobel$awardYear, 10, dig.lab=4)

nobel_no_year_gender <- nobel %>% 
  group_by(awardYear_cut, gender) %>% 
  summarise(number = length(awardYear_cut))

ggplot(data = nobel_no_year_gender, aes(x = awardYear_cut, y = number, fill = gender)) +
  geom_bar(stat = 'identity', color = 'darkgreen') +
  theme_minimal() 

# solution - adjust the geom_label:
ggplot(data = nobel_no_year_gender, aes(x = awardYear_cut, y = number, fill = factor(gender))) +
  geom_bar(na.rm = TRUE, position = "stack", width = 0.5, stat = "identity") +
  ggtitle('Gender and age in the sample') +
  xlab('Age') +
  ylab('Population') +
  labs(fill = 'Gender') +
  theme_minimal() +
  scale_fill_manual(values = c('grey78', 'khaki', 'blue')) +
  geom_label(aes(y = number, label = number), fill = 'white', position = "identity",
             vjust = 0.5, color = "black", size = 3) +
  theme(axis.text.x = element_text(angle=45, hjust=1))

# Plot 2 - let's look into the last decade more closely. Adjust geom_label (again)

last_decade_no <- nobel %>% 
  group_by(awardYear, gender) %>% 
  summarise(number = length(awardYear))

last_decade_no_adj <- last_decade_no %>% 
  arrange(awardYear, desc(gender)) %>% 
  group_by(awardYear) %>% 
  mutate(label_sum = cumsum(number))

final <- last_decade_no_adj %>% 
  mutate(label_sum2 = label_sum*.5)

ggplot(data = final, aes(x = factor(awardYear), y = number, fill = factor(gender))) +
  geom_bar(na.rm = TRUE, position = "stack", width = 0.7, stat = "identity") +
  ggtitle('Gender and age in the sample') +
  xlab('Age') +
  ylab('Population') +
  labs(fill = 'Gender') +
  theme_minimal() +
  geom_label(aes(y = label_sum2, label = number), fill = 'white', position = position_stack(),
             vjust = 0.5, color = "black", size = 3) +
  scale_x_discrete(limits = as.character(seq(2008, 2019, 1)))

# Plot 3 - raczej useless

mean_no_of_nobels_men <- mean(final[final$gender == "Male", ]$label_sum, na.rm=T)
mean_no_of_nobels_women <- mean(final[final$gender == "Female", ]$label_sum, na.rm=T)

ggplot(data = final, aes(x = awardYear, y = label_sum, group = factor(gender))) +
  geom_line(aes(linetype = factor(gender), color = factor(gender)), size = 2) +
  geom_point(size = 3, aes(shape = factor(gender))) +
  theme_minimal() +
  geom_line(aes(y=mean_no_of_nobels_men), color = "blue") +
  geom_line(aes(y=mean_no_of_nobels_women), color = "red") +
  ylab("Number of Nobel Prizes") +
  xlab("Year")

# Plot 4 - usage of ggplot extensions
library(ggrepel)

unique_categories <- unique(nobel$awardYear_cut)

nobel_no_year_category <- nobel %>% 
  group_by(awardYear_cut, category) %>% 
  summarise(number = length(awardYear_cut)) %>% 
  mutate(label = if_else(awardYear_cut == levels(nobel$awardYear_cut)[length(unique_categories <- unique(nobel$awardYear_cut))],
                         as.character(category),
                         NA_character_))

nobel_no_year_category %>%
  ggplot(aes(x = awardYear_cut, y = number, group = category, colour = category)) + 
  geom_line() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  geom_label_repel(aes(label = label,
                 na.rm = TRUE)) +
  xlab("Decades") +
  ylab("Number of Nobel Prizes") +
  ggtitle("Nobel prizes based on categories")

# Plot 5 - table

nobel.agg <- nobel %>%
  mutate(age_years_cut = cut(age_years, 4)) %>% 
  count(age_years_cut, category)

ggplot(nobel.agg, aes(x = age_years_cut, y = category)) + # nowa baza danych
  geom_tile(aes(fill = n), color = 'black', show.legend = F) +
  theme_minimal() + 
  geom_text(aes(label = n), size = 5, fontface = 'bold', color = 'white') +
  labs(title = 'Dispersion of Nobel Prizes',
       x = "Age",
       y = "Category") +
  scale_fill_gradient(low = 'green3', high = 'orange')

# Plot 6 - interactive (https://rpubs.com/abigailchen/nobelprize)
library(gganimate)
ggplot(nobel, aes(awardYear, age_years, group = awardYear, fill = age_years)) +
  xlab("Prize year") +
  ylab("Age") +
  labs(fill = "Age", group = "Prize Year", title = "Laureate Age across the Years") +
  geom_boxplot() +
  theme(legend.position = "top", panel.background = element_rect(fill = NA),
        panel.border = element_blank(), axis.text=element_text(size=8), 
        plot.title = element_text(size = 12L, face = "bold", hjust = 0.5) ) +
  # scale_fill_viridis() +
  transition_reveal(awardYear)+ 
  # scale_colour_wsj("colors6") +
  enter_grow() +
  enter_fade() +
  ease_aes("back-in")

#Plot 7 - Nobel Prize Winners'Historical Average Age
# https://medium.com/@TechTeamMarketFinance/exploring-nobel-prize-winners-with-r-b636875ee28f

avreage_age_nobel <- nobel %>%  
  group_by(category, awardYear) %>% 
  summarize (avg_age = mean(age_years, na.rm = TRUE))

ggplot(avreage_age_nobel, aes(x = awardYear, y = avg_age) ) +
  geom_point() + geom_smooth(method = "lm") + facet_wrap(~ category) +
  geom_hline(data=filter(avreage_age_nobel, category =="Chemistry"), aes(yintercept= 55.6), 
             colour="red", linetype = 'dashed') +
  geom_hline(data=filter(avreage_age_nobel, category =="Economic Sciences"), aes(yintercept= 63.3), 
             colour="red", linetype = 'dashed') +
  geom_hline(data=filter(avreage_age_nobel, category =="Literature"), aes(yintercept= 62.6), 
             colour="red", linetype = 'dashed') +
  geom_hline(data=filter(avreage_age_nobel, category =="Physiology or Medicine"), aes(yintercept= 55.5), 
             colour="red", linetype = 'dashed') +
  geom_hline(data=filter(avreage_age_nobel, category =="Peace"), aes(yintercept= 58.7), 
             colour="red", linetype = 'dashed') +
  geom_hline(data=filter(avreage_age_nobel, category =="Physics"), aes(yintercept= 53), 
             colour="red", linetype = 'dashed') +
  labs(title = "Nobel Prize Winners'Historical Average Age") +
  xlab("Year") +
  ylab("Average Age") +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 15, face = "bold"))

table(nobel$category)

# Plot 8- Distribution of Nobel Prizes over time by category
# https://medium.com/@TechTeamMarketFinance/exploring-nobel-prize-winners-with-r-b636875ee28f
ggplot(nobel, aes(x = awardYear, y = gender, col = gender)) +
  geom_point(position = "jitter") + facet_grid(category ~ .) + 
  labs(title = "Distribution of Nobel Prizes over time by category") +
  xlab("Assigned Prize Year") +
  ylab("Gender / Type") +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 15, face = "bold"))

# NA - it is organization probably
