library(tidyverse)
library(tidyr)
library(lubridate)
library(ggplot2)
library(forcats)
library(ggrepel)
library(plyr)
library(dplyr)
library(tidytext)

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

nobel_data$gender <- str_to_title(nobel_data$gender)
nobel_data$gender <- factor(nobel_data$gender, levels = c('Male', 'Female'))

nobel_data$prizeStatus <- factor(nobel_data$prizeStatus, levels = c('Received', 'Declined', 'Restricted'))


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
  select(awardYear, category, age_days, age_years, name)

ggplot(nobel_data %>% drop_na(age_years),
       aes(x = awardYear, y = age_years)) +
  geom_point(aes(colour = ifelse(age_years <= 30 | age_years >= 90, "red", "#d1c700")),
             size = 4,
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
ggsave("plots/plot1.png", width = 12, height = 9, dpi=150, bg='white')
  
  
# 2nd plot (Age of Nobel Laureates over the years) - Szymon ----

median_age <- ddply(age_data, .(category), summarise, median = median(age_years))

ggplot(transform(age_data, category=factor(category, levels = c("Chemistry", "Economic Sciences", "Literature", "Peace", "Physics", "Physiology or Medicine"))), aes(x = awardYear, y = age_years, col = category)) +
  geom_point(size = 4, alpha= 0.7) +
  geom_smooth(formula = y ~ x, method = 'loess', se = FALSE, size = 2) +
  labs(title = 'Age of Nobel Laureates over the years', x = 'Year', y = 'Age') +
  facet_wrap(vars(category), nrow = 2) + 
  theme(legend.position = "none",
        panel.spacing.x = unit(1.5, "lines")) + 
  scale_colour_brewer(palette="Paired") +
  geom_hline(aes(yintercept = median, colour = category), data = median_age, linetype = "dashed", size = 1) +
  geom_text_repel(aes(label=ifelse(age_years<30 | age_years > 85, as.character(nobel_data$knownName), "")),
                  size=5,
                  family = "IBM Plex Serif",
                  colour = "black")
ggsave("plots/plot2.png", width = 12, height = 9, dpi=150, bg='white')


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
ggsave("plots/plot3.png", width = 12, height = 9, dpi=150, bg='white')


# 4th plot - Award category distribution - Artur ----

nobel_gender_perc <- 
  nobel_data %>% 
  mutate(category=factor(category, levels = c("Chemistry", "Economic Sciences", "Literature", "Peace", "Physics", "Physiology or Medicine"))) %>% 
  group_by(category) %>% 
  tally() %>% 
  dplyr::rename(count = n) %>% 
  mutate(perc_n = 100*round(count/sum(count), 3)) %>% 
  unnest(cols = c()) %>% 
  arrange(desc(category)) %>%
  mutate(lab.ypos = cumsum(perc_n) - 0.5*perc_n)

# Make the plot
#mycols <- c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF", "lightgoldenrod4", "hotpink1")

ggplot(nobel_gender_perc, aes(x = 2, y = perc_n, fill = category)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = lab.ypos, label = paste0(perc_n, "%")), color = "black", size = 10, family = "IBM Plex Serif")+
  scale_fill_brewer(palette="Paired") +
  labs(x = '', y = '', title = "Award category distribution", fill = 'Category') + 
  #scale_fill_manual(values = mycols) +
  #theme_void()+
  theme(legend.position=c(0.12, 0.25),
        legend.justification = "top",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()
  ) + 
  xlim(0.5, 2.5)
ggsave("plots/plot4.png", width = 12, height = 9, dpi=150, bg='white')


# 5th plot - Artur ----

nobel_no_category_gender <- nobel_data %>%
  filter(!gender == "") %>% 
  group_by(category, gender) %>% 
  dplyr::summarise(number = length(category)) %>% 
  unnest(cols = c())

ggplot(data = nobel_no_category_gender, aes(x = reorder(category, -number), y = number, fill = factor(gender))) +
  geom_bar(na.rm = TRUE, position = "stack", width = 0.5, stat = "identity") +
  ggtitle('Gender and age in the sample') +
  xlab('Category') +
  ylab('Population') +
  labs(fill = 'Gender') +
  #theme_minimal() +
  scale_fill_manual(values = c('grey78', 'khaki')) +
  geom_label(aes(y = number, label = number), fill = 'white', position = "identity",
             vjust = 0.5, color = "black", size = 5, family = "IBM Plex Serif") +
  scale_x_discrete(labels = function(x) 
    stringr::str_wrap(x, width = 15)) +
  theme(legend.position="bottom",
        legend.direction="horizontal"
  ) 
ggsave("plots/plot5.png", width = 12, height = 9, dpi=150, bg='white')


# 6th plot - Artur ----

nobel_data$awardYear_cut <- nobel_data$awardYear_cut <- cut(x=nobel_data$awardDate, 
                                                            breaks="10 years",
                                                            labels = c("1901-1910",
                                                                       "1911-1920",
                                                                       "1921-1930",
                                                                       "1931-1940",
                                                                       "1941-1950",
                                                                       "1951-1960",
                                                                       "1961-1970",
                                                                       "1971-1980",
                                                                       "1981-1990",
                                                                       "1991-2000",
                                                                       "2001-2010",
                                                                       "2011-2019"))
unique_categories <- unique(nobel_data$awardYear_cut)

nobel_no_year_category <- nobel_data %>% 
  group_by(awardYear_cut, category) %>% 
  dplyr::summarise(number = length(awardYear_cut)) %>% 
  mutate(label = if_else(awardYear_cut == levels(nobel_data$awardYear_cut)[length(unique_categories <- unique(nobel_data$awardYear_cut))],
                         as.character(category),
                         NA_character_))

nobel_no_year_category %>%
  ggplot(aes(x = awardYear_cut, y = number, group = category, colour = category)) + 
  geom_line(size = 2) +
  theme(axis.text.x = element_text(angle=45, hjust=1, size = 25)) +
  geom_label_repel(aes(label = label,
                       nudge_x = 2019,
                       na.rm = TRUE),
                   size=8,
                   family = "IBM Plex Serif") +
  labs(x = 'Decades', y = 'Number of Nobel Prizes', title = "Nobel prizes based on categories", color = 'Category') +
  scale_color_brewer(palette="Paired") +
  theme(legend.position = "none")
ggsave("plots/plot6.png", width = 12, height = 9, dpi=150, bg='white')


# 7th plot - Artur - Nobel prize distribution by age and category - usage of tables from ggplot - ----

nobel.agg <- nobel_data %>%
  dplyr::mutate(age_years_cut = cut(age_years, 
                             breaks = 4,
                             labels = c('17-37', '38-57', '58-77', '78-97')),
         age_years_cut = forcats::fct_explicit_na(age_years_cut, paste0('Unknown -\n organization'))) %>% 
  dplyr::count(age_years_cut, category)

ggplot(nobel.agg, aes(x = age_years_cut, y = category)) + # nowa baza danych
  geom_tile(aes(fill = n), color = 'black', show.legend = F) +
  #theme_minimal() + 
  geom_text(aes(label = n), size = 8, fontface = 'bold', color = 'black', family = "IBM Plex Serif") +
  labs(title = 'Dispersion of Nobel Prizes',
       x = "Age",
       y = "Category") +
  scale_fill_gradient(low = 'green3', high = 'orange')
ggsave("plots/plot7.png", width = 12, height = 9, dpi=150, bg='white')


# 8th plot - Artur - number of laureates share the award ----

# nobel_prize_share <- nobel_data %>%  
#   group_by(category, awardYear) %>% 
#   dplyr::summarise(number = n()) %>% 
#   ungroup 
# 
# ggplot(nobel_prize_share, aes(x = awardYear, y = category, col = factor(number))) +
#   geom_point(size = 2, fill = 'black') + 
#   scale_x_continuous(limits = c(1899, 2021), breaks = seq(1900, 2020, 8), expand = c(0, 0)) +
#   labs(title='No. of laureates shared the award', x = '', y='') +
#   theme(legend.title = element_blank(), legend.position='top', legend.justification = "left")


# 9th plot - Artur - Birth continent ----

nobel_prize_share_by_continent <- nobel_data %>%
  filter(birth_continent %in% c("Europe", "North America", "Asia", "Africa", "Oceania", "South America")) %>% 
  drop_na(birth_continent) %>%
  group_by(birth_continent) %>%
  dplyr::summarise(number = n())

ggplot(nobel_prize_share_by_continent, aes(reorder(birth_continent, number), number, fill = birth_continent)) +
  geom_col() +
  labs(title = 'Birth Continent', x = '', y = 'Number of Nobel laureates') +  
  geom_text(aes(label = number), hjust = -0.3, colour = "#555555", size = 6, fontface = "bold", family = "IBM Plex Serif") +
  scale_y_continuous(limits = c(0, 520)) +
  coord_flip() +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position = 'none',
        panel.spacing.x = unit(1.5, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
ggsave("plots/plot9.png", width = 12, height = 9, dpi=150, bg='white')


# 10th plot - Artur - Birth continent ----

nobel_data$motivation<-as.character(nobel_data$motivation)

motivation_words <- nobel_data %>%
  select(category, motivation) %>%
  drop_na(motivation) %>%
  unnest_tokens(bigram, motivation, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigram_filtr_and_unite <- motivation_words %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  filter(word1 != word2) %>%
  unite(bigram, word1, word2, sep = " ")

bigrams_to_plot <- bigram_filtr_and_unite %>% 
  group_by(category, bigram) %>%
  dplyr::summarize(n = n()) %>% 
  top_n(5, n) %>%
  arrange(category, desc(n)) %>% 
  dplyr::mutate(row_number_in_group = row_number()) %>%  
  filter(row_number_in_group %in% seq(1,5)) %>% 
  ungroup %>%
  dplyr::mutate(word = reorder_within(bigram, n, category)) 

ggplot(transform(bigrams_to_plot, category=factor(category, levels = c("Chemistry", "Economic Sciences", "Literature", "Peace", "Physics", "Physiology or Medicine"))), aes(x= reorder(word, n), y = n, fill = category)) +
  geom_col() +
  geom_text(aes(label = n), hjust = 1.2, colour = "white", size = 7, fontface = 'bold', family = "IBM Plex Serif") +
  labs(title = 'Most 5 frequent words in each category', x = '', y = '', fill = '') +
  coord_flip() + 
  theme(legend.position = 'none',
        panel.spacing.x = unit(1.5, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        strip.text.x = element_text(size = 15)) +
  scale_x_reordered() +
  facet_wrap(vars(category), scales = 'free') +
  scale_fill_brewer(palette="Paired")
ggsave("plots/plot10.png", width = 12, height = 9, dpi=150, bg='white')
