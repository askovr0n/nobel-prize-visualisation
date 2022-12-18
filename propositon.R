# Proposition of story

library(ggplot2)
library(dplyr)
library(scales)
library(RColorBrewer)
library(lubridate)
library(stringr)
library(readr)
library(tidyr)

nobel <- read_csv('complete.csv')

nobel_data <- read_csv('complete.csv')

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


# Nobel prize distribution by category

# Load ggplot2
library(ggplot2)
library(forcats)


# Compute percentages
nobel_gender_perc <- 
  nobel %>% 
  group_by(category) %>% 
  tally() %>% 
  rename(count = n) %>% 
  mutate(perc_n = 100*round(count/sum(count), 3)) %>% 
  unnest() %>% 
  arrange(desc(category)) %>%
  mutate(lab.ypos = cumsum(perc_n) - 0.5*perc_n)

# Make the plot
mycols <- c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF", "lightgoldenrod4", "hotpink1")

ggplot(nobel_gender_perc, aes(x = 2, y = perc_n, fill = category)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = lab.ypos, label = paste0(perc_n, "%")), color = "white")+
  scale_fill_manual(values = mycols) +
  theme_void()+
  xlim(0.5, 2.5)

# 2. Nobel prize distribution by category and gender

nobel_no_category_gender <- nobel %>%
  filter(!gender == "") %>% 
  group_by(category, gender) %>% 
  summarise(number = length(category)) %>% 
  unnest()

ggplot(data = nobel_no_category_gender, aes(x = category, y = number, fill = factor(gender))) +
  geom_bar(na.rm = TRUE, position = "stack", width = 0.5, stat = "identity") +
  ggtitle('Gender and age in the sample') +
  xlab('Category') +
  ylab('Population') +
  labs(fill = 'Gender') +
  theme_minimal() +
  scale_fill_manual(values = c('grey78', 'khaki')) +
  geom_label(aes(y = number, label = number), fill = 'white', position = "identity",
             vjust = 0.5, color = "black", size = 3) +
  scale_x_discrete(labels = function(x) 
    stringr::str_wrap(x, width = 15))

# 3. Nobel prize distribution by year and category - usage of gg extensions

library(ggrepel)

nobel$awardYear_cut <- nobel$awardYear_cut <- cut(nobel$awardDate, 
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
                       nudge_x = 2019,
                       na.rm = TRUE)) +
  xlab("Decades") +
  ylab("Number of Nobel Prizes") +
  ggtitle("Nobel prizes based on categories")

# 4. Nobel prize distribution by age and category - usage of tables from ggplot
nobel.agg <- nobel %>%
  mutate(age_years_cut = cut(age_years, 
                             breaks = 4,
                             labels = c('17-37', '38-57', '58-77', '78-97')),
         age_years_cut = forcats::fct_explicit_na(age_years_cut, paste0('Unknown -\n organization'))) %>% 
  count(age_years_cut, category)

ggplot(nobel.agg, aes(x = age_years_cut, y = category)) + # nowa baza danych
  geom_tile(aes(fill = n), color = 'black', show.legend = F) +
  theme_minimal() + 
  geom_text(aes(label = n), size = 5, fontface = 'bold', color = 'white') +
  labs(title = 'Dispersion of Nobel Prizes',
       x = "Age",
       y = "Category") +
  scale_fill_gradient(low = 'green3', high = 'orange')

# Plot 5 - Distribution of Nobel Prizes over time and age by category
# Na podstawaie: 
# https://medium.com/@TechTeamMarketFinance/exploring-nobel-prize-winners-with-r-b636875ee28f

average_age_nobel <- nobel %>%  
  group_by(category, awardYear) %>% 
  summarize(avg_age = mean(age_years, na.rm = TRUE)) %>% 
  unnest()

average_age_nobel_with_names <- left_join(nobel %>% select(category, awardYear, knownName, age_years), 
                                          average_age_nobel, 
                                          by = c('category' = 'category', 'awardYear' = 'awardYear'))

ggplot(average_age_nobel_with_names, aes(x = awardYear, y = age_years, color=category)) +
  geom_point() + 
  facet_wrap(~ category) +
  geom_smooth(method = "lm") + 
  scale_color_manual(values=rainbow(n=6)) +
  geom_hline(data=filter(average_age_nobel_with_names, category =="Chemistry"), aes(yintercept= mean(age_years, na.rm=T)), 
             colour="black", 
             linetype = 'dashed') +
  geom_hline(data=filter(average_age_nobel_with_names, category =="Economic Sciences"), aes(yintercept= mean(age_years, na.rm=T)), 
             colour="black", linetype = 'dashed') +
  geom_hline(data=filter(average_age_nobel_with_names, category =="Literature"), aes(yintercept= mean(age_years, na.rm=T)), 
             colour="black", linetype = 'dashed') +
  geom_hline(data=filter(average_age_nobel_with_names, category =="Physiology or Medicine"), aes(yintercept= mean(age_years, na.rm=T)), 
             colour="black", linetype = 'dashed') +
  geom_hline(data=filter(average_age_nobel_with_names, category =="Peace"), aes(yintercept= mean(age_years, na.rm=T)), 
             colour="black", linetype = 'dashed') +
  geom_hline(data=filter(average_age_nobel_with_names, category =="Physics"), aes(yintercept= mean(age_years, na.rm=T)), 
             colour="black", linetype = 'dashed') +
  labs(title = "Nobel Prize Winners'Historical Average Age",
       color = "Category",
       x = "Year",
       y = "Age") +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 15, face = "bold")) +
  geom_text_repel(aes(label=ifelse(age_years<30 | age_years > 85, as.character(nobel$knownName), "")),
                  size=2.5,
                  colour = "black",
                  max.overlaps = Inf)

# LUB

StatMeanLine <- ggproto("StatMeanLine", Stat,
                        compute_group = function(data, scales) {
                          transform(data, yintercept=mean(y))
                        },
                        required_aes = c("x", "y")
)

stat_mean_line <- function(mapping = NULL, data = NULL, geom = "hline",
                           position = "identity", na.rm = FALSE, show.legend = NA, 
                           inherit.aes = TRUE, ...) {
  layer(
    stat = StatMeanLine, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

levels(nobel$category)
ggplot(average_age_nobel_with_names, aes(x = awardYear, y = age_years) ) +
  geom_point() + 
  geom_smooth(method = "lm") +
  stat_mean_line(color="red", linetype = "dashed") +
  facet_wrap(~ category)

ggplot(nobel, aes(x = awardYear, y = gender, col = gender)) +
  geom_point(position = "jitter") + facet_grid(category ~ .) + 
  labs(title = "Distribution of Nobel Prizes over time by category") +
  xlab("Assigned Prize Year") +
  ylab("Gender / Type") +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 15, face = "bold"))

# Plot 6 - number of laureates share the award
# https://www.kaggle.com/code/imdevskp/exploring-historic-nobel-prize-data

nobel_prize_share <- nobel %>%  
                        group_by(category, awardYear) %>% 
                        summarise(number = n()) %>% 
                        ungroup 

ggplot(nobel_prize_share, aes(x = awardYear, y = category, col = factor(number))) +
  geom_point(size = 2, fill = 'black') + 
  scale_x_continuous(limits = c(1899, 2021), breaks = seq(1900, 2020, 8), expand = c(0, 0)) +
  labs(title='No. of laureates shared the award', x = '', y='') +
  theme(legend.title = element_blank(), legend.position='top', legend.justification = "left")

# Plot 7 Birth continent

nobel_prize_share_by_continent <- nobel %>%
  filter(birth_continent %in% c("Europe", "North America", "Asia", "Africa", "Oceania", "South America")) %>% 
  drop_na(birth_continent) %>%
  group_by(birth_continent) %>%
  summarise(number = n())

ggplot(nobel_prize_share_by_continent, aes(reorder(birth_continent, number), number, fill = birth_continent)) +
  geom_col() +
  labs(title = 'Birth Continent', x = '', y = '') +  
  geom_text(aes(label = number), hjust = -0.3, colour = "#555555", size = 6, fontface = "bold") +
  scale_y_continuous(limits = c(0, 520)) +
  coord_flip() +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position = "none")

# Plot 8: Bigrams - motivation in each category?
library(tidytext)

nobel$motivation<-as.character(nobel$motivation)
motivation_bigrams <- nobel %>%
  unnest_tokens(bigram, motivation, token = "ngrams", n = 2)

bigrams_separated <- motivation_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) 

bigram_united <- bigrams_filtered %>%
  filter(word1 != word2) %>%
  unite(bigram, word1, word2, sep = " ")

bigram_counts <- bigram_united %>% 
  count(bigram, sort = TRUE)
head(bigram_counts) 


# ------
  
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
  summarize(n = n()) %>% 
  top_n(5, n) %>%
  arrange(category, desc(n)) %>% 
  mutate(row_number_in_group = row_number()) %>%  
  filter(row_number_in_group %in% seq(1,5)) %>% 
  ungroup %>%
  mutate(word = reorder_within(bigram, n, category)) 

ggplot(bigrams_to_plot, aes(x= reorder(word, n), y = n, fill = category)) +
  geom_col() +
  geom_text(aes(label = n), hjust = 1.2, colour = "white", size = 5, fontface = 'bold') +
  labs(title = 'Most frequent 10 words in each category', x = '', y = '', fill = '') +
  coord_flip() + 
  theme(legend.position = 'none', panel.spacing.x = unit(1.5, "lines")) +
  scale_x_reordered() +
  facet_wrap(~category, scales = 'free')
