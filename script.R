library(tidyverse)
library(tidyr)
library(lubridate)
library(ggplot2)
library(ggrepel)

#install.packages('showtext', dependencies = TRUE)
library(showtext)
font_add_google("Quicksand", "Quicksand") # https://fonts.google.com/
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
            theme(plot.title = element_text(face="bold", family="Quicksand", size = 50),
                  text = element_text(family="Quicksand", size = 20),
                  axis.text=element_text(size=20),
                  axis.title=element_text(size=25, face="bold"),
                  legend.direction = "vertical",
                  legend.box = "vertical",
            )
)


## color ----

nobelgold = "#d1c700" # gold

male =  "#97e5ef" # blue
female = "#efa8e4" # pink

ind = '#ffa5b0' # smoke
org = '#45046a' # purple

rec = '#b9cced' # light blue-violet
dec = '#f64b3c' # red
res = '#434e52' # black

phy = "#fe91ca" # light pink
che = "#7fdbda" # light cyan
med = "#ade498" # light green
pea = "#ede682" # light yellow
lit = "#abc2e8" # light blue
ecn = "#febf63" # light orange

## palette ----

# gender
gen_pal <- c(male, female) 

# individual - organization
org_pal <- c(ind, org) 

# male - female - org
mfo_pal <- c(male, female, org)
names(mfo_pal) <- c('Male', 'Female', 'Organization')

# received - declined - restricted
status_pal <- c(rec, dec, res)
names(status_pal) <- c('Received', 'Declined', 'Restricted')

# category palette
cat_pal <- c(phy, che ,med ,pea ,lit ,ecn)
names(cat_pal) <- c('Physics', 'Chemistry', 'Physiology or Medicine',
                    'Peace', 'Literature', 'Economic Sciences')

# share palette
share_pal <- c("#beeb9f", "#00a388", "#f96b85")

# per 20 years
per_20_pal = c('#ffbd39', '#f76b8a', '#ff8264', '#17b978', '#005691', '#1b262c')



# Pre processing ----

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

age_stats <- nobel_data %>%
  drop_na(age_years) %>%
  group_by(category) %>%
  summarize(mean_age = mean(age_years), 
            median_age = median(age_years))


ggplot(age_data, aes(x = awardYear, y = age_years)) +
  geom_point(aes(colour = ifelse(age_years <= 30 | age_years >= 90, "red", "#d1c700")),
             size = 5,
             alpha = 0.5) + 
  geom_smooth(color = "#3d3d3d",
              alpha=0.3) +
  scale_color_manual(name = "Age",
                     values = c(nobelgold, "red"),
                     labels = c("between 30 and 90", "less than 30 or more than 90")) +
  labs(title = 'Age of laureates at the time of receiving the Nobel Prize', 
       x = 'Year', 
       y = 'Age', 
       col='') +
  geom_text_repel(aes(label = ifelse(age_years <= 30 | age_years >= 90, as.character(name), "")),
                  size = 10,
                  family = "Quicksand") +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal")
ggsave("plots/plot1.png", dpi=300)
  
  
# 2nd plot (Age of Nobel Laureates over the years) - Szymon ----

ggplot(age_data, aes(x = awardYear, y = age_years, col = category)) +
  geom_point(size = 4, alpha= 0.5) +
  geom_smooth(formula = y ~ x, method = 'loess', se = FALSE, size = 2) +
  labs(title = 'Age of Nobel Laureates over the years', x = '', y = '') +
  facet_wrap(vars(category), nrow = 2) + 
  theme(legend.position = "none", panel.spacing.x = unit(2, "lines")) + 
  scale_colour_manual(values = cat_pal)
ggsave("plots/plot2.png", dpi=150)


# 3rd plot - Artur ----

# 4rd plot - Artur ----