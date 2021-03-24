### PRE-COURSE SURVEY ANALYSIS ###

# Packages
library(calecopal)
library(janitor)
library(tidyverse)

# Load survey data
survey <- read_tsv('survey_analysis/test_survey.tsv') %>% 
  clean_names() %>% 
  rowid_to_column('survey_id')

# Wrangle data
stats <- survey %>% 
  select(1,10) %>% 
  rename('stats' = 2) %>% 
  mutate(
    NoStats = grepl('experience', stats, ignore.case = TRUE),
    HighschoolStats = grepl('high', stats, ignore.case = TRUE),
    CollegeStats = grepl('college', stats, ignore.case = TRUE),
    Biometry = grepl('biometry', stats, ignore.case = TRUE),
    InstalledR = grepl('installed', stats, ignore.case = TRUE),
    TentativeR = grepl('tentatively', stats, ignore.case = TRUE),
    ConfidentR = grepl('confident', stats, ignore.case = TRUE)
  ) %>% 
  pivot_longer(NoStats:ConfidentR, 'variable') %>% 
  group_by(variable) %>% 
  count(value) %>% 
  ungroup() %>% 
  filter(value == TRUE) %>% 
  mutate(variable = factor(variable, levels = c('NoStats',
                                                'HighschoolStats', 'CollegeStats', 'Biometry',
                                                'TentativeR', 'ConfidentR', 'InstalledR')))

poll <- read_csv('week3_poll.csv') %>% 
  rename(section = 1,
         ratio = 2,
         stats = 3,
         postgrad = 4) %>% 
  group_by(section) %>% 
  mutate(class_n = length(section)) %>% 
  ungroup()



stats <- poll %>% 
  select(section, class_n, stats) %>% 
  mutate(
    MassivelyConfused = str_detect(stats, 'confused'),
    HighschoolStats = str_detect(stats, 'high'),
    CollegeStats = str_detect(stats, 'college'),
    Biometry = str_detect(stats, 'biometry'),
    InstalledR = str_detect(stats, 'installed'),
    TentativeR = str_detect(stats, 'tentatively'),
    ConfidentR = str_detect(stats, 'confident')
  ) %>% 
  pivot_longer(MassivelyConfused:ConfidentR, 'variable') %>% 
  group_by(section, class_n, variable) %>% 
  count(value) %>% 
  filter(value == TRUE) %>%
  mutate(n_adj = n/class_n*25) %>% 
  select(section, variable, n_adj) %>% 
  group_by(variable) %>% 
  summarize(mean = mean(n_adj),
            sd = sd(n_adj)) %>% 
  mutate(variable = factor(variable, levels = c('MassivelyConfused',
                                                'HighschoolStats', 'CollegeStats', 'Biometry',
                                                'TentativeR', 'ConfidentR', 'InstalledR')))