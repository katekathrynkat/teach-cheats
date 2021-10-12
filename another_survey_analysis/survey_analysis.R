setwd("C:/Users/kathr/Downloads")

library(tidyverse)
library(calecopal)

responses <- read_csv('responses.csv') %>% 
  pivot_longer(2:6, names_to = 'topic') %>% 
  mutate(value = as_factor(value),
         topic = as_factor(topic))

labs <- c('Don\'t know and don\'t care', 'Don\'t know much', 'Vaguely understand it', 'Mostly understand it', 'Understand it really well')

ggplot(responses,
       aes(x = value)) +
  geom_histogram(aes(fill = value),
                 stat = 'count') +
  scale_fill_manual(values = rev(cal_palette('kelp2')),
                    labels = labs) +
  scale_y_continuous(expand = c(0,0)) +
  facet_wrap(~topic,
             nrow = 1, strip.position = 'bottom') +
  theme_classic() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.title = element_blank()) +
  labs(y = 'Count')

ggsave('plot.png', plot = last_plot(),
       width = 7, height = 2, units = 'in')
