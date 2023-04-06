library(tidyverse)
library(ggplot2)

source = 'comparison.csv'

salaries <- readr::read_csv(here::here("data", source)) %>%
  mutate(annual = str_remove_all(annual, "[\\$\\,\\s]")) %>%
  mutate(annual = as.numeric(annual), 
         band = as.ordered(band), 
         state = as.factor(state))

bands <- salaries %>% 
  group_by(state, band) %>% 
  summarise(lo = min(annual), hi = max(annual))

# for easier comparison only look at the first five bands (P1 to P5 in WA)

bands <- filter(bands, band < 6)

base_plot <- ggplot(bands) +
  aes(colour = band, fill = band, x = state, ymin = lo, ymax = hi) + 
  scale_y_continuous(labels = scales::dollar_format(), 
                     breaks = seq(0, 180000, 20000)) +
  labs(y="annual salary", x = "") +
  theme(legend.position="none")

# Plot using linerange
p1 <- base_plot + geom_linerange(size = 15)
print(p1)

# plot using crossbar (fake the y value, normally y median etc.)
p2 <- base_plot + geom_crossbar(aes(y = lo))
print(p2)

# add a rectangle to indicate the Hurston pay range
# plot the linerange/crossbar again to get on top
p3 <- p1 + 
  annotate("rect", xmin=0, xmax=5, 
                   ymin=post$lo, ymax=post$hi, 
                   alpha = 0.1, fill = "red") + 
  geom_linerange(size = 25)
print(p3)

p4 <- p2 + 
  annotate("rect", xmin=0, xmax=5, 
           ymin=post$lo, ymax=post$hi, 
           alpha = 0.1, fill = "red") + 
  geom_crossbar(aes(y = lo))
print(p4)