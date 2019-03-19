library(tidyverse)
library(cowplot)
library(grid)

# fig 2: climate across region --------------------------------------------

# climate plot
climate = read.csv("data/all_clim.csv")
site_abbrevs <- climate %>% 
  select(site, abbreviation) %>% 
  unique()

yearly = read.csv("data/sites_historic_clim.csv") %>% 
  mutate(Tsum = (Tave06 + Tave07)/2, Psum = PPT06 + PPT07) %>% 
  select(region, site, Tsum, Psum, MAT, MAP) %>% 
  gather(var, "value", 3:6) %>% 
  left_join(., site_abbrevs)

yearly$abbreviation = factor(yearly$abbreviation, levels = c("N1", "N2", "C1", "C2", "C3", "SW1", "SW2", "SW3"))
yearly$site_numeric = as.numeric(yearly$abbreviation)

clim_tall = climate %>% 
  dplyr::select(site, region, abbreviation, Tsum_exp, PPT_sm_exp, 
                MAP_exp, MAT_exp) %>% 
  rename(Psum_exp = PPT_sm_exp) %>% 
  gather(var,"value",4:7) %>% 
  separate(var, into = c("var", "time"), sep = "_")

clim_tall$site <- factor(clim_tall$site, levels = c("Blue Lake", "Rock Creek", "Abel's Ridge", "Government Trail Ridge", "Tucannon", "Highway 26", "Leslie Gulch", "McKay Creek"))
clim_tall$abbreviation <- factor(clim_tall$abbreviation, levels = c("N1", "N2", "C1", "C2", "C3", "SW1", "SW2", "SW3"))
clim_tall$site_numeric <- as.numeric(clim_tall$abbreviation)
clim_tall$region <- factor(clim_tall$region, levels = c("North", "Center", "Southwest"))

MAP = ggplot(data = filter(yearly, var == "MAP"), aes(y = value, x = site_numeric + 0.14, group = abbreviation)) +
  geom_boxplot(color = "black", outlier.size = 0.1, width = 0.25) + 
  geom_point(data = filter(clim_tall, var == "MAP"), aes(y = value, x = site_numeric - 0.14, group = abbreviation), shape = 2) +
  ylab("Precipitation (mm)") +
  guides(color = FALSE, linetype = FALSE, shape = FALSE) +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8), labels = c("N1", "N2", "C1", "C2", "C3", "SW1", "SW2", "SW3")) +
  geom_segment(x = 2.5, xend = 2.5, y = -Inf, yend = 1500, color = "grey50", lwd = 0.2) +
  geom_segment(x = 5.5, xend = 5.5, y = -Inf, yend = 1500, color = "grey50", lwd = 0.2) +
  theme(axis.title.y=element_text(size = 12),
        axis.title.x=element_blank(),
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.text = element_text(size = 9)); MAP

Psum = ggplot(data = filter(yearly, var == "Psum"), aes(y = value, x = site_numeric + 0.14, group = abbreviation)) +
  geom_boxplot(color = "black", outlier.size = 0.1, width = 0.25) + 
  geom_point(data = filter(clim_tall, var == "Psum"), aes(y = value, x = site_numeric - 0.14, group = abbreviation), shape = 2) +
  ylab("Precipitation (mm)") +
  ylim(0, 250) +
  guides(color = FALSE, linetype = FALSE, shape = FALSE) +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8), labels = c("N1", "N2", "C1", "C2", "C3", "SW1", "SW2", "SW3")) +
  geom_segment(x = 2.5, xend = 2.5, y = -Inf, yend = 220, color = "grey50", lwd = 0.2) +
  geom_segment(x = 5.5, xend = 5.5, y = -Inf, yend = 220, color = "grey50", lwd = 0.2) +
  theme(axis.title.y=element_text(size = 12),
        axis.title.x=element_blank(),
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.text = element_text(size = 9)); Psum

MAT = ggplot(data = filter(yearly, var == "MAT"), aes(y = value, x = site_numeric + 0.14, group = abbreviation)) +
  geom_boxplot(color = "black", outlier.size = 0.1, width = 0.25) + 
  geom_point(data = filter(clim_tall, var == "MAT"), aes(y = value, x = site_numeric - 0.14, group = abbreviation), shape = 2) +
  ylab("Temperature (°C)") +
  guides(color = FALSE, linetype = FALSE, shape = FALSE) +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8), labels = c("N1", "N2", "C1", "C2", "C3", "SW1", "SW2", "SW3")) +
  geom_segment(x = 2.5, xend = 2.5, y = -Inf, yend = 10, color = "grey50", lwd = 0.2) +
  geom_segment(x = 5.5, xend = 5.5, y = -Inf, yend = 10, color = "grey50", lwd = 0.2) +
  theme(axis.title.y=element_text(size = 12),
        axis.title.x=element_blank(),
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.text = element_text(size = 9)); MAT

Tsum = ggplot(data = filter(yearly, var == "Tsum"), aes(y = value, x = site_numeric + 0.14, group = abbreviation)) +
  geom_boxplot(color = "black", outlier.size = 0.1, width = 0.25) + 
  geom_point(data = filter(clim_tall, var == "Tsum"), aes(y = value, x = site_numeric - 0.14, group = abbreviation), shape = 2) +
  geom_segment(x = 2.5, xend = 2.5, y = -Inf, yend = 20, color = "grey50", lwd = 0.2) +
  geom_segment(x = 5.5, xend = 5.5, y = -Inf, yend = 20, color = "grey50", lwd = 0.2) +
  ylab("Temperature (°C)") +
  guides(color = FALSE, linetype = FALSE, shape = FALSE) +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8), labels = c("N1", "N2", "C1", "C2", "C3", "SW1", "SW2", "SW3")) +
  theme(axis.title.y=element_text(size = 12),
        axis.title.x=element_blank(),
        axis.text.x = element_text(color = "black"), 
        axis.text = element_text(size = 9),
        plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm")); Tsum


plot_grid(MAP, Psum, MAT, Tsum, NULL, ncol = 1, rel_heights = c(0.9, 0.9, 0.9, 1, 0.2), align = "v") +
  draw_label("Region", 0.58, 0.02, size = 12) +
  draw_label("North", 0.31, 0.045, size = 9, colour = 'black') +
  draw_label("Center", 0.54, 0.045, size = 9, colour = 'black') +
  draw_label("Southwest", 0.82, 0.045, size = 9, colour = 'black') +
  draw_label("a) Annual", 0.23, 0.98, size = 10, hjust = 0) +
  draw_label("b) Summer", 0.23, 0.75, size = 10, hjust = 0) +
  draw_label("c) Annual", 0.23, 0.52, size = 10, hjust = 0) +
  draw_label("d) Summer", 0.23, 0.29, size = 10, hjust = 0)
ggsave("figs/climate.pdf", height = 8, width = 3.5)


