library(tidyverse) # for dataframe manipulation
library(glmmTMB) # for models
library(cowplot) # for figure aesthetics
library(ggeffects) # for prediction plots
library(magrittr) # for %<>%



# load and prep data ------------------------------------------------------

# import fruit data
fruit_data <- read.csv("data/fruits.csv") %>% 
  filter(damage != "yes") %>% # remove damaged plants
  dplyr::select(fruits, net, water, region, site, blocksite, MAP_hist, 
                MAT_hist, Tsum_exp, PPT_sm_exp) %>% # select relevant columns
  mutate(pollinators = ifelse(net == "yes", "no", "yes")) # make inverted column, if plot was netted, no pollinators

fruit_data$pollinators = factor(fruit_data$pollinators, levels = c("yes", "no")) # code this as a factor

# calculate means and SDs of predictors to backtransform for plots after scaling
PPT_sm_exp_mean = mean(fruit_data$PPT_sm_exp) 
PPT_sm_exp_sd = sd(fruit_data$PPT_sm_exp) 
MAT_mean = mean(fruit_data$MAT_hist) 
MAT_sd = sd(fruit_data$MAT_hist) 

# import seed data
seed_data <- read.csv("data/seeds.csv") %>% 
  filter(exclude_seeds_estimated != "yes") %>% # exclude fruits without precise seed counts
  dplyr::select(seeds, net, water, region, site, blocksite, MAP_hist,
                MAT_hist, Tsum_exp, PPT_sm_exp) %>% # select relevant columns
  mutate(pollinators = ifelse(net == "yes", "no", "yes")) # make inverted column, if plot was netted, no pollinators

seed_data$pollinators = factor(seed_data$pollinators, levels = c("yes", "no")) # code this as a factor

# scale all predictors
fruit_data$MAP_hist %<>% scale() %>% as.vector()
fruit_data$MAT_hist %<>% scale() %>% as.vector()
fruit_data$Tsum_exp %<>% scale() %>% as.vector()
fruit_data$PPT_sm_exp %<>% scale() %>% as.vector()

seed_data$MAP_hist %<>% scale() %>% as.vector()
seed_data$MAT_hist %<>% scale() %>% as.vector()
seed_data$Tsum_exp %<>% scale() %>% as.vector()
seed_data$PPT_sm_exp %<>% scale() %>% as.vector()

# subtract 1 from fruits to conform to negative binomial
fruit_data$fruits %<>% subtract(1)



# plots and models with water addition: seeds -----------------------------

# make region a factor with center first for models, but in geographic order for plotting
seed_data$region = factor(seed_data$region, levels = c("Center", "North", "Southwest"))
seed_data$region_ordered = factor(seed_data$region, levels = c("Southwest", "Center", "North"))

# make a combined treatment column
seed_data$trt = paste0("Net", seed_data$net, "Water", seed_data$water)
# convert to pretty labels
seed_data$trtlabel = ifelse(seed_data$trt == "NetnoWaterno", "control", ifelse(seed_data$trt == "NetnoWateryes", "water addition", ifelse(seed_data$trt == "NetyesWaterno","pollinator exclusion", "both")))
# order labels
seed_data$trtlabel = factor(seed_data$trtlabel, levels = c("control", "water addition", "pollinator exclusion", "both"))

# model including water
seeds_reg_wat = glmmTMB(seeds ~ region*pollinators*water + (1|site/blocksite), data = seed_data, family = nbinom2, ziformula = ~ 1)
summary(seeds_reg_wat)
# significant pollinator*water interaction

# generate model predictions for plotting
pred_seeds_wat = ggaverage(seeds_reg_wat, term = c("region", "water", "pollinators"))
# check it out
plot(pred_seeds_wat)
# convert numeric levels to regions
pred_seeds_wat$region = c("Center", "Center","Center", "Center", "North", "North", "North", "North", "Southwest", "Southwest", "Southwest", "Southwest")
# again, create different order for plotting
pred_seeds_wat$region_ordered = factor(pred_seeds_wat$region, levels = c("Southwest", "Center", "North"))
# make water column
pred_seeds_wat$water = factor(pred_seeds_wat$group, levels = c("yes", "no"))
# make pollinator column
pred_seeds_wat$pollinators = factor(pred_seeds_wat$facet, levels = c("yes", "no"))
# make combined treatment column
pred_seeds_wat$trtlabel = ifelse(pred_seeds_wat$water == "no" & pred_seeds_wat$pollinators == "yes", "control", ifelse(pred_seeds_wat$water == "yes" & pred_seeds_wat$pollinators == "yes", "water addition", ifelse(pred_seeds_wat$water == "no" & pred_seeds_wat$pollinators == "no", "pollinator exclusion", "both")))
# order these levels
pred_seeds_wat$trtlabel = factor(pred_seeds_wat$trtlabel, levels = c("control", "water addition", "pollinator exclusion", "both"))

# make plot
ggplot() +
  # boxplots of raw data
  geom_boxplot(data = seed_data, aes(y = seeds, x = region_ordered, fill = trtlabel), outlier.size = 0.7) +
  # predicted means
  geom_point(data = pred_seeds_wat, aes(y = predicted, x = region_ordered, group = trtlabel), size = 2, 
                  position = position_dodge(width = 0.75)) +
  # confidence intervals
  geom_errorbar(data = pred_seeds_wat, aes(ymin = conf.low, ymax = conf.high, x = region_ordered, group = trtlabel), size = 0.9, 
                       position = position_dodge(width = 0.75), width = 0.2) +
  # raw means
  stat_summary(data = seed_data, aes(y = seeds, x = as.numeric(region_ordered) + 0.05, group = trtlabel), geom = "point", fun.data = "mean_se", 
               position = position_dodge(width = 0.75), shape = 2, color = "black") +
  xlab("Region") +
  ylab("Seeds per fruit") +
  scale_fill_discrete(name = "Treatment")
ggsave("figs/seeds_water.pdf", height = 4, width = 8)



# plots and models with water addition: fruits ----------------------------

# make region a factor with center first for models, but in geographic order for plotting
fruit_data$region = factor(fruit_data$region, levels = c("Center", "North", "Southwest"))
fruit_data$region_ordered = factor(fruit_data$region, levels = c("Southwest", "Center", "North"))

# make a combined treatment column
fruit_data$trt = paste0("Net", fruit_data$net, "Water", fruit_data$water)
# convert to pretty labels
fruit_data$trtlabel = ifelse(fruit_data$trt == "NetnoWaterno", "control", ifelse(fruit_data$trt == "NetnoWateryes", "water addition", ifelse(fruit_data$trt == "NetyesWaterno", "pollinator exclusion", "both")))
# order labels
fruit_data$trtlabel = factor(fruit_data$trtlabel, levels = c("control", "water addition", "pollinator exclusion", "both"))

# model including water
fruits_reg_wat = glmmTMB(fruits ~ region*pollinators*water + (1|site/blocksite), data = fruit_data, family = nbinom2)
summary(fruits_reg_wat)
# significant pollinator*water interaction and region*water interaction

# generate model predictions for plotting
pred_fruit_wat = ggaverage(fruits_reg_wat, term = c("region","water", "pollinators"))
# check it out
plot(pred_fruit_wat)
# convert numeric levels to regions
pred_fruit_wat$region = c("Center", "Center","Center", "Center", "North", "North", "North", "North", "Southwest", "Southwest", "Southwest", "Southwest")
# again, create different order for plotting
pred_fruit_wat$region_ordered = factor(pred_fruit_wat$region, levels = c("Southwest", "Center", "North"))
# make water column
pred_fruit_wat$water = factor(pred_fruit_wat$group, levels = c("yes", "no"))
# make pollinator column
pred_fruit_wat$pollinators = factor(pred_fruit_wat$facet, levels = c("yes", "no"))
# make combined treatment column
pred_fruit_wat$trtlabel = ifelse(pred_fruit_wat$water == "no" & pred_fruit_wat$pollinators == "yes", "control", ifelse(pred_fruit_wat$water == "yes" & pred_fruit_wat$pollinators == "yes", "water addition", ifelse(pred_fruit_wat$water == "no" & pred_fruit_wat$pollinators == "no", "pollinator exclusion", "both")))
# order these levels
pred_fruit_wat$trtlabel = factor(pred_fruit_wat$trtlabel, levels = c("control", "water addition", "pollinator exclusion", "both"))

# make plot
ggplot() +
  # boxplot of raw data
  geom_boxplot(data = fruit_data, aes(y = fruits, x = region_ordered, fill = trtlabel), outlier.size = 0.7) +
  # model predicted means
  geom_point(data = pred_fruit_wat, aes(y = predicted, x = region_ordered, group = trtlabel), size = 2, 
             position = position_dodge(width = 0.8)) +
  # confidence intervals
  geom_errorbar(data = pred_fruit_wat, aes(ymin = conf.low, ymax = conf.high, x = region_ordered, group = trtlabel), size = 0.9, 
                position = position_dodge(width = 0.8), width = 0.2) +
  # raw means
  stat_summary(data = fruit_data, aes(y = fruits, x = as.numeric(region_ordered) + 0.05, group = trtlabel), geom = "point", fun.data = "mean_se", 
               position = position_dodge(width = 0.75), shape = 2, color = "black") +
  xlab("Region") +
  ylab("Fruits per plant") +
  scale_fill_discrete(name = "Treatment")
ggsave("figs/fruits_water.pdf", height = 4, width = 8)



# region and seeds --------------------------------------------------------

# check out data distribution
hist(seed_data$seeds, breaks = 40)

# run model
seeds_reg_mod = glmmTMB(seeds ~ region*pollinators + (1|site/blocksite), data = seed_data, family = nbinom2, ziformula = ~ 1)
summary(seeds_reg_mod)
# effect of pollinators, interaction between pollinators and north

# generate model predictions for plotting
pred_region = ggaverage(seeds_reg_mod, term = c("region", "pollinators"))
# check it out
plot(pred_region)
# change numeric column to region
pred_region$region = c("Center", "Center", "North", "North", "Southwest", "Southwest")
# alternate order for plotting
pred_region$region_ordered = factor(pred_region$region, levels = c("Southwest", "Center", "North"))
# make pollinator column
pred_region$pollinators = factor(pred_region$group, levels = c("yes", "no"))

# make plot
ggplot() +
  # boxplots of raw data
  geom_boxplot(data = seed_data, aes(y = seeds, x = region_ordered, fill = pollinators), position = position_dodge(width = 0.8), color = "grey40", outlier.size = 0.7) +
  # model predicted means
  geom_point(data = pred_region, aes(y = predicted, x = region_ordered, group = pollinators), size = 2, 
                  position = position_dodge(width = 0.8)) +
  # confidence interval
  geom_errorbar(data = pred_region, aes(ymin = conf.low, ymax = conf.high, x = region_ordered, group = pollinators), size = 0.9, 
                  position = position_dodge(width = 0.8), width = 0.2) +
  # raw means
  stat_summary(data = seed_data, aes(y = seeds, x = as.numeric(region_ordered) + 0.1, group = pollinators), geom = "point", fun.data = "mean_se", 
               position = position_dodge(width = 0.75), shape = 2, color = "black") +
  scale_fill_manual(values = c("white", "grey70")) +
  xlab("Region") +
  ylab("Seed count") +
  guides(fill = guide_legend(title = "Pollinators")) +
  theme(legend.position = c(0.1, 0.8))
ggsave("figs/seeds_region.pdf", width = 4, height = 4)



# seeds and summer precipitation -------------------------------------------

seeds_pptsum_mod = glmmTMB(seeds ~ PPT_sm_exp*pollinators + (1|site/blocksite), data = seed_data, family = nbinom2, ziformula = ~ 1)
summary(seeds_pptsum_mod)
# only pollinator exclusion

pred = ggpredict(seeds_pptsum_mod, term = c("PPT_sm_exp","pollinators"))
plot(pred)



# seeds and summer temperature ----------------------------------------------

seeds_tsum_mod = glmmTMB(seeds ~ Tsum_exp*pollinators + (1|site/blocksite), data = seed_data, family = nbinom2, ziformula = ~ 1)
summary(seeds_tsum_mod)
# only pollinator exclusion

pred = ggpredict(seeds_tsum_mod, term = c("Tsum_exp","pollinators"))
plot(pred)



# seeds and MAT ------------------------------------------------------------

seeds_mat_mod = glmmTMB(seeds ~ MAT_hist*pollinators + (1|site/blocksite), data = seed_data, family = nbinom2, ziformula = ~ 1)
summary(seeds_mat_mod)
# only pollinator exclusion

pred = ggpredict(seeds_mat_mod, term = c("MAT_hist","pollinators"))
plot(pred)



# seeds and MAP ------------------------------------------------------------

seeds_map_mod = glmmTMB(seeds ~ MAP_hist*pollinators + (1|site/blocksite), data = seed_data, family = nbinom2, ziformula = ~ 1)
summary(seeds_map_mod)
# effect of pollinator exclusion, marginally significant interaction

pred = ggpredict(seeds_map_mod, term = c("MAP_hist","pollinators"))
plot(pred)



# region and fruits --------------------------------------------------------

# check out the distribution
hist(fruit_data$fruits, breaks = 40)
summary(fruit_data$fruits)
# how many pseudo-0s?
dim(fruit_data[fruit_data$fruits == 0,]); dim(fruit_data)

# run model
fruits_reg_mod = glmmTMB(fruits ~ region*pollinators + (1|site/blocksite), data = fruit_data, family = nbinom2)
summary(fruits_reg_mod)
# significant effect of north, pollinator exclusion, marginal interaction between these two factors

# generate model predictions for plotting
pred_region_fruits = ggaverage(fruits_reg_mod, term = c("region","pollinators"))
# check it out
plot(pred_region_fruits)
# create region column in two orders
pred_region_fruits$region = c("Center", "Center", "North", "North", "Southwest", "Southwest")
pred_region_fruits$region_ordered = factor(pred_region_fruits$region, levels = c("Southwest", "Center", "North"))
# create pollinator column
pred_region_fruits$pollinators = factor(pred_region_fruits$group, levels = c("yes", "no"))

# look at means
fruit_data %>% 
  group_by(region) %>% summarize(mean(fruits))
fruit_data %>% 
  group_by(net) %>% summarize(mean(fruits))

# make plot
ggplot() +
  # boxplots of raw data
  geom_boxplot(data = fruit_data, aes(y = fruits, x = region_ordered, fill = pollinators), position = position_dodge(width = 0.8), color = "grey40", outlier.size = 0.7) +
  # model means
  geom_point(data = pred_region_fruits, aes(y = predicted, x = region_ordered, group = group), size = 2, 
             position = position_dodge(width = 0.8)) +
  # CIs
  geom_errorbar(data = pred_region_fruits, aes(ymin = conf.low, ymax = conf.high, x = region_ordered, group = group), size = 0.9, 
                position = position_dodge(width = 0.8), width = 0.2) +
  # raw means
  stat_summary(data = fruit_data, aes(y = fruits, x = as.numeric(region_ordered) + 0.1, group = pollinators), geom = "point", fun.data = "mean_se", 
               position = position_dodge(width = 0.75), shape = 2, color = "black") +
  scale_fill_manual(values = c("white", "grey70")) +
  xlab("Region") +
  ylab("Fruit count") +
  guides(fill = guide_legend(title = "Pollinators")) +
  theme(legend.position = c(0.1, 0.8))
ggsave("figs/fruits_region.pdf", width = 4, height = 4)



# fruits and summer precipitation ------------------------------------------

# run model
fruits_pptsum_mod = glmmTMB(fruits ~ PPT_sm_exp*pollinators + (1|site/blocksite), data = fruit_data, family = nbinom2)
summary(fruits_pptsum_mod)
# pollintors significant, significant interaction

# generate predictions for plotting
pred_fruits_pptsum = ggpredict(fruits_pptsum_mod, term = c("PPT_sm_exp","pollinators"))
# check them out
plot(pred_fruits_pptsum)
# rename x variable
pred_fruits_pptsum$PPT_sm_exp = pred_fruits_pptsum$x 

# generate raw means
fruits_means_pptsm = fruit_data %>% 
  group_by(PPT_sm_exp, pollinators, site) %>% 
  summarize(fruits_mean = mean(fruits))

# create plot and same to object for multipanel
a = ggplot() +
  geom_ribbon(data = pred_fruits_pptsum, aes(ymin = conf.low, ymax = conf.high, x = (PPT_sm_exp*PPT_sm_exp_sd)+PPT_sm_exp_mean, fill = group), alpha = 0.3) +
  geom_line(data = pred_fruits_pptsum, aes(y = predicted, x = (PPT_sm_exp*PPT_sm_exp_sd)+PPT_sm_exp_mean, color = group), size = 1.5) +
  ylim(0, 7) +
  geom_point(data = fruits_means_pptsm, aes(y = fruits_mean, x = (PPT_sm_exp*PPT_sm_exp_sd)+PPT_sm_exp_mean, color = pollinators), size = 3) +
  xlab("Summer precipitation in 2015 (mm)") +
  ylab("Fruit count") +
  guides(fill = FALSE, color = FALSE) +
  scale_fill_manual("Pollinators", values = c("black", "grey50")) +
  scale_color_manual("Pollinators", values = c("black", "grey50")) +
  scale_shape_manual(values = c(1, 2, 3, 4, 5, 6, 7, 8)) +
  guides(shape = FALSE)



# fruits and summer temperature -------------------------------------------

fruits_tsum_mod = glmmTMB(fruits ~ Tsum_exp*pollinators + (1|site/blocksite), data = fruit_data, family = nbinom2)
summary(fruits_tsum_mod)
# only pollinators significant

pred = ggpredict(fruits_tsum_mod, term = c("Tsum_exp","pollinators"))
plot(pred)



# fruits and MAT ----------------------------------------------------------

# run model
fruits_mat_mod = glmmTMB(fruits ~ MAT_hist*pollinators + (1|site/blocksite), data = fruit_data, family = nbinom2)
summary(fruits_mat_mod)
# MAT significant, pollinators significant, marginal interaction

# generate predictions for plotting
pred_fruits_mat = ggpredict(fruits_mat_mod, term = c("MAT_hist","pollinators"))
# check them out
plot(pred_fruits_mat)
# rename x variable
pred_fruits_mat$MAT_hist = pred_fruits_mat$x 

# calculate raw means
fruits_means_mat = fruit_data %>% 
  group_by(MAT_hist, pollinators, site) %>% 
  summarize(fruits_mean = mean(fruits))

# make plot and save to object for multipanel
b = ggplot() +
  geom_ribbon(data = pred_fruits_mat, aes(ymin = conf.low, ymax = conf.high, x = (MAT_hist*MAT_sd)+MAT_mean, fill = group), alpha = 0.3) +
  geom_line(data = pred_fruits_mat, aes(y = predicted, x = (MAT_hist*MAT_sd)+MAT_mean, color = group), size = 1.5) +
  # geom_point(data = fruits_means_pptsm, aes(y = fruits, x = (MAT_hist*MAT_sd)+MAT_mean, color = pollinators)) +
  geom_point(data = fruits_means_mat, aes(y = fruits_mean, x = (MAT_hist*MAT_sd)+MAT_mean, color = pollinators), size = 3) +
  xlab("Mean annual temperature (°C)") +
  ylab("Fruit count") +
  # ylim(0, 7) +
  scale_shape_manual(values = c(1, 2, 3, 4, 5, 6, 7, 8)) +
  scale_fill_manual("Pollinators", values = c("black", "grey50")) +
  scale_color_manual("Pollinators", values = c("black", "grey50")) +
  theme(legend.position = c(0.6, 0.8)) +
  guides(shape = FALSE)

plot_grid(a, b, labels = c("A", "B"), label_fontface = "bold", rel_widths = c(1, 1))
ggsave("figs/fruit_climate.pdf", width = 8, height = 4)



# fruits and MAP ----------------------------------------------------------

fruits_map_mod = glmmTMB(fruits ~ MAP_hist*pollinators + (1|site/blocksite), data = fruit_data, family = nbinom2)
summary(fruits_map_mod)
# only pollinators significant

pred = ggpredict(fruits_map_mod, term = c("MAP_hist","pollinators"))
plot(pred)



# seed production and plant counts in the next year -----------------------

dat = read.csv("data/seeds_per_plot.csv") %>% 
  select(region, site, blocksite, plantcount_2015, plantcount_2016_total, plantcount_2016_inner, total)

# data distributions, both counts are pretty left-skewed
hist(dat$plantcount_2016_inner)
hist(dat$plantcount_2016_total, breaks = 200)
plot(dat$plantcount_2016_total, dat$plantcount_2016_inner)
plot(dat$plantcount_2015, dat$plantcount_2016_inner)
plot(dat$plantcount_2015, dat$plantcount_2016_total)
plot(log(dat$plantcount_2016_total), log(dat$total))

# scale some variables
dat$plantcount_2016_scaled = as.vector(scale(dat$plantcount_2016_total))
dat$total_scaled = as.vector(scale(dat$total))
dat$plantcount_2015_scaled = as.vector(scale(dat$plantcount_2015))

# run model
next_year_mod = glmmTMB(plantcount_2016_total ~ total_scaled + (1|site/blocksite), family = poisson, data = dat)
summary(next_year_mod)

pred_next_year = ggpredict(next_year_mod, term = "total_scaled", pretty = FALSE)
plot(pred_next_year)
pred_next_year$x_un = pred_next_year$x * sd(dat$total) + mean(dat$total)

ggplot() +
  geom_line(data = pred_next_year, aes(x = x_un, y = predicted)) +
  geom_ribbon(data = pred_next_year, aes(x = x_un, ymin = conf.low, ymax= conf.high), alpha = 0.2) +
  ylab("Adult plants in 2016") +
  xlab("Estimated seed input in 2015")


# run model with plants in 2015 as a covariate
next_year_mod = glmmTMB(plantcount_2016_total ~ total_scaled + plantcount_2015 + (1|site/blocksite), family = poisson, data = dat)
summary(next_year_mod)

pred_next_year = ggpredict(next_year_mod, term = "total_scaled", pretty = FALSE)
plot(pred_next_year)
pred_next_year$x_un = pred_next_year$x * sd(dat$total) + mean(dat$total)

ggplot() +
  geom_line(data = pred_next_year, aes(x = x_un, y = predicted)) +
  geom_ribbon(data = pred_next_year, aes(x = x_un, ymin = conf.low, ymax= conf.high), alpha = 0.2) +
  ylab("Adult plants in 2016") +
  xlab("Estimated seed input in 2015")

# result is similar, even when number of plants last year is a covariate. 
# number of plants last year is also significant



# gather model output for tables -------------------------------------------

mod_list = ls(pattern = "*mod")
seeds_mod_list = grep("seeds*", mod_list, value = TRUE)
fruits_mod_list = grep("fruits*", mod_list, value = TRUE)

for (i in 1:length(seeds_mod_list)){
  mod = get(as.character(seeds_mod_list[i]))
  mod_sum = summary(get(as.character(seeds_mod_list[i])))
  c = rownames_to_column(data.frame(t(mod_sum$coefficients$cond)))
  seeds_results = cbind(model = paste(mod_list[i]), part = "cond", c) 
  if (i == 1) seeds_results_new = seeds_results else seeds_results_new = bind_rows(seeds_results_old, seeds_results)
  seeds_results_old = seeds_results_new
}

results_seeds = seeds_results_new %>% 
  unite(model, model, part, remove = TRUE, sep = ".") %>% 
  gather(var, value, c(3:16)) %>%
  filter(!is.na(value)) %>%
  unite(variable, var, rowname, sep = ":") %>%
  spread(variable, value)

write.csv(results_seeds, "tables/seeds_wide.csv")

for (i in 1:length(fruits_mod_list)){
  mod = get(as.character(fruits_mod_list[i]))
  mod_sum = summary(get(as.character(fruits_mod_list[i])))
  c = rownames_to_column(data.frame(t(mod_sum$coefficients$cond)))
  fruits_results = cbind(model = paste(mod_list[i]), part = "cond", c) 
  if (i == 1) fruits_results_new = fruits_results else fruits_results_new = bind_rows(fruits_results_old, fruits_results)
  fruits_results_old = fruits_results_new
}

results_fruits = fruits_results_new  %>% 
  unite(model, model, part, remove = TRUE, sep = ".") %>% 
  gather(var, value, c(3:16)) %>%
  filter(!is.na(value)) %>%
  unite(variable, var, rowname, sep = ":") %>%
  spread(variable, value)

write.csv(results_fruits, "tables/fruits_wide.csv")



# look at seeds as a proportion of seeds in control plots (unpublished) ------

seeds_net = seed_data %>% filter(net == "yes") %>% 
  group_by(region, site, blocksite, MAP_hist, MAT_hist, Tsum_exp, PPT_sm_exp) %>% 
  summarize(seeds_net = mean(seeds))

seeds_nonet = seed_data %>% filter(net == "no") %>% 
  group_by(region, site, blocksite, MAP_hist, MAT_hist, Tsum_exp, PPT_sm_exp) %>% 
  summarize(seeds_nonet = mean(seeds))

seeds_binom = left_join(seeds_net, seeds_nonet) %>% 
  group_by(region, site, blocksite, MAP_hist, MAT_hist, Tsum_exp, PPT_sm_exp) %>% 

seeds_binom$region = factor(seeds_binom$region, levels = c("Southwest", "Center", "North"))

ggplot(seeds_binom, aes(y = prop_seeds, x = region)) +
  geom_boxplot() +
  xlab("Region") +
  ylab("Proportion seed set in\nabsence of pollinators") +
  ylim(0,1)

# with water

seeds_net_water = seed_data %>% filter(net == "yes") %>% 
  group_by(region, site, blocksite, MAP_hist, MAT_hist, Tsum_exp, PPT_sm_exp, water) %>% 
  summarize(seeds_net = mean(seeds))

seeds_nonet_water = seed_data %>% filter(net == "no") %>% 
  group_by(region, site, blocksite, MAP_hist, MAT_hist, Tsum_exp, PPT_sm_exp, water) %>% 
  summarize(seeds_nonet = mean(seeds))

seeds_binom_water = left_join(seeds_net_water, seeds_nonet_water) %>% 
  group_by(region, site, blocksite, MAP_hist, MAT_hist, Tsum_exp, PPT_sm_exp, water) %>% 
  mutate(prop_seeds = seeds_net/seeds_nonet)
seeds_binom_water$prop_seeds[seeds_binom_water$prop_seeds > 1] = 1

ggplot(seeds_binom_water, aes(y = prop_seeds, x = region, fill = water)) +
  geom_boxplot() +
  xlab("Region") +
  ylab("Proportion seed set in\nabsence of pollinators")

