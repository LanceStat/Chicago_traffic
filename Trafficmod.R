library(tidyverse)
library(mice)
library(VIM)
library(lubridate)
library(gtsummary)

traffic <- read_csv("Traffic_Crashes_-_Crashes.csv")

# missing values
#aggr(traffic, sortVars = TRUE)

# create new two level factor variables
traffic <- traffic %>%
  mutate(CRASH_YEAR = year(as.POSIXct(substr(traffic$CRASH_DATE, 1, 10), format = "%m/%d/%Y"))) %>% 
  mutate(CRASH_TYPE_DUM = ifelse(startsWith(traffic$CRASH_TYPE, "NO"), 0, 1)) %>% 
  mutate(PERIOD = ifelse(CRASH_YEAR < 2020, "pre", "post")) %>% 
  mutate(WEATHER_CLEAR = ifelse(str_detect(WEATHER_CONDITION, "CLEAR"), 1, 0)) %>% 
  mutate(LIGHTING_DARK = ifelse(str_detect(LIGHTING_CONDITION, "DARK"), 1, 0)) %>% 
  mutate(ROAD_WET = ifelse(str_detect(ROADWAY_SURFACE_COND, "DRY|UNKNOWN"), 0, 1)) %>% 
  mutate(INTERSECTION_RELATED_I = replace_na(INTERSECTION_RELATED_I, "N"))

#save(traffic, file = "traffic.Rdata")

# EDA 

traffic %>% 
  group_by(CRASH_YEAR) %>% 
  summarise(injuries_year = sum(CRASH_TYPE_DUM), total_crash = n()) %>% 
  mutate(injury_rate = injuries_year / total_crash) %>%
  filter(between(CRASH_YEAR, 2018, 2021)) %>% 
  ggplot(aes(CRASH_YEAR,injury_rate)) +
  geom_line() +
  geom_text(aes(label = round(injury_rate, 3)), position = position_stack(vjust = 1.1)) + 
  ylim(0.1, 0.4)


traffic %>% 
  ggplot(aes(x = PERIOD, fill = factor(CRASH_TYPE_DUM))) +
  geom_bar(position = "fill")

traffic %>% 
  ggplot(aes(x=factor(1), stat = "bin", fill = factor(CAUSE))) +
  geom_bar(position = "fill") + 
  facet_grid(facets =. ~ PERIOD) +
  coord_polar(theta = "y")

# variables we want to look at

traffic2 <- traffic %>% 
  select(CRASH_TYPE_DUM, PERIOD, WEATHER_CLEAR, LIGHTING_DARK, ROAD_WET, INTERSECTION_RELATED_I,
         DAMAGE, STREET_DIRECTION, CAUSE, CRASH_YEAR, CRASH_MONTH, CRASH_DAY_OF_WEEK, CRASH_HOUR) %>% 
  mutate(across(CRASH_TYPE_DUM:CAUSE, factor))

# make table of individual logistic regressions for each variable to help choose what to include

logistic_table <- traffic2 %>%
  select(CRASH_TYPE_DUM, PERIOD, WEATHER_CLEAR, LIGHTING_DARK, ROAD_WET, INTERSECTION_RELATED_I,
         DAMAGE, STREET_DIRECTION, CAUSE, CRASH_YEAR, CRASH_MONTH, CRASH_DAY_OF_WEEK,
         CRASH_HOUR) %>% 
  tbl_uvregression(
    method = glm,
    y = CRASH_TYPE_DUM,
    method.args = list(family = binomial),
    exponentiate = TRUE,
    pvalue_fun = ~style_pvalue(.x, digits = 3)
  )
logistic_table

# 2x2 for pre and post pandemic injury rates

dattable <- traffic %>% 
  group_by(PERIOD) %>% 
  summarise(injuries_period = sum(CRASH_TYPE_DUM), total_crash = n())

table2_2 <- tibble(Group = c("Pre-pandemic", "Pre-pandemic","Post-pandemic", "Post-pandemic"),
                   Injury = c("Yes", "No", "Yes", "No"),
                   Count = c(85524, 374627 - 85524, 70435, 228494 - 70435))

addmargins(xtabs(Count ~ Group + Injury, table2_2))
prop.test(c(70435, 85524), c(228494, 374627), correct = FALSE)

oddspost <- (70435/228494) / (1 - (70435/228494))
oddspre <- (85524/374627) / (1- (85524/374627))
odds_rat <- oddspost/oddspre
RRpan <- (70435/228494) / (85524/374627)

# logistic model

log_mod <- glm(CRASH_TYPE_DUM ~ PERIOD + WEATHER_CLEAR + LIGHTING_DARK +
                 ROAD_WET + INTERSECTION_RELATED_I + DAMAGE + CAUSE,
               family = binomial,
               data = traffic2)

logmodcoef <- coef(log_mod)
pi1 <- exp(logmodcoef[1] + logmodcoef[4] +logmodcoef[5] +
             logmodcoef[6] + logmodcoef[8] + logmodcoef[10]) / 
  (1 + exp(logmodcoef[1] + logmodcoef[4] +logmodcoef[5] +
             logmodcoef[6] + logmodcoef[8] + logmodcoef[10]))
pi2 <- exp(logmodcoef[1] + logmodcoef[2] + logmodcoef[3]) / 
  (1 + exp(logmodcoef[1] + logmodcoef[2] + logmodcoef[3]))
             

# multi-category logit mod
library(VGAM)
mult_mod <- vglm(DAMAGE ~ PERIOD,
                 family = multinomial,
                 data = traffic2)
summary(mult_mod)

mult_mod2 <- vglm(DAMAGE ~ INTERSECTION_RELATED_I,
                 family = multinomial,
                 data = traffic2)

mult_mod3 <- vglm(DAMAGE ~ LIGHTING_DARK,
                  family = multinomial,
                  data = traffic2)  