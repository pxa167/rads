# Run this part each time you re-start R
library(survey)
library(foreign)
library(tidyverse)
library(tableone)

# Set options to deal with lonely psu
options(survey.lonely.psu='adjust');

#load data
load("data.RData")

#make final changes to data (add pool_year, pool_wt, labels for categorical variables, etc)
data <- data %>%
  filter(perweight > 0, age > 18) %>%
  mutate(pool_year = cut(year,
                         breaks = c(-Inf, 2004, 2007, 2010, 2013, Inf),
                         labels = c("2002-2004", "2005-2007", "2008-2010", "2011-2013", "2014-2016"),
                         ordered_result = T),
         pool_wt = perweight / 3,
         agecat = cut(age,
                      breaks = c(-Inf, 64, 74, Inf),
                      labels = c("\u2264 64", "65-74", "\u2265 75"),
                      ordered_result = TRUE),
         sex = recode_factor(sex, `1` = "Male",
                             `2` = "Female"),
         covertype = recode_factor(covertype, `1` = "Private",
                                   `2` = "Any Public",
                                   `4` = "Uninsured"),
         povcat = recode_factor(povcat, `1` = "Poor",
                                `2` = "Near poor",
                                `3` = "Low",
                                `4` = "Middle",
                                `5` = "High",
                                .ordered = TRUE),
         regionmeps = recode_factor(regionmeps, `1` = "Northeast",
                                    `2` = "Midwest",
                                    `3` = "South",
                                    `4` = "West",
                                    .default = "Northeast"),
         racea = recode_factor(racea, `100` = "White",
                               `200` = "Black",
                               .default = "Other"),
         hideg = cut(hideg,
                     breaks = c(-Inf, 10, 30, 53, Inf),
                     labels = c("\u003C High School", "High School/GED Equivalent", "\u2265 Some college", "\u003C High School"),
                     ordered_result = TRUE)) %>%
  select(year, pool_year, mepsid, psuann, stratann, perweight, pool_wt, 
         age, agecat, sex, racea, povcat, covertype, hideg, regionmeps, 
         ace:aldo_exp_oop, -dupersid)
         
save(data, file = "final_data.RData")
summary(data)
         
