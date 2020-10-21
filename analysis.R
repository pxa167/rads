# Run this part each time you re-start R
library(survey)
library(foreign)
library(tidyverse)

# Set options to deal with lonely psu
options(survey.lonely.psu='adjust');

#load data
load("data.RData")

#transform data for trend lines
trend_data <- data %>%
  mutate(pool_year = recode(year, `2002` = "2002-2004",
                            `2003` = "2002-2004",
                            `2004` = "2002-2004",
                            `2005` = "2005-2007",
                            `2006` = "2005-2007",
                            `2007` = "2005-2007",
                            `2008` = "2008-2010",
                            `2009` = "2008-2010",
                            `2010` = "2008-2010",
                            `2011` = "2011-2013",
                            `2012` = "2011-2013",
                            `2013` = "2011-2013",
                            `2014` = "2014-2016",
                            `2015` = "2014-2016",
                            `2016` = "2014-2016"),
         pool_wt = perweight / 3,
         age_cat = if_else(age < 65, "40-64",
                           if_else(age > 75, ">75",
                                   "65-74")))

#create survey object
mepsdsgn = svydesign(
  id = ~psuann, 
  strata = ~stratann, 
  weights = ~pool_wt, 
  data = trend_data, 
  nest = TRUE)  

#demo table
svyCreateCatTable(
  vars = c("sex", "age_cat", "regionmeps", "povcat", "covertype"),
  strata = c("pool_year"),
  data = mepsdsgn,
  includeNA = FALSE,
  test = TRUE,
  testApprox = svyTestChisq,
  argsApprox = NULL,
  smd = TRUE,
  addOverall = FALSE
)


#demographics
a <- svyby(formula = ~ bb, by = ~pool_year,
      design = mepsdsgn,
      FUN = svymean, na.rm = TRUE,
      keep.names = FALSE) 

b <- svyby(formula = ~ ace, by = ~pool_year,
      design = mepsdsgn,
      FUN = svymean, na.rm = TRUE,
      keep.names = FALSE)

c <- svyby(formula = ~ aldo, by = ~pool_year,
      design = mepsdsgn,
      FUN = svymean, na.rm = TRUE,
      keep.names = FALSE)

df <- bind_rows(a,b,c) %>%
  select(pool_year, se, everything()) %>%
  gather(key = "med", value = "prop", bb:aldo) %>%
  drop_na() %>%
  mutate(med = factor(med, levels = c("bb", "ace", "aldo")))
