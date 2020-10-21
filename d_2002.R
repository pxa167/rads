library(foreign)
library(tidyverse)
library(ipumsr)

# load data files, change col names to lowercase
mc <- read.xport("./mc/mc_2002.ssp")
pm <- read.xport("./pm/pm_2002.ssp")
colnames(mc) <- tolower(colnames(mc))
colnames(pm) <- tolower(colnames(pm))

ddi <- read_ipums_ddi("meps_00001.xml")
meps <- read_ipums_micro(ddi)
colnames(meps) <- tolower(colnames(meps))
meps <- filter(meps, year == 2002)

#add mepsid var to data for join with FYC file later on --> filter to only include people with HF
mc_hf <- mc %>%
  select(dupersid, panel02, icd9codx, rxnum) %>%
  mutate(mepsid = if_else(panel02 < 10,
                          paste(0, dupersid, panel02, sep = ""),
                          paste(dupersid, panel02, sep = ""))) %>%
  filter(icd9codx == 428) %>%
  select(dupersid, mepsid, rxnum)

#use mc_hf filter PM to only include people with heart failure --> filter only for drugs of interest --> recode to consolidate ACEi/ARB
pm %>%
  filter(tc1s1 %in% c(47, 42, 56, 340)| grepl('SPIRONOLACTONE|EPLERENONE',rxname)) %>%
  count(tc1s1, tc1s2)

pm_hf <- pm %>%
  filter(dupersid %in% mc_hf$dupersid) %>%
  filter(tc1s1 %in% c(47, 42, 56) | grepl('SPIRONOLACTONE|EPLERENONE',rxname)) %>%
  mutate(tc1s1 = recode(tc1s1, `42` = 42,
                        `56` = 42,
                        `47` = 47,
                        `49` = 49,
                        `55` = 49))

#try to create ideal data frame for just beta blockers --> beta blocker ID var | bb n_presc | bb tot_exp | bb oop_exp
pm_bb <- pm_hf %>%
  filter(tc1s1 == 47) %>%
  group_by(dupersid) %>%
  summarise(bb_n_presc = n(),
            bb_exp_tot = sum(rxxp02x),
            bb_exp_oop = sum(rxsf02x)) %>%
  mutate(bb = 1) %>%
  select(bb, everything())

pm_ace <- pm_hf %>%
  filter(tc1s1 == 42) %>%
  group_by(dupersid) %>%
  summarise(ace_n_presc = n(),
            ace_exp_tot = sum(rxxp02x),
            ace_exp_oop = sum(rxsf02x)) %>%
  mutate(ace = 1)%>%
  select(ace, everything())

pm_aldo <- pm_hf %>%
  filter(tc1s1 == 49) %>%
  group_by(dupersid) %>%
  summarise(aldo_n_presc = n(),
            aldo_exp_tot = sum(rxxp02x),
            aldo_exp_oop = sum(rxsf02x)) %>%
  mutate(aldo = 1)%>%
  select(aldo, everything())

test <- full_join(pm_ace, pm_bb, by = "dupersid") %>%
  full_join(pm_aldo, by = "dupersid") 

test[is.na(test)] <- 0
data <- test

d <- full_join(data, mc_hf, by = "dupersid") %>% 
  distinct() %>%
  left_join(meps, by = "mepsid")

d[is.na(d)] <- 0
d_2002 <- d  
rm(list = setdiff(ls(), "d_2002"))
save(d_2002, file = "d_2002.RData")
