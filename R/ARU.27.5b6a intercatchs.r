################################################################################
# ARU.27.5b6a intercatch
#
# Generating overviews of intercatch information
#
# 03/05/2019 Initial coding
################################################################################

rm(list=ls());


# libraries
library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(directlabels)
library(ggmisc) # e.g. for crayola
library(readxl)

# source("_Common/crayola.r")
source("r/lowcase.r")

options(max.print=999999)

# ===================================================================================
# Load datasets 
# ===================================================================================

aru.dir  <- "//community.ices.dk@SSL/DavWWWRoot/ExpertGroups/WGDEEP/2019 Meeting docs/08. Personal folders/LiseHelenOfstad/InterCatch"
lise.dir  <- "//community.ices.dk@SSL/DavWWWRoot/ExpertGroups/WGDEEP/2019 Meeting docs/08. Personal folders/LiseHelenOfstad"

# Numbers at length
file.list <- list.files(aru.dir, pattern = "NumbersAtAgeLength", full.names=TRUE, recursive=TRUE)

t <- data.frame()
# f <- file.list[1]

for (f in file.list) {
  t1 <-
    read.table(f, 
               sep="\t", header=TRUE, skip=2, stringsAsFactors = FALSE) %>% 
    data.frame() %>% 
    dplyr::select(-X) %>% 
    gather(key=length, value=value, 16:ncol(.)) %>% 
    mutate(length = as.numeric(gsub("UndeterminedLngt","", length))/10 ) %>% 
    lowcase() 
  
  t <- bind_rows(t, t1)
}

# length sample summary
eu_lengthsampling <-
  t %>% 
  distinct(stock, year, season, area, country, catchcat, reportcat, fleets, sampledcatch, numagemeasurement,
           numlengthmeasurements, numsamplesage, numsampleslength) 

eu_lengthsampling %>% 
  group_by(stock, year, area, catchcat, country) %>% 
  summarize(numlengthmeasurements = sum(numlengthmeasurements, na.rm=TRUE)) %>% 

  ggplot(aes(x=country, y=numlengthmeasurements, group=catchcat)) +
  theme_bw() +
  geom_bar(aes(fill=catchcat), stat="identity") +
  coord_flip() +
  facet_grid(year~area) 

  
# numbers at length
eu_lengthnumbers <-
  t %>% 
  mutate(value = ifelse(country == "Netherlands" & year == 2017, value*1000, value)) %>% 
  group_by(stock, year, area, catchcat, length) %>% 
  summarize(value = sum(value, na.rm=TRUE))

# t %>% filter(country == "Netherlands" & year == 2017) %>% View()

eu_lengthnumbers %>% 
  ggplot(aes(x=length, y=value, group=catchcat)) +
  theme_bw() +
  geom_line(aes(colour=catchcat)) +
  facet_grid(year~area)


# age length keys
eu_alk <-
  read.table(file.path(aru.dir, "2018/LandingOnly_Age - data.txt"), sep="\t", header=TRUE, skip=2, fill=TRUE) %>% 
  lowcase() %>% 
  filter(!is.na(ageorlength)) %>%
  rename(age = ageorlength, value = numberlanded) %>% 
  mutate(value = ifelse(country == "Netherlands" & year == 2017, value*1000, value)) 
  

eu_alk %>% 
  ggplot(aes(x=age, y=value, group=country)) +
  theme_bw() +
  geom_line(aes(colour=country)) +
  geom_point(aes(colour=country)) +
  facet_grid(year~area)

  

fo_lengthsamples <-
  read.csv(file=file.path(lise.dir, "ToMartin/5b_OverviewTrawlLength1994-2018.csv"), 
           header=TRUE) %>% 
  data.frame() %>% 
  lowcase() %>% 
  gather(key=year, value=count, x1994:x2018) %>% 
  rename(length=lengthcm) %>% 
  mutate(year = gsub("x","", year),
         year = as.integer(year))


fo_lengthsamples %>% 
  group_by(year) %>% 
  summarize(count = sum(count, na.rm=TRUE)) %>% 
  ggplot(aes(x=year, y=count)) +
  theme_bw() +
  geom_line() +
  geom_point() +
  expand_limits(y=0)


fo_agesamples <-
  read.csv(file=file.path(lise.dir, "ToMartin/5b_OverviewAges1994-2018.csv"), 
           header=TRUE) %>% 
  data.frame() %>% 
  lowcase() %>% 
  gather(key=year, value=count, x1994:x2018) %>% 
  mutate(year = gsub("x","", year),
         year = as.integer(year))


fo_agesamples %>% 
  group_by(year) %>% 
  summarize(count = sum(count, na.rm=TRUE)) %>% 
  ggplot(aes(x=year, y=count)) +
  theme_bw() +
  geom_line() +
  geom_point() +
  expand_limits(y=0)


glimpse(fo_lengthsamples)

?read.csv

# stock overview
# stockoverview <-
#   read.table(file.path(aru.dir, "InterCatch/StockOverview.txt"), sep="\t", header=TRUE, skip=2)


