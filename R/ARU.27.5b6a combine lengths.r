################################################################################
# WGDEEP data overviews
#
# Generating overviews of several stocks assessed within WGDEEP
#
# 18/03/2018 coding during HAWG 2018
# 20/03/2018 added all weight in the catch; added the crayola plots; Note stock trends now 
#            via the SAG download
# 21/03/2019 Updated during HAWG 2019 (MP)
# 02/05/2019 Updated during WGDEEP
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

options(max.print=999999)

# ===================================================================================
# Load datasets 
# ===================================================================================


aru.dir  <- "//community.ices.dk@SSL/DavWWWRoot/ExpertGroups/WGDEEP/2019 Meeting docs/08. Personal folders/LiseHelenOfstad/"
mp.dir  <- "//community.ices.dk@SSL/DavWWWRoot/ExpertGroups/WGDEEP/2019 Meeting docs/08. Personal folders/Martin Pastoors"
# WBSS.data <- get(load(file.path(WBSS.dir,"run/data.RData")))
# WBSS.fit  <- get(load(file.path(WBSS.dir,"run/model.RData" )))

length5b <-
  read.csv(file.path(aru.dir,"LBI/arg5b6a_numbers.csv"), header=TRUE) %>% 
  data.frame() %>% 
  rename(length = Length_cm) %>% 
  gather(key=year, value=prop, X1994:X2018) %>% 
  mutate(year = as.integer(gsub("X","",year)),
         area="5b",
         source="wg")

length6a <-
  read.csv(file.path(aru.dir,"LBI/6a/arg5b6a_numbers.csv"), header=TRUE) %>% 
  data.frame() %>% 
  rename(length = Length_cm) %>% 
  gather(key=year, value=prop, X1994:X2018) %>% 
  mutate(year = as.integer(gsub("X","",year)),
         prop = ifelse(year == 1995 , prop/2, prop),  ## Error in the input file
         area="6a",
         source = "wg")

lengthpfa <-
  read.csv(file.path(mp.dir,"pfa_arg_lengthfrequencies_2015_2019.csv"), header=TRUE) %>% 
  data.frame() %>% 
  mutate(division = gsub("27|\\.","", division)) %>% 
  rename(area=division, number=catchnumber) %>% 
  group_by(species, year, area, length) %>% 
  summarize(number = sum(number, na.rm=TRUE)) %>% 
  group_by(species, year, area) %>% 
  mutate(prop = 100* number / sum(number, na.rm=TRUE),
         number = number/10,
         source="pfa") 


catch <-
  read_excel(path=file.path(aru.dir,"GSS_Landings_Vb_VIa.xlsx"), 
             sheet="GL_landings.csv",
             col_names=TRUE) %>% 
  rename(string = Year) %>% 
  gather(key=year, value=catch, names(.)[2:length(.)]) %>% 
  separate(string, into=c("area","country"), sep=" ") %>% 
  group_by(year, area) %>% 
  summarize(catch = sum(catch, na.rm=TRUE)) %>% 
  group_by(year) %>% 
  mutate(type = "catch") %>% 
  ungroup() %>% 
  mutate(year = as.numeric(year))

raised <-
  bind_rows(length5b, length6a, lengthpfa) %>% 
  left_join(catch, by=c("year", "area")) %>% 
  # mutate(number = ifelse(source == "wg", catch * prop, number))
  mutate(number = catch * prop)

# plot all years (not pfa)
raised %>% 
  mutate(areasource = paste0(area, source)) %>% 
  filter(source == "wg") %>% 
  
  ggplot(aes(x=length, y=number, group=area)) + 
  theme_bw() +
  geom_line(aes(colour=area), size=1) +
  facet_wrap(~year, ncol=9)

# plot
raised %>% 
  mutate(areasource = paste0(area, source)) %>% 
  filter(year %in% 2015:2018, area %in% c("5b", "6a")) %>% 
  filter(!(source == "pfa" & year >= 2017 & area == "5b")) %>% 
  
  ggplot(aes(x=length, y=number, group=source)) + 
  theme_bw() +
  geom_line(aes(colour=source), size=1) +
  facet_grid(area~year)

# plot summed
raised %>% 
  group_by(year, length) %>% 
  summarize(number = sum(number)) %>% 
  ungroup() %>% 
  mutate(year = factor(year),
         length = factor(length)) %>% 
  
  ggplot(aes(x=length, y=number, group=year)) + 
  theme_bw() +
  theme(legend.position="none") +
  geom_bar(aes(fill=year), stat="identity") +
  facet_wrap(~year, ncol=9) 


