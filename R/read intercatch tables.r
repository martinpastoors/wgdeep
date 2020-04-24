################################################################################
# Read intercatch tables
#
# Generating overviews of intercatch information
#
# 20/04/2020 Initial coding
################################################################################

rm(list=ls());


# libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(RColorBrewer)
library(pander)
library(reshape2)
library(writexl)

source("r/lowcase.r")
source("r/theme_publication.r")

options(max.print=999999)

# datapath  <- "//community.ices.dk@SSL/DavWWWRoot/ExpertGroups/WGDEEP/2019 Meeting docs/08. Personal folders/LiseHelenOfstad/InterCatch"
datapath <- "D:/iWGDEEP/2020/06. Data/aru.27.5b6a/InterCatch/aru.27.5b6a_all_ 2020-4-20 12_10_35"

# ---------------------------------------------------------------------------------
# Load tables
# ---------------------------------------------------------------------------------

t1 <-     
  read.table(file.path(datapath, "CatchAndSampleDataTables table1.txt"), 
             sep="\t", header=TRUE, skip=0, stringsAsFactors = FALSE) %>% 
  lowcase() %>% 
  filter(caton > 0)
  
t2 <-     
  read.table(file.path(datapath, "CatchAndSampleDataTables table2.txt"), 
             sep="\t", header=TRUE, skip=0, stringsAsFactors = FALSE) %>% 
  lowcase()

# ---------------------------------------------------------------------------------
# Overview of catch and sampling
# ---------------------------------------------------------------------------------

t1 %>% 
  group_by(country, catchcategory, sampledorestimated) %>% 
  summarize_at(c("caton", "nooflengthsamples", "nooflengthmeasured",
            "noofagesamples", "noagereadings"), sum, na.rm = TRUE) %>% 
  mutate(
    caton                   = round(caton / 1000, digits=0),
    noagereadings_kton      = round(noagereadings / (caton/1000), digits=0),
    nooflengthmeasured_kton = round(nooflengthmeasured / (caton/1000), digits=0)
  ) %>% 
  gather(key="variable", value="data", caton:nooflengthmeasured_kton) %>%
  dcast(catchcategory + sampledorestimated + country  ~ variable , 
        value.var="data", sum, margins=c("catchcategory", "sampledorestimated")) %>% 
  
  group_by(catchcategory) %>%
  do(add_row(., .after=0)) %>%
  
  writexl::write_xlsx(., path="table1.xlsx")

  # pandoc.table(., 
  #              style        = "simple",
  #              split.tables = 200, 
  #              # split.cells  = c(rep(7,10)),
  #              justify      = "right",
  #              missing      =" ",
  #              big.mark     = ',', 
  #              round        = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0) )


# ---------------------------------------------------------------------------------
# Overview of age compositions
# ---------------------------------------------------------------------------------

t2 %>% 
  mutate(area = substr(area, 1, 6)) %>% 
  group_by(area, country, ageorlength, sampledorestimated) %>% 
  summarize_at(c("canum"), sum, na.rm = TRUE) %>% 
  mutate(canum = round(canum, digits=0)) %>% 
  gather(key="variable", value="data", canum) %>%
  dcast(area + sampledorestimated + ageorlength  ~ country , 
        value.var="data", sum, margins=c("area", "sampledorestimated")) %>% 
  
  group_by(area) %>%
  do(add_row(., .after=0)) %>%
  
  writexl::write_xlsx(., path="table2.xlsx")

# pandoc.table(., 
#              style        = "simple",
#              split.tables = 200, 
#              # split.cells  = c(rep(7,10)),
#              justify      = "right",
#              missing      =" ",
#              big.mark     = ',', 
#              round        = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0) )


t2 %>% 
  mutate(area = substr(area, 1, 6)) %>% 
  group_by(area, country, ageorlength) %>% 
  summarize_at(c("canum"), sum, na.rm = TRUE) %>% 
  group_by(area, country) %>% 
  mutate(prop = canum/sum(canum, na.rm=TRUE)) %>% 
  mutate(canum = round(canum, digits=0)) %>% 
  gather(key="variable", value="data", c(canum,prop)) %>% 
  
  # filter(sampledorestimated=="Sampled_Distribution") %>% 
  filter(variable=="canum") %>% 
  
  ggplot(aes(x=ageorlength, y=data)) +
  theme_publication() +
  geom_bar(aes(fill=country), stat="identity") +
  # geom_line(aes(colour=country)) +
  facet_wrap(~area)
  

