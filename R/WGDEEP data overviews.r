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

rm(list=ls())

# R.version
# find_rtools()

# library(devtools)
# library(pkgbuild)

# Installing the stockassessment package is tricky. Better done in R directly than in RStudio 
# (because there the RTools cannot be found)
# install.packages("Matrix")
# install.packages("ellipse")
# install.packages("TMB")
# devtools::install_github("fishfollower/SAM/stockassessment", ref="components", dependencies=FALSE)

library(stockassessment)

# libraries
library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(directlabels)
library(ggmisc) # e.g. for crayola

# source("_Common/crayola.r")

options(max.print=999999)

# ===================================================================================
# Load datasets 
# ===================================================================================

# Load NSAS data
# load("//community.ices.dk@SSL/DavWWWRoot/ExpertGroups/HAWG/2019 Meeting Docs/06. Data/NSAS/NSH_HAWG2019_sf.Rdata")

# NSH.stock.n <-
#   slot(NSH,"stock.n") %>% 
#   as.data.frame() %>% 
#   dplyr::select(year, age, number = data) %>%
#   filter(number != -1) %>% 
#   mutate(stock = "NSH")

aru.dir  <- "//community.ices.dk@SSL/DavWWWRoot/ExpertGroups/WGDEEP/2019 Meeting docs/08. Personal folders/LiseHelenOfstad/"
# WBSS.data <- get(load(file.path(WBSS.dir,"run/data.RData")))
# WBSS.fit  <- get(load(file.path(WBSS.dir,"run/model.RData" )))

aru.canum <-
  read.ices(file.path(aru.dir,"SAM/cn.dat")) %>% 
  data.frame() %>% 
  rownames_to_column(var="year") %>% 
  gather(key=age, value=number, X5:X21) %>% 
  mutate(age = as.integer(gsub("X","",age)),
         stock="ARU", 
         year=as.numeric(year))

# WBSS.weca <- 
#   read.ices(file.path(WBSS.dir,"data/cw.dat")) %>% 
#   data.frame() %>% 
#   rownames_to_column(var="year") %>% 
#   gather(key=age, value=weight, X0:X8) %>% 
#   filter(!is.na(weight)) %>% 
#   ungroup() %>% 
#   mutate(age = as.integer(gsub("X","",age)),
#          stock="WBSS", 
#          year=an(year)) 
# 
# WBSS.west <- 
#   read.ices(file.path(WBSS.dir,"data/sw.dat")) %>% 
#   data.frame() %>% 
#   rownames_to_column(var="year") %>% 
#   gather(key=age, value=weight, X0:X8) %>% 
#   filter(!is.na(weight)) %>% 
#   ungroup() %>% 
#   mutate(age = as.integer(gsub("X","",age)),
#          stock="WBSS", 
#          year=an(year)) 

# ===================================================================================
# Combine all the data 
# ===================================================================================

canum <-
  bind_rows( aru.canum ) %>% 
  mutate(stock = factor(stock, levels=c("ARU")))

# ===================================================================================
# Plot the crayola of catch at age or stock at age
# ===================================================================================


canum %>% 

  filter(stock %in% c("ARU")) %>% 
  filter(year >= 1960) %>% 
  filter(age %in% 5:20) %>% 
  
  # first calculate the proportions at age
  group_by(stock, year) %>%
  mutate(number = number/sum(number, na.rm=TRUE)) %>%
  
  group_by(stock, year, age) %>% 
  summarise(value = sum(number, na.rm=TRUE)) %>% 
  group_by(stock, age) %>% 
  mutate(value = value/mean(value, na.rm=TRUE)) %>% 
  mutate(yc = year - age) %>% 
  data.frame() %>% 
  
  ggplot() +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.y = element_blank()) +
  theme(panel.border     = element_rect(colour="black" , size=0.1)) +
  theme(axis.ticks.y     = element_blank() ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=10)) +
  theme(panel.spacing = unit(0.2, "lines")) +
  geom_col(aes(year, value, fill = factor(yc))) + 
  scale_fill_crayola() +
  labs(x = NULL, y = NULL, title="Greater silver smelt relative stock at age") +
  facet_grid(age ~ stock, scale = "free_y", switch = "y")


