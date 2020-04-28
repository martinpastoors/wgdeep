################################################################################
# ARU.27.5b6a plot biomass trends.r
#
# 27/04/2020 WGDEEP 2020
################################################################################

rm(list=ls());

# libraries
library(tidyverse)
library(lubridate)
library(RColorBrewer)

source("R/lowcase.r")
source("R/theme_publication.r")

# ===================================================================================
# Load dataset 
# ===================================================================================

aru.dir  <- "//community.ices.dk/ExpertGroups/WGDEEP/2020 Meeting Docs/06. Data/aru.27.5b6a/CPUE"

df <-
  read.csv(file.path(aru.dir,"aru5b6aIndex_lho.csv")) %>% 
  lowcase()  %>% 
  dplyr::select(year, faroesedeepsurvkgh, faroesesummersurvkgh, scottishdeepsurvkgh, fareucommcombined) %>% 
  pivot_longer(-year, names_to="survey") %>% 
  filter(!is.na(value)) %>% 
  mutate(value2 = ifelse(year >= 2010, value, NA)) %>% 
  group_by(survey) %>% 
  mutate(avg = mean(value2, na.rm=TRUE)) %>% 
  mutate(idx = value/avg -1 ) %>% 
  mutate(idx2 = ifelse(survey=="scottishdeepsurvkgh",idx/2, idx))


 df %>% 
   ggplot(aes(x=year, y=idx, group=survey)) +
   theme_publication() +
   geom_point(aes(colour=survey)) +
   # geom_path(aes(colour=survey)) +
   geom_smooth(aes(colour=survey, fill=survey), alpha=0.4, span=0.8)
 
 
 
 

  


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


