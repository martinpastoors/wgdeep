# The crayola function originates from Einar Hjorleifsson
# Converted into a function, by Martin Pastoors
# 
# requires libraries: tidyverse, RColorBrewer
#
# input: data.frame (df), value_var (value to plot) and title (t)
#
# note: data needs to be in long format: year, age, variable
# note: data needs to contain an age and year column; these will be automatically converted to lowercase

crayola <- function(df, value_var="oC", t="my title") {

  require(tidyverse)
  require(RColorBrewer)

  # to do: check whether the df is in the right format

  # create the colour scheme
  PAIRED <- rep(RColorBrewer::brewer.pal(12, "Paired"), 100)
  
  # make sure that age and year are lowercase, the data is numbers and add yearclass
  df <-
    df %>%
    setNames(gsub("Year|YEAR","year", names(.))) %>% 
    setNames(gsub("Age|AGE","age", names(.))) %>% 
    filter(age >= 0) %>% 
    mutate(
      age  = as.integer(as.character(age)),
      year = as.integer(as.character(year)),
      yc   = year - age
    )

  # number of yearclasses
  n <- length(unique(df$yc))
  
  print(
    df %>%
      ggplot(aes_string("year", y=value_var)) +
      theme_bw() +
      geom_hline(yintercept = 1, col = "grey") +
      geom_bar(aes(fill = factor(yc)), stat = "identity") +
      expand_limits(x = c(min(df$year), max(df$year)), y = 0) +
      scale_fill_manual(values = PAIRED[1:n]) +
      facet_grid(age ~ ., scale = "free_y", switch = "y") +
      theme(legend.position = "none", axis.text.y = element_blank()) +
      labs(x = NULL, y = NULL, title = t) +
      scale_y_continuous(NULL, NULL)
  )
  
  return()

}  # end of the function

# Example
# crayola (df       = filter(df, file=="ARU.27.5b6a_WGDEEP_2020_"), 
#          value_var="oC", 
#          t        ="Observed catch") 

# Example data
# year age         n            f    cW    dW lF    lW    m  mat   oC    oU1 oU2 oU3 oU4 pF pM    sW                     file   yc
# 1995   5  79483.73 0.0003199665 0.225 0.206  1 0.206 0.15 0.13   12     NA  NA  NA  NA  0  0 0.225 ARU.27.5b6a_WGDEEP_2020_ 1990
# 1996   5  84083.36 0.0009739903 0.194 0.215  1 0.215 0.15 0.13   73     NA  NA  NA  NA  0  0 0.194 ARU.27.5b6a_WGDEEP_2020_ 1991
# 1997   5  92809.49 0.0032536005 0.198 0.193  1 0.193 0.15 0.13  306     NA  NA  NA  NA  0  0 0.198 ARU.27.5b6a_WGDEEP_2020_ 1992
# 1998   5 102045.39 0.0097627128 0.189 0.247  1 0.247 0.15 0.13 1696 21.585  NA  NA  NA  0  0 0.189 ARU.27.5b6a_WGDEEP_2020_ 1993
# 1999   5 104375.78 0.0094735596 0.192 0.210  1 0.210 0.15 0.13 1282 17.780  NA  NA  NA  0  0 0.192 ARU.27.5b6a_WGDEEP_2020_ 1994
# 2000   5 106149.04 0.0070904415 0.243 0.284  1 0.284 0.15 0.13  544 19.140  NA  NA  NA  0  0 0.243 ARU.27.5b6a_WGDEEP_2020_ 1995  