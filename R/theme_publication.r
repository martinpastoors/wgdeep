# publication theme,  updated: 20170704
theme_publication <- function(base_size=14, base_family="Helvetica") {
  # library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title       = element_text(face = "bold",size = rel(1.0), hjust = 0.5),
            plot.margin      = unit(c(10,5,5,5),"mm"),
            plot.background  = element_rect(colour = NA),
            text             = element_text(),
            axis.title       = element_text(face = "bold",size = rel(1)),
            axis.title.y     = element_text(angle=90,vjust =2),
            axis.title.x     = element_text(vjust = -0.2),
            axis.text        = element_text(), 
            axis.line        = element_line(colour="black"),
            axis.ticks       = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            panel.border     = element_rect(colour="black" , size=0.1),
            panel.background = element_rect(colour = NA),
            strip.background = element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text       = element_text(face="bold"),
            legend.key       = element_rect(colour = NA),
            legend.position  = "bottom",
            legend.direction = "horizontal",
            legend.key.size  = unit(0.2, "cm"),
            legend.spacing   = unit(0, "cm"),  # updated from legend.margin which is deprecated
            legend.title     = element_text(face="italic", size=rel(0.8))
    ))
}
