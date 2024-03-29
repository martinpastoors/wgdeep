#' @title sam process error
#'
#' @description XXX
#'
#' @export
#'
#' @param rbya XXX
#' @param plus_group XXX
#' @param plot_it XXX
#'
sam_process_error <- function(rbya, plus_group=TRUE, plot_it=FALSE) {

  # rbya = stk$rbya

  # dummy
  n.d <- cW <- year <- z.d <- age <- b <- 0

  # ----------------------------------------------------------------------------
  # align the year-classes
  x <- rbya[,c("year","age","n")]
  x$year <- x$year - 1
  x$age <- x$age - 1
  names(x)[3] <- "n.end"
  d <- plyr::join(rbya[,c("year","age","n","m","f","oC","cW")],x, by=c("year","age"))
  d <- d[!is.na(d$n.end),]

  # ----------------------------------------------------------------------------
  # exclude plus-group
  if(plus_group) d <- d[d$age < max(d$age),]

  # ----------------------------------------------------------------------------
  # process error expressed as mortality
  d$z.n <- log(d$n/d$n.end)
  d$z.f <- d$f + d$m
  d$z.d  <- d$z.n - d$z.f

  # ----------------------------------------------------------------------------
  # process error expressed as numbers
  d$n.end2 <- d$n * exp(-(d$f + d$m))
  # Calculate the difference
  d$n.d <- d$n.end - d$n.end2

  # ----------------------------------------------------------------------------
  # process errror expressed as biomass
  x <- plyr::ddply(d,c("year"),plyr::summarise,b=sum(n.d * cW,na.rm=TRUE))
  x <- plyr::ddply(d,c("year"),plyr::summarise,b=sum(n.d * cW,na.rm=TRUE))
  
  ss <- plyr::ddply(d,c("year"),plyr::summarise,b=sum(oC * cW,na.rm=TRUE))

  x$var<-"PE"
  ss$var<-"Catches"
  x<-rbind(x,ss)

  if(plot_it) {
    mort <- ggplot2::ggplot(d,ggplot2::aes(year,z.d)) +
      ggplot2::theme_bw() +
      ggplot2::geom_text(ggplot2::aes(label=age)) +
      ggplot2::stat_smooth(span=0.1) +
      ggplot2::labs(x="",y="",title="Process error expressed as deviations in mortality")
    abun <- ggplot2::ggplot(d,ggplot2::aes(year,n.d)) +
      ggplot2::theme_bw() +
      ggplot2::geom_bar(stat="identity") +
      ggplot2::facet_wrap(~ age, scale="free_y") +
      ggplot2::labs(x="", y="",title="Process error expressed as deviations in number of fish")

    mass <- ggplot2::ggplot(x[x$year <= 2018,],ggplot2::aes(x=year,y=b,fill=var)) +
      ggplot2::theme_bw() +
      ggplot2::geom_bar(stat="identity", position = "dodge") 
      ggplot2::labs(x="",y="Mass",title="Process error expressed as deviation in mass")

    return(list(rbya=d,rby=x[x$var=="PE",1:2],mort=mort,abun=abun,mass=mass))
  }

  return(list(rbya=d, rby=x))

}





