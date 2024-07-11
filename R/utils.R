
#' Batched Frequencies
#' @description This function fits a variable (e.g., flow, snow, rain, etc) to a univariate distribution
#' function, e.g. Weibull,Log-pearson type III, GEV, Normal and Lognormal distributions.
#' It is up to the user to decide which distribution function to use and whether any pre-processing is necessary (outliers, mixed populations, etc).
#' @param data A \code{data.frame}.
#' @param value A \code{numeric} unquoted column.
#' @param ... Values to pass to \link[fitdistrplus]{fitdist}
#'
#' @return A \code{data.frame} with return intervals and associated value for Weibull,
#' Log-pearson type III, GEV, Normal and Lognormal distributions.
#' @importFrom dplyr transmute
#' @importFrom stats na.omit
#' @importFrom extRemes fevd
#' @export
#'

btbr_batch_distribution <- function(data, value, ...) {


      max.x <- data %>% transmute(x = {
        {
          value
        }
      }) %>% na.omit()

    lot_outliers <- MGBT::MGBT(max.x$x)$LOThresh

    if (lot_outliers > 0) {
      max.x_lp <- subset(max.x, x > lot_outliers)
      message(paste0('Fitting Log Pearson Type III with values above ', round(lot_outliers, 3)))
    } else {
      max.x_lp <- max.x
    }

    log_mean.x <- mean(log(max.x_lp$x), na.rm = TRUE)
    log_sd.x <- sd(log(max.x_lp$x), na.rm = TRUE)
    skew <- skewed(max.x$x, type = 3, na.rm = TRUE)
    skew.x_pearson <- skewed(max.x_lp$x, type = 3, na.rm = TRUE)
    skew_log <- skewed(log(max.x_lp$x), type = 3, na.rm = TRUE)
    fevd_gev <- extRemes::fevd(max.x$x, time.units = "years", type = "GEV")
    scale_gev <- fevd_gev$results$par[2] %>% unname()
    loc_gev <- fevd_gev$results$par[1] %>% unname()
    shape_gev <- fevd_gev$results$par[3] %>% unname()
    weib <- fitdistrplus::fitdist(max.x$x, distr = "weibull", ...)
    lnorm <- fitdistrplus::fitdist(max.x$x, distr = "lnorm", ...)
    norm <- fitdistrplus::fitdist(max.x$x, distr = "norm", ...)
    lpearson <- fitdistrplus::fitdist(max.x_lp$x, distr = "lpearsonIII", start = list(meanlog = log_mean.x,
                                                                        sdlog = log_sd.x, skew = skew_log), ...)
    gev <- suppressWarnings(fitdistrplus::fitdist(max.x$x, distr = "gev", start = list(loc = loc_gev,
                                                                         scale = scale_gev, shape = shape_gev), ...))
     list(normal = norm,
          lognorm = lnorm,
          weibull = weib,
          gev = gev,
          lpearson = lpearson)

}

#corrected May 7, 2007
#modified October ,2011 to use apply for mean and sd
#modified April, 2012 to return 3 estimates, depending upon type
#partly based upon e1071  skewness and kurtosis
#' Skew
#'
#' @param x numeric vector
#' @param na.rm remove NA's
#' @param type numeric
#'
#' @return Skew from the \link[psych]{psych-package}
#'

skewed <-function (x, na.rm = TRUE,type=3) {
    if (length(dim(x)) == 0) {
      if (na.rm) {
        x <- x[!is.na(x)]
      }
      sdx <- sd(x,na.rm=na.rm)
      mx <- mean(x)
      n <- length(x[!is.na(x)])
      switch(type,
             {skewer <- sqrt(n) *( sum((x - mx)^3,  na.rm = na.rm)/( sum((x - mx)^2,na.rm = na.rm)^(3/2)))}, #case 1
             {skewer <- n *sqrt(n-1) *( sum((x - mx)^3,  na.rm = na.rm)/((n-2) * sum((x - mx)^2,na.rm = na.rm)^(3/2)))}, #case 2
             {skewer <- sum((x - mx)^3)/(n * sd(x)^3) })  #case 3
    } else {

      skewer <- rep(NA,dim(x)[2])
      if (is.matrix(x)) {mx <- colMeans(x,na.rm=na.rm)} else {mx <- apply(x,2,mean,na.rm=na.rm)}
      sdx <- apply(x,2,sd,na.rm=na.rm)
      for (i in 1:dim(x)[2]) {
        n <- length(x[!is.na(x[,i]),i])
        switch(type,
               {skewer[i] <-sqrt(n) *( sum((x[,i] - mx[i])^3,  na.rm = na.rm)/( sum((x[,i] - mx[i])^2,na.rm = na.rm)^(3/2)))}, #type 1
               {skewer[i] <- n *sqrt(n-1) *( sum((x[,i] - mx[i])^3,  na.rm = na.rm)/((n-2) * sum((x[,i] - mx[i])^2,na.rm = na.rm)^(3/2)))},#type 2
               {skewer[i] <- sum((x[,i] - mx[i])^3,  na.rm = na.rm)/(n * sdx[i]^3)} #type 3
        ) #end switch
      } #end loop
    }
    return(skewer)
  }
