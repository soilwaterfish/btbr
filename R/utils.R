
#' Get Posterior Predictive (PP)
#' @description
#' This function takes the results from the \link[btbr_brm]{brbr} model and performs posterior predictive (PP) summaries for
#' each Hydrological Unit Code (HUC12). The PP distribution is then split into three categories: Functioning Acceptably (FA),
#' Functioning at Acceptable Risk (FAR), and Functioning at Unacceptable Risk (FUR) where the cutoffs are 0-0.1 (FA), 0.1-0.3 (FAR), and
#' 0.3-1 (FAR).
#'
#' @param A data.frame with appropriate covariates to predict with from previous model, e.g. `natural_erosion`, `spec_delFS`, `ig_or_not`, etc.
#' @param btbr_brm A previously created \link[btbr_brm]{brbr} model.
#' @param indicator A character defining the type of indicator, e.g. 'sediment', 'temperature'.
#' @note The `indicator` argument is used to split the PP into custom thresholds previously defined.
#' @return A data.frame.
#' @export
#'
#' @examples
btbr_pp <- function(data, btbr_brm, indicator) {


  data <- data %>% dplyr::mutate(id = dplyr::row_number())

  predictions <- brms::posterior_predict(btbr_brm, newdata = data) %>% as.data.frame() %>% dplyr::tibble()

  custom_pp_thresholds <- proportion_function(predictions, indicator = indicator)

  custom_pp_thresholds <- dplyr::left_join(data, custom_pp_thresholds) %>%
                          dplyr::select(-id)


  if(indicator == 'sediment'){
  final_risk <- custom_pp_thresholds %>%
                dplyr::mutate(
                fa = ifelse(road_length == 0, 100, fa),
                far = ifelse(road_length == 0,  0, far),
                fur = ifelse(road_length == 0, 0, fur)
                ) %>%
                tidyr::pivot_longer(c(fa, far, fur)) %>%
                dplyr::group_by(huc12) %>%
                dplyr::mutate(final_risk = max(value),
                              final_risk = ifelse(final_risk == value, name, NA_character_)) %>%
                dplyr::ungroup() %>%
                reframe(huc12, final_risk) %>%
                na.omit()


  custom_pp_thresholds <- custom_pp_thresholds %>%
                          dplyr::mutate(
                          fa = ifelse(road_length == 0, 100, fa),
                          far = ifelse(road_length == 0,  0, far),
                          fur = ifelse(road_length == 0, 0, fur)
                          ) %>%
                          dplyr::left_join(final_risk)
  }

  custom_pp_thresholds
}



#' Posterior Predictive Percentage
#' @description
#' This function creates the custom splits in the PP.
#'
#' @param data A previously created posterior prediction.
#' @param indicator A character defining the type of indicator, e.g. 'sediment', 'temperature'.
#' @return A data.frame
proportion_function <- function(data, indicator) {

  switch(indicator,
         sediment = {
           ratings <- data.frame(fa = vector(), far = vector(), fur = vector(), id = vector())

           for(i in 1:length(data)){
             fa <- sum(data[,i] < 0.1)/nrow(data)
             far <- sum(data[,i] >= 0.1 & data[,i] < 0.3)/nrow(data)
             fur <- sum(data[,i] > 0.3)/nrow(data)

             ratings[i,'fa'] <- fa
             ratings[i,'far'] <- far
             ratings[i,'fur'] <- fur
             ratings[i, 'id'] <- i
           }},
         temperature = {
           ratings <- data.frame(fa = vector(), far = vector(), fur = vector(), id = vector())

           for(i in 1:length(data)){
             fa <- sum(data[,i] < 12)/nrow(data)
             far <- sum(data[,i] >= 12 & data[,i] < 15)/nrow(data)
             fur <- sum(data[,i] > 15)/nrow(data)

             ratings[i,'fa'] <- fa
             ratings[i,'far'] <- far
             ratings[i,'fur'] <- fur
             ratings[i, 'id'] <- i
           }
         })

  ratings

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

#' Get Bull Trout Baseline HUCs
#'
#' @param usfs A logical.
#'
#' @return A sf MULTIPOLYGON.
#' @export
btbr_hucs <- function(usfs = TRUE) {
  if(usfs){

    sf::read_sf(system.file('data/btb_data.gpkg',package = 'btbr'), layer = 'btb_hucs_fs')

  } else {

    sf::read_sf(system.file('data/btb_data.gpkg',package = 'btbr'), layer = 'btb_hucs')

  }

}
