
#' Fit Bayesian Linear Model
#'
#' @param btbr_rs A previously created \link[btbr_sediment_randomsamples]{btbr} object.
#' @param linear Logical. Whether to run without spline or not, TRUE (default).
#' @return A `brmsfit` model object.
#'
#' @export
#' @examples
#' data <- btbr_tss()
#'
#' granitic_dist <- btbr_batch_distribution(data %>% dplyr::filter(geology == 'Granitic'),
#'                                          value = value_tons_mi2_yr,
#'                                          method = 'mge')
#'
#'
#' sedimentary_dist <- btbr_batch_distribution(data %>% dplyr::filter(geology == 'Sedimentary'),
#'                                             value = value_tons_mi2_yr,
#'                                             method = 'mge')
#'
#' btbr_rs <- btbr_sediment_randomsamples(usfs = TRUE,
#'                                        sedimentary_dist = sedimentary_dist[['lpearson']],
#'                                        granitic_dist = granitic_dist[['lognorm']])
#' btbr_sedmod <- btbr_brm(btbr_rs)

btbr_brm_sediment <- function(btbr_rs, linear = TRUE) {

if(linear){
  sed_mod <- brms::brm(
    proportion ~ spec_delFS + natural_erosion + ig_or_not,
    family = brms::Beta(),
    data = btbr_rs
  )

} else {

  sed_mod <- brms::brm(
    proportion ~ spec_delFS*splines::ns(natural_erosion, 2) + ig_or_not,
    family = brms::Beta(),
    data = btbr_rs
  )

}


}

#' Randomly Sample Total Suspended Sediment (TSS) Distributions
#'
#' @description
#' This is a **very** specific function used for the BTB model so likely won't be useful for others. The outputs of this function are used to
#' for proportion of road-related sediment to natural erosion in the Bayesian linear model.
#'
#' @param type A logical. Whether to use USFS land GRAIP_Lite National Model Runs. TRUE (default)
#' @param sedimentary_dist A previously created \link[btbr]{btbr_batch_distribution} list with selected distribution.
#' @param granitic_dist A previously created \link[btbr]{btbr_batch_distribution} list with selected distribution.
#' @return A \link[dplyr]{tibble} with new variable `proportion`.
#' @export
#'
#' @examples
#'
#' data <- btbr_tss()
#'
#' granitic_dist <- btbr_batch_distribution(data %>% dplyr::filter(geology == 'Granitic'),
#'                                          value = value_tons_mi2_yr,
#'                                          method = 'mge')
#'
#'
#' sedimentary_dist <- btbr_batch_distribution(data %>% dplyr::filter(geology == 'Sedimentary'),
#'                                             value = value_tons_mi2_yr,
#'                                             method = 'mge')
#'
#' btbr_rs <- btbr_sediment_randomsamples(usfs = TRUE,
#'                                        sedimentary_dist = sedimentary_dist[['lpearson']],
#'                                        granitic_dist = granitic_dist[['lognorm']])

btbr_sediment_randomsamples <- function(usfs = TRUE, sedimentary_dist, granitic_dist) {


  btb_hucs <- btbr_hucs(usfs = usfs)

  set.seed(1234)

  nhucs <- nrow(btb_hucs)

  sed_sim <- dplyr::tibble(spec_delFS = ifelse(btb_hucs$ig_or_not == 'sedimentary',
                                        {b <- btb_hucs$tlenFS
                                        error <- rnorm(nhucs, sd = 1/(b+0.001))
                                        a <- btb_hucs$specdelFS_HA*1000*14*error
                                        (btb_hucs$specdelFS_HA*1000*14 + a)*0.00285497 # converting to tons and using sedimentary geologic base rate from GRAIP_Lite
                                        },{
                                          b <- btb_hucs$tlenFS
                                          error <- rnorm(nhucs, sd = 1/(b+0.001))
                                          a <- btb_hucs$specdelFS_HA*1000*21*error
                                          (btb_hucs$specdelFS_HA*1000*21.3 + a)*0.00285497 # converting to tons and using granitic geologic base rate from GRAIP_Lite
                                        }),
                    natural_erosion = ifelse(btb_hucs$ig_or_not == 'sedimentary',
                                      smwrBase::rlpearsonIII(10000,
                                                             meanlog = as.numeric(sedimentary_dist$estimate['meanlog']),
                                                             sdlog = as.numeric(sedimentary_dist$estimate['sdlog']),
                                                             skew = as.numeric(sedimentary_dist$estimate['skew'])),
                                      rlnorm(10000,
                                             meanlog = as.numeric(granitic_dist$estimate['meanlog']),
                                             sdlog = as.numeric(granitic_dist$estimate['sdlog'])
                                                                  )),
                    road_length = btb_hucs$tlenFS,

                    ig_or_not = btb_hucs$ig_or_not,

                    huc12 = btb_hucs$HUC_12,

                    og_spec = ifelse(
                                     btb_hucs$ig_or_not == 'sedimentary',
                                     btb_hucs$specdelFS_HA*1000*14*0.00285497, # converting to tons and using sedimentary geologic base rate from GRAIP_Lite
                                     btb_hucs$specdelFS_HA*1000*21.3*0.00285497 # converting to tons and using granitic geologic base rate from GRAIP_Lite
                                     )
                                    ) %>%
                                      dplyr::mutate(
                                            spec_delFS = dplyr::case_when(spec_delFS <= 0 & road_length == 0 ~ 0,
                                                                          spec_delFS <= 0 & road_length > 0 ~ 0.01,
                                                                          TRUE ~ spec_delFS),
                                            proportion = spec_delFS/(spec_delFS+natural_erosion)) %>%
                                            dplyr::filter(proportion > 0,
                                                          proportion < 1
                                                    ) # removing zero's to avoid zero inflation potentially....

}



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
#' @examples
#' granitic_dist <- btbr_batch_distribution(data %>% dplyr::filter(geology == 'Granitic'),
#'                                          value = value_tons_mi2_yr,
#'                                          method = 'mge')
#'
#'
#' sedimentary_dist <- btbr_batch_distribution(data %>% dplyr::filter(geology == 'Sedimentary'),
#'                                             value = value_tons_mi2_yr,
#'                                             method = 'mge')
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


#' Get Total Suspended Sediment (TSS) Yields
#'
#' @description
#' This function returns Total Suspended Sediment (TSS) yields in Tons/mi<sup>2</sup>/yr and Kg/km<sup>2</sup>/yr for different geology types.
#' These observations are then used to generate parameters for theoretical probability distribution functions (PDF) used in the
#' Bull Trout Baseline model.
#'
#' @param geology A character option for returning geology type, e.g. 'all' (default), 'granitic', 'sedimentary'.
#'
#' @return A \link[dplyr]{tibble} with columns `value_tons_mi2_yr` and `value_tons_km2_yr` and associated attributes, e.g. area, total_samples, etc.
#'
#' @details
#' The `geology` methods currently available:
#' \itemize{
#' \item  \strong{granitic}: These values are taken from \insertCite{kirchner2001mountain}{btbr} and 'personal communication' from \insertCite{smith1984suspended}{btbr}.
#' \item  \strong{sedimentary}: These values are taken from \insertCite{region1tss2024}{btbr}.
#' }
#' @export
#' @references {
#' \insertAllCited{}
#' }
#' @importFrom Rdpack reprompt
#' @importFrom dplyr "%>%"
#' @examples
#' data <- btbr_tss()
#'
btbr_tss <- function(geology = 'all') {



  switch(geology,

         'sedimentary' = {

           knf_data <- btbr_knfdata()

           fnf_data <- btbr_fnfdata()

           dplyr::bind_rows(knf_data, fnf_data) %>% dplyr::mutate(geology = 'Sedimentary')

         },

         'granitic' = {

           btbr_granitic() %>% dplyr::mutate(geology = 'Granitic')

         },

         'all' = {

           knf_data <- btbr_knfdata()

           fnf_data <- btbr_fnfdata()

           sedimentary <- dplyr::bind_rows(knf_data, fnf_data) %>% dplyr::mutate(geology = 'Sedimentary')

           granitic <- btbr_granitic() %>% dplyr::mutate(geology = 'Granitic')

           dplyr::bind_rows(sedimentary, granitic)

         }

         )

}


#' Get Flathead National Forest TSS
#'
#' @return A tibble.
btbr_fnfdata <- function() {

  fnf_data <- dplyr::tribble(
    ~station_number, ~station_name,                  ~area_mi2, ~year, ~value_tons_mi2_yr, ~value_tons_km2_yr, ~area_km2,  ~total_samples,
    "FL1027",        "Goat Creek",                   20.7,      1987,  1.6,                560.4256,         53.61279,   5,
    "FL1027",        "Goat Creek",                   20.7,      1988,  1.98,               693.52668,        53.61279,   5,
    "FL1027",        "Goat Creek",                   20.7,      1989,  8.61,               3015.79026,       53.61279,   5,
    "FL1027",        "Goat Creek",                   20.7,      1990,  8,                  2802.128,         53.61279,   5,
    "FL1027",        "Goat Creek",                   20.7,      1991,  12.4,               4343.2984,        53.61279,   5,
    "FL1028",        "Elk Creek Estimated",          23.6,      1987,  6.3,                2206.6758,        61.12376,   5,
    "FL1028",        "Elk Creek Estimated",          23.6,      1988,  6.91,               2420.33806,       61.12376,   5,
    "FL1028",        "Elk Creek Estimated",          23.6,      1989,  21.75,              7618.2855,        61.12376,   5,
    "FL1028",        "Elk Creek Estimated",          23.6,      1990,  15.4,               5394.0964,        61.12376,   5,
    "FL1028",        "Elk Creek Estimated",          23.6,      1991,  35.4,               12399.4164,       61.12376,   5,
    "FL1029",        "Lion Creek",                   24.1,      1987,  2.2,                770.5852,         62.41876,   5,
    "FL1029",        "Lion Creek",                   24.1,      1988,  16.43,              5754.87038,       62.41876,   5,
    "FL1029",        "Lion Creek",                   24.1,      1989,  6.09,               2133.11994,       62.41876,   5,
    "FL1029",        "Lion Creek",                   24.1,      1990,  8.2,                2872.1812,        62.41876,   5,
    "FL1029",        "Lion Creek",                   24.1,      1991,  14.4,               5043.8304,        62.41876,   5,
    "FL4004",        "Sullivan Creek",               46.3,      1978,  42.89,              15022.90874,      119.91654,  9,
    "FL4004",        "Sullivan Creek",               46.3,      1979,  8.77,               3071.83282,       119.91654,  9,
    "FL4004",        "Sullivan Creek",               46.3,      1980,  1.45,               507.8857,         119.91654,  9,
    "FL4004",        "Sullivan Creek",               46.3,      1983,  4.1,                1436.0906,        119.91654,  9,
    "FL4004",        "Sullivan Creek",               46.3,      1985,  6.7,                2346.7822,        119.91654,  9,
    "FL4004",        "Sullivan Creek",               46.3,      1986,  21.77,              7625.29082,       119.91654,  9,
    "FL4004",        "Sullivan Creek",               46.3,      1987,  7.1,                2486.8886,        119.91654,  9,
    "FL4004",        "Sullivan Creek",               46.3,      1988,  1.43,               500.88038,        119.91654,  9,
    "FL4004",        "Sullivan Creek",               46.3,      1989,  10.85,              3800.3861,        119.91654,  9,
    "FL7002",        "Whale Creek Lower",            52.8,      1983,  14.18,              4966.77188,       136.75147,  9,
    "FL7002",        "Whale Creek Lower",            52.8,      1984,  6.85,               2399.3221,        136.75147,  9,
    "FL7002",        "Whale Creek Lower",            52.8,      1985,  31.03,              10868.75398,      136.75147,  9,
    "FL7002",        "Whale Creek Lower",            52.8,      1986,  34.84,              12203.26744,      136.75147,  9,
    "FL7002",        "Whale Creek Lower",            52.8,      1987,  6.4,                2241.7024,        136.75147,  9,
    "FL7002",        "Whale Creek Lower",            52.8,      1988,  6.57,               2301.24762,       136.75147,  9,
    "FL7002",        "Whale Creek Lower",            52.8,      1989,  12.3,               4308.2718,        136.75147,  9,
    "FL7002",        "Whale Creek Lower",            52.8,      1990,  14.7,               5148.9102,        136.75147,  9,
    "FL7002",        "Whale Creek Lower",            52.8,      1991,  20.1,               7040.3466,        136.75147,  9,
    "FL7009",        "Coal Creek NF",                20.8,      1982,  12.11,              4241.72126,       53.87179,   10,
    "FL7009",        "Coal Creek NF",                20.8,      1983,  8.18,               2865.17588,       53.87179,   10,
    "FL7009",        "Coal Creek NF",                20.8,      1984,  9.29,               3253.97114,       53.87179,   10,
    "FL7009",        "Coal Creek NF",                20.8,      1985,  11.69,              4094.60954,       53.87179,   10,
    "FL7009",        "Coal Creek NF",                20.8,      1986,  15.88,              5562.22408,       53.87179,   10,
    "FL7009",        "Coal Creek NF",                20.8,      1987,  4.1,                1436.0906,        53.87179,   10,
    "FL7009",        "Coal Creek NF",                20.8,      1988,  5.48,               1919.45768,       53.87179,   10,
    "FL7009",        "Coal Creek NF",                20.8,      1989,  21.92,              7677.83072,       53.87179,   10,
    "FL7009",        "Coal Creek NF",                20.8,      1990,  18.1,               6339.8146,        53.87179,   10,
    "FL7009",        "Coal Creek NF",                20.8,      1991,  18.4,               6444.8944,        53.87179,   10,
    "FL7010",        "Coal Creek SF",                14.1,      1983,  11.96,              4189.18136,       36.51886,   9,
    "FL7010",        "Coal Creek SF",                14.1,      1984,  12.85,              4500.9181,        36.51886,   9,
    "FL7010",        "Coal Creek SF",                14.1,      1985,  15.66,              5485.16556,       36.51886,   9,
    "FL7010",        "Coal Creek SF",                14.1,      1986,  10.8,               3782.8728,        36.51886,   9,
    "FL7010",        "Coal Creek SF",                14.1,      1987,  3.2,                1120.8512,        36.51886,   9,
    "FL7010",        "Coal Creek SF",                14.1,      1988,  5.6,                1961.4896,        36.51886,   9,
    "FL7010",        "Coal Creek SF",                14.1,      1989,  9.63,               3373.06158,       36.51886,   9,
    "FL7010",        "Coal Creek SF",                14.1,      1990,  6.7,                2346.7822,        36.51886,   9,
    "FL7010",        "Coal Creek SF",                14.1,      1991,  25.5,               8931.783,         36.51886,   9,
    "FL7011",        "Coal Creek Lower",             45.9,      1983,  9.19,               3218.94454,       118.88054,  9,
    "FL7011",        "Coal Creek Lower",             45.9,      1984,  15.46,              5415.11236,       118.88054,  9,
    "FL7011",        "Coal Creek Lower",             45.9,      1985,  79.65,              27898.6869,       118.88054,  9,
    "FL7011",        "Coal Creek Lower",             45.9,      1986,  24.38,              8539.48508,       118.88054,  9,
    "FL7011",        "Coal Creek Lower",             45.9,      1987,  27.1,               9492.2086,        118.88054,  9,
    "FL7011",        "Coal Creek Lower",             45.9,      1988,  17.45,              6112.1417,        118.88054,  9,
    "FL7011",        "Coal Creek Lower",             45.9,      1989,  28.6,               10017.6076,       118.88054,  9,
    "FL7011",        "Coal Creek Lower",             45.9,      1990,  12.5,               4378.325,         118.88054,  9,
    "FL7011",        "Coal Creek Lower",             45.9,      1991,  40.9,               14325.8794,       118.88054,  9,
    "FL7012",        "Big Creek at Lookout Bridge",  68.6,      1986,  199.82,             69990.15212,      177.67331,  6,
    "FL7012",        "Big Creek at Lookout Bridge",  68.6,      1987,  133.4,              46725.4844,       177.67331,  6,
    "FL7012",        "Big Creek at Lookout Bridge",  68.6,      1988,  8.37,               2931.72642,       177.67331,  6,
    "FL7012",        "Big Creek at Lookout Bridge",  68.6,      1989,  23.7,               8301.3042,        177.67331,  6,
    "FL7012",        "Big Creek at Lookout Bridge",  68.6,      1990,  41.3,               14465.9858,       177.67331,  6,
    "FL7012",        "Big Creek at Lookout Bridge",  68.6,      1991,  81.3,               28476.6258,       177.67331,  6,
    "FL8002",        "Gregg Creek",                  8.6,       1978,  4.89,               1712.80074,       22.27391,   11,
    "FL8002",        "Gregg Creek",                  8.6,       1979,  2.39,               837.13574,        22.27391,   11,
    "FL8002",        "Gregg Creek",                  8.6,       1980,  3.29,               1152.37514,       22.27391,   11,
    "FL8002",        "Gregg Creek",                  8.6,       1981,  3.5,                1225.931,         22.27391,   11,
    "FL8002",        "Gregg Creek",                  8.6,       1984,  1.42,               497.37772,        22.27391,   11,
    "FL8002",        "Gregg Creek",                  8.6,       1986,  3.31,               1159.38046,       22.27391,   11,
    "FL8002",        "Gregg Creek",                  8.6,       1987,  0.8,                280.2128,         22.27391,   11,
    "FL8002",        "Gregg Creek",                  8.6,       1988,  0.43,               150.61438,        22.27391,   11,
    "FL8002",        "Gregg Creek",                  8.6,       1989,  6.81,               2385.31146,       22.27391,   11,
    "FL8002",        "Gregg Creek",                  8.6,       1990,  7.4,                2591.9684,        22.27391,   11,
    "FL8002",        "Gregg Creek",                  8.6,       1991,  15.9,               5569.2294,        22.27391,   11,
    "FL8020",        "Squaw Meadows Creek",          12.5,      1980,  1.23,               430.82718,        32.37487,   12,
    "FL8020",        "Squaw Meadows Creek",          12.5,      1981,  5.22,               1828.38852,       32.37487,   12,
    "FL8020",        "Squaw Meadows Creek",          12.5,      1982,  1.1,                385.2926,         32.37487,   12,
    "FL8020",        "Squaw Meadows Creek",          12.5,      1983,  4.7,                1646.2502,        32.37487,   12,
    "FL8020",        "Squaw Meadows Creek",          12.5,      1984,  0.82,               287.21812,        32.37487,   12,
    "FL8020",        "Squaw Meadows Creek",          12.5,      1985,  0.74,               259.19684,        32.37487,   12,
    "FL8020",        "Squaw Meadows Creek",          12.5,      1986,  0.81,               283.71546,        32.37487,   12,
    "FL8020",        "Squaw Meadows Creek",          12.5,      1987,  0.5,                175.133,          32.37487,   12,
    "FL8020",        "Squaw Meadows Creek",          12.5,      1988,  0.2,                70.0532,          32.37487,   12,
    "FL8020",        "Squaw Meadows Creek",          12.5,      1989,  4.36,               1527.15976,       32.37487,   12,
    "FL8020",        "Squaw Meadows Creek",          12.5,      1990,  5.1,                1786.3566,        32.37487,   12,
    "FL8020",        "Squaw Meadows Creek",          12.5,      1991,  4.3,                1506.1438,        32.37487,   12
  )

  fnf_data %>%
    dplyr::group_by(station_number) %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with('value_'), ~mean(.x)),
                  value_tons_km2_yr = value_tons_km2_yr/1000) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(-year) %>%
    dplyr::relocate(area_km2, .before = value_tons_km2_yr)
}


#' Get Kootenai National Forest TSS
#'
#' @return A tibble.
btbr_knfdata <- function() {

  cabinet_data <- dplyr::tribble(
    ~station_name, ~year, ~value_tons_mi2_yr,
    'Trout Creek', 2008, 10282,
    'Trout Creek', 2009, 3257,
    'Trout Creek', 2010, 10413,
    'Trout Creek', 2011, 10262,
    'Trout Creek', 2012, 12613,
    'Trout Creek', 2013, 4161,
    'Trout Creek', 2014, 8300,
    'Trout Creek', 2015, 3807,
    'Trout Creek', 2016, 925,
    'Trout Creek', 2017, 2914,
    'Trout Creek', 2018, 3149,
    'Trout Creek', 2019, 1091,
    'Trout Creek', 2020, 1579,
    'Trout Creek', 2021, 673,
    'Vermillion River', 2008, 46575,
    'Vermillion River', 2009, 82172,
    'Vermillion River', 2010, 4977,
    'Vermillion River', 2011, 20869,
    'Vermillion River', 2012, 28390,
    'Vermillion River', 2013, 7082,
    'Vermillion River', 2014, 3358,
    'Vermillion River', 2015, 3496,
    'Vermillion River', 2016, 2239,
    'Vermillion River', 2017, 2605,
    'Vermillion River', 2019, 4357,
    'Vermillion River', 2020, 6746,
    'Vermillion River', 2021,2647,
    'Vermillion River', 2022,2624
  ) %>% dplyr::mutate(station_number = dplyr::if_else(station_name == 'Trout Creek', 'KNF01', 'KNF02'),
              area_mi2 = dplyr::if_else(station_name == 'Trout Creek', 47.25, 102),
              value_tons_mi2_yr = dplyr::if_else(station_name == 'Trout Creek', value_tons_mi2_yr/47.25, value_tons_mi2_yr/102),
              value_tons_km2_yr = (value_tons_mi2_yr*350.266)/1000,
              area_km2 = area_mi2*2.58999) %>%
              dplyr::add_count(station_number, name = 'total_samples')

  cabinet_data <- cabinet_data %>%
                  dplyr::group_by(station_number) %>%
                  dplyr::mutate(dplyr::across(dplyr::starts_with('value_'), ~mean(.x))) %>%
                  dplyr::slice(1) %>%
                  dplyr::ungroup() %>%
                  dplyr::select(-year) %>%
                  dplyr::select(station_number,station_name, area_mi2,
                                value_tons_mi2_yr, area_km2, value_tons_km2_yr,
                                total_samples) %>%
                  dplyr::relocate(area_km2, .before = value_tons_km2_yr)

  libby_data <- dplyr::tribble(
                        ~`Sub Basin`,	~`Station name`, ~`Area (mi2)`,	~total_samples, ~`Mean Daily Q (cfs)`, ~`Mean Daily suspended yield (tons/day)`,	~`Mean Annual total  suspended yield (tons)`,
                        'mid Kootenai',	'Quartz Creek',	35.8,	17,	74.9,	3.15,	1149.75,
                        'Fisher River',	'Himes Creek',	10.4,	19,	15.5,	0.25,	91.25,
                        'mid Kootenai',	'Pipe Creek',		105.8,	10,	59,	2.04,	745,
                        'mid Kootenai',	'Flower Creek',	11.2,	15,	25.4,	1.14,	416.1
                        ) %>%
                        janitor::clean_names() %>%
                        dplyr::mutate(value_tons_mi2_yr = mean_annual_total_suspended_yield_tons/area_mi2,
                               value_tons_km2_yr = (value_tons_mi2_yr*350.266)/1000,
                               station_number = c('KNF03', 'KNF04', 'KNF05', 'KNF06'),
                               area_km2 = area_mi2*2.58999) %>%
                        dplyr::select(station_number,station_name, area_mi2,
                                      value_tons_mi2_yr, area_km2, value_tons_km2_yr,
                                      total_samples) %>%
                        dplyr::relocate(area_km2, .before = value_tons_km2_yr)

  dplyr::bind_rows(cabinet_data, libby_data)
}

#' Get Granitic TSS
#'
#' @return A tibble.
btbr_granitic <- function() {

 dplyr::tribble(
    ~station_name,~station_number, ~area_km2, ~total_samples, ~value_tons_km2_yr,
    "Trapper Creek" ,'NPCW01' ,20, 10, 9.8,
    "South Fk. Red River",'NPCW03', 98, 14, 8.0,
    "Upper Red River",'NPCW03', 129, 14, 10.1 ,
    "Johns Creek",'NPCW05', 293, 10, 7.6,
    "West Fork",'NPCW06', 17, 23, 5,
    "East Fork",'NPCW07', 14, 23, 2.5,
    "Tailholt Main",'NPCW08', 6.6, 28, 14
  ) %>%
    dplyr::mutate(value_tons_mi2_yr = value_tons_km2_yr*1000*0.00285497, # convert to tons/mi2/yr
                  area_mi2 = area_km2*0.386102 # convert to mi2
                  ) %>%
    dplyr::bind_rows(dplyr::tribble(
      ~station_name,~station_number, ~area_km2, ~total_samples, ~value_tons_mi2_yr,
      "Martin Creek", 'BNF01', NA, NA, 11.81,
      "Meadow Creek", 'BNF02', NA, NA, 7.91,
      "Moose Creek", 'BNF03', NA, NA, 5.81,
      "Paint Creek", 'BNF04', NA, NA, 58.7,
      "Tolan Creek", 'BNF05', NA, NA, 10.29,
      "Warm Springs", 'BNF06', NA, NA, 6.61
    )%>%
      dplyr::mutate(value_tons_km2_yr = (value_tons_mi2_yr*350.266)/1000, # convert to kg/km2/yr
                    area_mi2 = area_km2*0.386102 # convert to mi2
      ))%>%
    dplyr::select(station_number,station_name, area_mi2,
                  value_tons_mi2_yr, area_km2, value_tons_km2_yr,
                  total_samples)

}
