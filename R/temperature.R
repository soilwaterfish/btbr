#' Fit Bayesian Linear Model with Prediction Error
#'
#' @param tempdata A NorWest Stream Temperature data.frame with `S1_93_11` and `S22_PredSE` columns.
#' @return A `brmsfit` model object.
#'
#' @details
#' This model accounts for measurement error in the covariate \eqn{X_i = \texttt{S1\_93\_11}_i} using its estimated standard error \eqn{\sigma_i = \texttt{S22\_PredSE}_i}. When \eqn{\sigma_i \leq 0} or missing, it is replaced with a default value of 1.
#'
#' The measurement error model is specified using the \code{me()} function in \pkg{brms}, which treats the observed covariate as noisy:
#'
#' \deqn{
#' X_i^{\text{obs}} \sim \mathcal{N}(X_i^{\text{true}}, \sigma_i) \\
#' \log(Y_i) \sim \mathcal{N}(\beta_0 + \beta_1 X_i^{\text{true}}, \sigma)
#' }
#'
#' Where:
#' \itemize{
#'   \item \eqn{X_i^{\text{obs}}} is the observed value of the covariate (\code{S1_93_11})
#'   \item \eqn{\sigma_i} is the known standard error from \code{S22_PredSE}
#'   \item \eqn{X_i^{\text{true}}} is the unobserved true covariate
#'   \item \eqn{Y_i} is the outcome (also \code{S1_93_11}, log-transformed)
#' }
#'
#' The priors are:
#' \deqn{
#' \beta_0 \sim \mathcal{N}(\mu, 0.025) \\
#' \sigma \sim \mathcal{N}(\sigma_{\log}, 0.025)
#' }
#'
#' Here, \eqn{\mu} and \eqn{\sigma_{\log}} are computed from the mean and variance of the observed data, transformed to log scale to match the \code{lognormal} modeling assumption.
#' @export
#' @examples
#' spokoot <- fishguts::get_NorWestStreams('SpoKoot') %>% st_as_sf()
#' huc_comid <- read.csv('data/comid_huc12.csv')
#' spokoot_df <- dplyr::left_join(huc_comid, by = c('COMID' = 'comid')) %>%
#'   dplyr::filter(!is.na(huc12)) %>%
#'   sf::st_drop_geometry() %>%
#'   dplyr::filter(S1_93_11 > 0)
#'
#' temp_model <- btbr_brm_temperature(spokoot_df)

btbr_brm_temperature <- function(tempdata){

  # Clean the standard errors: replace -9999 or negative with 1
  tempdata <- tempdata %>%
    dplyr::mutate(
      S22_PredSE = ifelse(S22_PredSE <= 0, 1, S22_PredSE)
    )

  # Store means for prior estimates
  mean_y <- mean(tempdata[['S1_93_11']], na.rm = TRUE)
  var_y <- var(tempdata[['S1_93_11']], na.rm = TRUE)

  sigma_log <- sqrt(log(var_y / mean_y^2 + 1))
  mu_log <- log(mean_y) - sigma_log^2 / 2

  # Set up data with 'me' column for measurement error
  model_data <- dplyr::tibble(
    Y = tempdata[['S1_93_11']],
    X = tempdata[['S1_93_11']],
    X_se = tempdata[['S22_PredSE']]
  )

  # Fit brms model with measurement error in X
  mod_data <- brms::brm(
    formula = bf(log(Y) ~ 1 + me(X, X_se), sigma ~ 1),
    data = model_data,
    family = gaussian(),
    prior = c(
      prior(normal(mu_log, 0.025), class = Intercept),
      prior(normal(sigma_log, 0.025), class = sigma)
    ),
    sample_prior = "only",
    seed = 1234
  )

  return(mod_data)
}


#' Temp Helper
#'
#' @param data sf LINESTRING object. Temperature data.
#' @param btb_hucs_og Original FS only object.
#'
#' @return A sf object
#'
btbr_temphuc_intersection <- function(data, btb_hucs_og) {


  wmt_norwest_temp_int <- data %>% sf::st_transform(sf::st_crs(btb_hucs_og)) %>% sf::st_intersects(btb_hucs_og)

  wmt_norwest_temp_fs <- data[lengths(wmt_norwest_temp_int) > 0,]

  wmt_norwest_temp_fs <- wmt_norwest_temp_fs %>%
                         dplyr::mutate(stream_length = as.numeric(units::set_units(st_length(.), 'mi')))

  adequate_stream_length <-  wmt_norwest_temp_fs %>%
                            sf::st_drop_geometry() %>%
                            dplyr::group_by(huc12) %>%
                            dplyr::summarise(total_length = sum(stream_length, na.rm = T)) %>%
                            dplyr::filter(total_length > 3) %>%
                            dplyr::pull(huc12)

  wmt_norwest_temp_fs %>%
    dplyr::filter(huc12 %in% adequate_stream_length)



}


#' Get NorWest
#'
#' @description
#' This layer represents modeled stream temperatures derived from the NorWeST point feature class (NorWest_TemperaturePoints) \insertCite{isaak2016norwest}{btbr}.
#' NorWeST summer stream temperature scenarios were developed for all rivers and streams in the western U.S. from the more than 20,000 stream sites in
#' the NorWeST database where mean August stream temperatures were recorded. The resulting dataset includes stream lines (NorWeST_PredictedStreams)
#' and associated mid-points NorWest_TemperaturePoints) representing 1 kilometer intervals along the stream network.
#' Stream lines were derived from the 1:100,000 scale NHDPlus dataset (\insertCite{usepausgs2010nhdplus}{btbr}; \insertCite{mckay2012nhdplus}{btbr}).
#' Shapefile extents correspond to NorWeST processing units, which generally relate to 6 digit (3rd code) hydrologic
#' unit codes (HUCs) or in some instances closely correspond to state borders.
#' The line and point shapefiles contain identical modeled stream temperature results.
#' The two feature classes are meant to complement one another for use in different applications.
#'
#' In addition, spatial and temporal covariates used to generate the modeled temperatures are included in the attribute tables at [](https://www.fs.usda.gov/rm/boise/AWAE/projects/NorWeST/ModeledStreamTemperatureScenarioMaps.shtml).
#' The NorWeST NHDPlusV1 processing units include: Salmon, Clearwater, Spokoot, Missouri Headwaters, Snake-Bear, MidSnake, MidColumbia, Oregon Coast,
#'  South-Central Oregon, Upper Columbia-Yakima, Washington Coast, Upper Yellowstone-Bighorn, Upper Missouri-Marias, and Upper Green-North Platte.
#' The NorWeST NHDPlusV2 processing units include: Lahontan Basin, Northern California-Coastal Klamath, Utah, Coastal California, Central California, Colorado, New Mexico, Arizona, and Black Hills.
#'
#' `Copyright Text:` U.S. Forest Service; Rocky Mountain Research Station; Air, Water, and Aquatic Environments Program (AWAE). [](https://www.fs.usda.gov/rm/boise/awae_home.shtml)
#'
#' @param filter_geom an object of class bbox, sfc or sfg used to filter query results based on a predicate function.
#' @param ... Arguments to pass to `arc_select`, see \link[arcgislayers]{arc_select}.
#' @references {
#' \insertAllCited{}
#' }
#' @return A sf object.
#' @export
#'
btbr_norwest_temperature <- function(filter_geom, ...) {

  url <- arcgislayers::arc_open('https://apps.fs.usda.gov/arcx/rest/services/EDW/EDW_NorWeST_StreamTemperatures_01/MapServer/2')

  if(missing(filter_geom)){

    norwest <- arcgislayers::arc_select(url, ...)

  } else {

    norwest <- arcgislayers::arc_select(url, filter_geom = filter_geom, ...)

  }

}
