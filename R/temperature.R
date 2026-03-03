#' Fit Bayesian Distribution Model with Known Prediction Error
#'
#' @param tempdata A NorWest Stream Temperature data.frame with `S1_93_11` and `S22_PredSE` columns.
#' @return A `brmsfit` model object.
#'
#' @details
#' This model characterizes the distribution of stream temperatures while accounting for
#' known prediction standard errors from the NorWest model. The goal is to generate a
#' posterior predictive distribution that can be used for probabilistic inference
#' (e.g., P(temperature > threshold)) rather than point estimates with confidence intervals.
#'
#' When \eqn{\sigma_i \leq 0} or missing, it is replaced with a default value of 1.
#'
#' The model uses the \code{mi()} function in \pkg{brms} to incorporate known measurement
#' error on the response variable:
#'
#' \deqn{
#' Y_i^{\text{obs}} \sim \mathcal{N}(Y_i^{\text{true}}, \sigma_i^{\text{pred}}) \\
#' \log(Y_i^{\text{true}}) \sim \mathcal{N}(\mu, \sigma)
#' }
#'
#' Where:
#' \itemize{
#'   \item \eqn{Y_i^{\text{obs}}} is the observed/predicted stream temperature (\code{S1_93_11})
#'   \item \eqn{\sigma_i^{\text{pred}}} is the known prediction standard error (\code{S22_PredSE})
#'   \item \eqn{Y_i^{\text{true}}} is the unobserved true temperature
#'   \item \eqn{\mu} is the mean of the log-transformed true temperatures
#'   \item \eqn{\sigma} is the standard deviation of the log-transformed true temperatures
#' }
#'
#' The posterior distribution accounts for both:
#' \itemize{
#'   \item Natural variability in stream temperatures across hydrological units (\eqn{\sigma})
#'   \item Prediction uncertainty from the NorWest model (\eqn{\sigma_i^{\text{pred}}})
#' }
#'
#' The priors are derived from the empirical distribution of observed data:
#' \deqn{
#' \mu \sim \mathcal{N}(\hat{\mu}_{\log}, 2 \cdot \hat{\sigma}_{\log}) \\
#' \sigma \sim \text{Student-t}(3, 0, \hat{\sigma}_{\log})
#' }
#'
#' Where \eqn{\hat{\mu}_{\log}} and \eqn{\hat{\sigma}_{\log}} are computed from the mean
#' and variance of the observed data, transformed to the log scale:
#' \deqn{
#' \hat{\sigma}_{\log} = \sqrt{\log\left(\frac{\text{Var}(Y)}{\bar{Y}^2} + 1\right)} \\
#' \hat{\mu}_{\log} = \log(\bar{Y}) - \frac{\hat{\sigma}_{\log}^2}{2}
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' spokoot <- fishguts::get_NorWestStreams('SpoKoot') %>% sf::st_as_sf()
#' huc_comid <- read.csv('data/comid_huc12.csv')
#' spokoot_df <- spokoot %>%
#'   dplyr::left_join(huc_comid, by = c('COMID' = 'comid')) %>%
#'   dplyr::filter(!is.na(huc12)) %>%
#'   sf::st_drop_geometry() %>%
#'   dplyr::filter(S1_93_11 > 0)
#'
#' temp_model <- btbr_brm_temperature(spokoot_df)
#'
#' # Extract posterior samples for probabilistic inference
#' posterior_samples <- brms::posterior_predict(temp_model)
#'
#' # Example: P(temperature > 20)
#' mean(posterior_samples > 20)
#' }
btbr_brm_temperature <- function(tempdata) {
  # Clean the standard errors: replace -9999 or negative with 1

  tempdata <- tempdata %>%
    dplyr::mutate(
      S22_PredSE = ifelse(S22_PredSE <= 0, 1, S22_PredSE)
    )

  # Calculate empirical moments for priors (log scale for lognormal)
  mean_y <- mean(tempdata[['S1_93_11']], na.rm = TRUE)
  var_y <- var(tempdata[['S1_93_11']], na.rm = TRUE)
  sigma_log <- sqrt(log(var_y / mean_y^2 + 1))
  mu_log <- log(mean_y) - sigma_log^2 / 2

  # Set up data with response and known prediction error
  model_data <- dplyr::tibble(
    Y = tempdata[['S1_93_11']],
    Y_se = tempdata[['S22_PredSE']]
  )

  # Priors informed by empirical distribution
  priors <- c(
    brms::prior_string(
      paste0("normal(", round(mu_log, 4), ", ", round(sigma_log * 2, 4), ")"),
      class = "Intercept"
    ),
    brms::prior_string(
      paste0("student_t(3, 0, ", round(sigma_log, 4), ")"),
      class = "sigma"
    )
  )

  # Fit intercept-only model with known measurement error on response
  mod_data <- brms::brm(
    formula = brms::bf(Y | mi(Y_se) ~ 1),
    data = model_data,
    family = brms::lognormal(),
    prior = priors,
    sample_prior = "only",
    backend = 'rstan',
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
#' @param local logical. Whether to use the data that comes with the package.
#' @param filter_geom an object of class bbox, sfc or sfg used to filter query results based on a predicate function.
#' @param ... Arguments to pass to `arc_select`, see \link[arcgislayers]{arc_select}.
#' @references {
#' \insertAllCited{}
#' }
#' @return A sf object.
#' @export
#'
btbr_norwest_temperature <- function(local = TRUE, filter_geom, ...) {


  if(local) {


    return(sf::read_sf(system.file('data/btb_data.gpkg',package = 'btbr'), layer = 'wmt_norwest_nhd'))


  }

  url <- arcgislayers::arc_open('https://apps.fs.usda.gov/arcx/rest/services/EDW/EDW_NorWeST_StreamTemperatures_01/MapServer/2')

  if(missing(filter_geom)){

    norwest <- arcgislayers::arc_select(url, ...)

  } else {

    norwest <- arcgislayers::arc_select(url, filter_geom = filter_geom, ...)

  }

}
