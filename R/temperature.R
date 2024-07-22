
#' Fit Bayesian Linear Model
#'
#' @param tempdata A NorWest Stream Temperature data.frame with `S1_93_11` column name.
#' @return A `brmsfit` model object.
#'
#' @export
#' @examples
#'
#' spokoot <- fishguts::get_NorWestStreams('SpoKoot') %>% st_as_sf()
#'
#' huc_comid <-  read.csv('data/comid_huc12.csv')
#'
#' spokoot_df <- dplyr::left_join(huc_comid, by = c('COMID' = 'comid')) %>%
#'   dplyr::filter(!is.na(huc12)) %>%
#'   sf::st_drop_geometry() %>%
#'   dplyr::filter(S1_93_11 > 0)
#'
#' temp_model <- btbr_brm_temperature(spokoot_df)

btbr_brm_temperature <- function(tempdata){

  mean <- mean(tempdata[['S1_93_11']], na.rm = T)
  variance <- var(tempdata[['S1_93_11']], na.rm = T)

  sigma <- sqrt(log(variance / mean ^ 2 + 1))

  # and plugging in sigma into our first equation, mu becomes
  mu <- log(mean) - sigma ^ 2 / 2

  mod_data <- brms::brm(value ~1,
                              data = dplyr::tibble(value = mean),
                              family = brms::lognormal(),
                              prior = c(eval(call("prior",
                                                  sprintf("normal(%f, %f)", round(mu[1], 4), 0.025),
                                                  class = "Intercept")), eval(call("prior",
                                                                                   sprintf("normal(%f, %f)", round(sigma[1], 4), 0.025),
                                                                                   class = "sigma"))),
                              sample_prior = "only",
                              seed = 1234,
  )

  mod_data
}


#' Get USDA-Forest Service Administration Boundaries
#'
#' @description
#' This layer represents modeled stream temperatures derived from the NorWeST point feature class (NorWest_TemperaturePoints).
#' NorWeST summer stream temperature scenarios were developed for all rivers and streams in the western U.S. from the more than 20,000 stream sites in
#' the NorWeST database where mean August stream temperatures were recorded. The resulting dataset includes stream lines (NorWeST_PredictedStreams)
#' and associated mid-points NorWest_TemperaturePoints) representing 1 kilometer intervals along the stream network.
#' Stream lines were derived from the 1:100,000 scale NHDPlus dataset (USEPA and USGS 2010; McKay et al. 2012, \insertCite{kirchner2001mountain}{btbr}).
#' Shapefile extents correspond to NorWeST processing units, which generally relate to 6 digit (3rd code) hydrologic
#' unit codes (HUCs) or in some instances closely correspond to state borders.
#' The line and point shapefiles contain identical modeled stream temperature results.
#' The two feature classes are meant to complement one another for use in different applications.
#' In addition, spatial and temporal covariates used to generate the modeled temperatures are included in the attribute tables at \url(https://www.fs.usda.gov/rm/boise/AWAE/projects/NorWeST/ModeledStreamTemperatureScenarioMaps.shtml).
#' The NorWeST NHDPlusV1 processing units include: Salmon, Clearwater, Spokoot, Missouri Headwaters, Snake-Bear, MidSnake, MidColumbia, Oregon Coast, South-Central Oregon, Upper Columbia-Yakima, Washington Coast, Upper Yellowstone-Bighorn, Upper Missouri-Marias, and Upper Green-North Platte.
#' The NorWeST NHDPlusV2 processing units include: Lahontan Basin, Northern California-Coastal Klamath, Utah, Coastal California, Central California, Colorado, New Mexico, Arizona, and Black Hills.
#'
#' `Copyright Text:` U.S. Forest Service; Rocky Mountain Research Station; Air, Water, and Aquatic Environments Program (AWAE). \url(https://www.fs.usda.gov/rm/boise/awae_home.shtml)
#'
#' @param filter_geom an object of class bbox, sfc or sfg used to filter query results based on a predicate function.
#' @param ... Arguments to pass to \link[arcgislayers] package `arc_select` function.
#' @references {
#' \insertAllCited{}
#' }
#' @return A sf object.
#' @export
#'
btbr_norwest_temperature <- function(filter_geom, ...) {

  url <- arcgislayers::arc_open('https://apps.fs.usda.gov/arcx/rest/services/EDW/EDW_NorWeST_StreamTemperatures_01/MapServer/2')

  if(missing(filter_geom)){

    admin <- arcgislayers::arc_select(url, ...)

  } else {

    admin <- arcgislayers::arc_select(url, filter_geom = filter_geom, ...)

  }
}
