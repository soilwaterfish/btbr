
#' Get Aquatic Organism Passage data
#'
#' @description
#' These layers display the locations of aquatic organism passage activities, surveys, and length in miles of habitat improved. All layers display at all scales.
#'
#' `Copyright Text:` The USDA Forest Service makes no warranty, expressed or implied, including the warranties of merchantability and fitness for a particular purpose,
#' nor assumes any legal liability or responsibility for the accuracy, reliability, completeness or utility of these geospatial data, or for the improper or incorrect
#' use of these geospatial data. These geospatial data and related maps or graphics are not legal documents and are not intended to be used as such. The data and maps
#' may not be used to determine title, ownership, legal descriptions or boundaries, legal jurisdiction, or restrictions that may be in place on either public or private land.
#' Natural hazards may or may not be depicted on the data and maps, and land users should exercise due caution. The data are dynamic and may change over time. The user is
#' responsible to verify the limitations of the geospatial data and to use the data accordingly.
#' @param filter_geom an object of class bbox, sfc or sfg used to filter query results based on a predicate function.
#' @param layer A character vector with layer type, e.g. 'surveys','activities', 'improved' see Details below.
#' @param ... Arguments to pass to `arc_select`, see \link[arcgislayers]{arc_select}.
#' @details
#' \itemize {
#' \item \strong{surveys}: This dataset provides USFS Aquatic Organism Passage (AOP) survey data.
#'  It shows stream passage locations, passage measurements, and passability assessment categories from AOP field surveys.
#'  Structure included: culverts, dams, diversion dams, fords, and natural features such as waterfalls.
#' \item \strong{activities}: Activities included are where barriers to upstream migration have been improved.
#'  This includes improving existing passage structures or removing them entirely.
#'   Structures include culverts, dams, diversion dams, and fords.
#'    Also included are where structures have been added to purposely create barriers to protect native populations from invasive species.
#'     Data include the planned fiscal year and planned cost, the completed fiscal year, approximate completed cost, and partners involved.
#'      Each AOP activity is displayed as a single point.
#' \item \strong{improved}: Displays how far upstream from an activity on an Aquatic Organism Passage (AOP) structure that habitat has been improved.
#'  Data includes the completed fiscal year and lists species that benefit from the habitat improvement.
#'  The miles of habitat improved are displayed as a line or multi-line.
#' }
#' @references {
#' \insertAllCited{}
#' }
#' @return A sf object.
#' @export
#'
btbr_aop <- function(filter_geom, layer,  ...) {

  url <- switch(layer,
                surveys = arcgislayers::arc_open('https://apps.fs.usda.gov/arcx/rest/services/EDW/EDW_AquaticOrganismPassage_01/MapServer/1'),
                activities = arcgislayers::arc_open('https://apps.fs.usda.gov/arcx/rest/services/EDW/EDW_AquaticOrganismPassage_01/MapServer/0'),
                improved = arcgislayers::arc_open('https://apps.fs.usda.gov/arcx/rest/services/EDW/EDW_AquaticOrganismPassage_01/MapServer/2')


  )

  if(missing(filter_geom)){

    norwest <- arcgislayers::arc_select(url, ...)

  } else {

    norwest <- arcgislayers::arc_select(url, filter_geom = filter_geom, ...)

  }

}
