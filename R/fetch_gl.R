
#' Fetch GRAIP-Lite Data
#'
#' @description This is a higher level wrapper around the \link{get_GL} and \link{get_GL_layers}
#' functions. This function can fetch multiple File Geodatabases (GDB) and returns all the layers within the GDB.
#' @param gdb A \code{character} vector of the GDB(s), e.g. \code{'Deschutes'}.
#' @param ... Arguments to pass to \link{get_GL}.
#' @author Josh Erickson
#' @return A list.
#' @seealso `get_GL()` `get_GL_layers()`
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # fetch R1 GRAIP-Lite run
#'
#' GL <- fetchGL(gdb = c('R1'), quiet = TRUE)
#'
#' }

fetchGL <- function(gdb, ...) {

  gdb <- .get_GL_gdb_names(gdb)

  sapply(gdb, function(x) {
    suppressWarnings(get_GL(x, layers = get_GL_layers(x)$name, ...))
  })
}

#' Get Geomorphic Road Analysis and Inventory Package Lite (GRAIP-Lite) Data
#'
#' @description This function calls Rocky Mountain Research Station (zip files) to get Geomorphic Road Analysis and Inventory Package Lite (GRAIP-Lite) data. These
#' datasets contain both spatial and non-spatial data in the form of a File Geodatabase (GDB).
#' @param gdb A \code{character} of the GDB, e.g. \code{'R1'}.
#' @param version A \code{character} indicating the data version, e.g. \code{'v3'}.
#' @param layers A \code{character} of the layer(s) within the GDB, e.g. \code{'HUCs_GL_Runs_R01'} (default).
#' @param quiet A \code{logical}; suppress info on name, driver, size and spatial reference, or signaling no or multiple layers.
#' @param simplify A \code{logical}; whether to return a simplified list (\code{data.frame} or \code{sf}) if length(layers) == 1.
#' @param ... Arguments to pass to `terra::vect()`.
#' @author Josh Erickson
#' @seealso `get_GL_layers()`
#' @return An \code{sf} or \code{data.frame} object.
#'
#' @note Please use \code{\link{get_GL_layers}} to get the layer id information needed for the layer argument. This will
#' help with joining \code{sf} and \code{data.frame} objects. Proj4 = '+proj=aea +lat_0=40 +lon_0=-96 +lat_1=20 +lat_2=60 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs'.
#'
#' @details  Road Density, Proximity and Erosion Data (both unit sets1) and Stability Index Information. Data is available nationally and by Forest Service Region.
#' The GDB's currently available:
#' \itemize{
#' \item  \strong{Region 1}
#' \item  \strong{Region 2}
#' \item \strong{Region 3}
#' \item \strong{Region 4}
#' \item \strong{Region 5}
#' \item \strong{Region 6}
#' \item  \strong{Region 8}
#' \item  \strong{Region 9}
#' \item \strong{Region 10}
#' \item \strong{National Slope Stability}
#' }
#' @export
#' @examples
#' \dontrun{
#'
#' # get R1 GRAIP-Lite
#' GL_R1 <- get_GL('R1')
#'
#' # get multiple layers in a list
#'
#' GL_R1_multiple <- get_GL(gdb = 'R1',
#' layers = c('WCATT_HUCs_GL_Data_USC_Units_R01', 'WCATT_HUCs_GL_Data_SI_Units_R01'))
#'
#' # Or run with a SQL query
#' r1_gl <- get_GL(gdb = 'R1', layers = 'WCATT_HUCs_GL_Data_USC_Units_R01',
#' query = "select * from \"WCATT_HUCs_GL_Data_USC_Units_R01\" where STATES='CN,MT' OR STATES='MT' AND SUBSTR(HUC_12, 1, 4) = '1701'")
#'
#' }
#'
#'
get_GL <- function(gdb, layers = 'WCATT_HUCs_GL_Data_USC_Units_R01', version = 'v3', quiet = FALSE, simplify = TRUE, ...) {

  gdb <- .get_GL_gdb_names(gdb)

  GL <- list()

  for(i in layers){

    GL_get <- try(list(terra::vect(paste0('/vsizip//vsicurl/https://www.fs.usda.gov/research/sites/default/files/2023-02/rmrs',gdb,'_',version,'_gdb.zip'),
                                   layer = i, ...)),
                  silent = TRUE)

    names(GL_get) <- i

    GL <- append(GL, GL_get)

  }

  if(length(layers) == 1) {if(isTRUE(simplify)){GL <- GL[[1]]} else {GL}}

  GL

}

#' Get GL Layers
#'
#' @param gdb A \code{character} of the GDB, e.g. \code{'R1'}.
#' @param version A \code{character} indicating the data version, e.g. \code{'v3'}.
#' @author Josh Erickson
#'
#' @return A list of metadata about the GDB
#' @export
#'
#' @note Refer to \code{\link{get_GL}} for information on File Geodatabase (GDB) availability.
#'
#' @examples
#' \dontrun{
#' GL_layers <- get_GL_layers('R1')
#' }
#'
get_GL_layers <- function(gdb, version = 'v3') {

  gdb <- .get_GL_gdb_names(gdb)

  layers <- try(sf::st_layers(paste0('/vsizip//vsicurl/https://www.fs.usda.gov/research/sites/default/files/2023-02/rmrs',gdb,'_',version,'_gdb.zip')), silent = TRUE)

  as.data.frame(sapply(layers, I))
}


#' matching helper
#' @param gdb A character.
#' @return A gdb character.
.get_GL_gdb_names <- function(gdb) {

  gdb_names <- tolower(c('Region1', 'Region2', 'Region3', 'Region4', 'Region5',
                         'Region6', 'Region8', 'Region9', 'Region10',
                         'Region 1', 'Region 2', 'Region 3', 'Region 4',
                         'Region 5', 'Region 6', 'Region 8', 'Region 9',
                         'Region 10', 'Slope Stability', 'SS',
                         'R1', 'R2', 'R3', 'R4', 'R5', 'R6', 'R8', 'R9', 'R10'
  ))

  gdb <- match.arg(tolower(gdb), choices = gdb_names, several.ok = TRUE)

  ifelse(gdb %in% c('region1', 'region 1', 'r1'), '-region01',
         ifelse(gdb %in% c('region2', 'region 2', 'r1'), '-region06',
                ifelse(gdb %in% c('region3', 'region 3', 'r1'), '-region03',
                       ifelse(gdb %in% c('region4', 'region 4', 'r1'), '-region04',
                              ifelse(gdb %in% c('region5', 'region 5', 'r1'), '-region05',
                                     ifelse(gdb %in% c('region6', 'region 6', 'r1'), '-region06',
                                            ifelse(gdb %in% c('region8', 'region 8', 'r1'), '-region08',
                                                   ifelse(gdb %in% c('region9', 'region 9', 'r1'), '-region09',
                                                          ifelse(gdb %in% c('region10', 'region 10', 'r1'), '-region10',
                                                                 ifelse(gdb %in% c('ss', 'slope stability'), '-national_slopestability', NA))))))))))

}
