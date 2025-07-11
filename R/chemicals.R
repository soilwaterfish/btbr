#' Get WCC
#' @description
#' Uses corporate SDE to get most recent Watershed Condition Class (WCC) information.
#'
#'
#' @param conda_path A path to an arcgispro-py3 conda env.
#'
#' @return data.frame with WCC attributes in R1.
#' @export
#'
#' @examples
get_wcc <- function(conda_path = r'{C:\Program Files\ArcGIS\Pro\bin\Python\envs\arcgispro-py3}'){

  reticulate::use_condaenv(conda_path)

  arcpy <- reticulate::import('arcpy')

  arcgis.features <- reticulate::import('arcgis.features')

  # Use the as_myself SDE connection file on the T:
  arcpy$env$workspace = r"{T:\FS\Reference\EDW\EDWLyrxLibrary\SDE\EDW\_EDW_SDE.sde}"

  # Get layer from this path S_USA.Hydro_WatershedConditionClass_USGS_Gen
  fc = "S_USA.Hydro_WatershedConditionClass_USGS_Gen"

  # where clause where Forest Unit ID	starts with '01'
  where_clause = "FS_UNIT_ID LIKE '01%'"

  # Create a feature layer
  arcpy$management$MakeFeatureLayer(fc, "wcc", where_clause)

  # now get the layer wcc as a pandas dataframe
  df = arcgis.features$GeoAccessor$from_featureclass("wcc") %>% select(-SHAPE)

}
