# doc or install if using future
# targets seems to not work completely the last time i ran. It misses out on some HUCS.... so i just do the workflow below.
#devtools::document()
remotes::install_github('soilwaterfish/btbr')

library(tidyverse)

library(future)
plan(multisession(workers = 11))

#### sediment workflow
data <- btbr_tss()

granitic_dist <- btbr_batch_distribution(data %>% dplyr::filter(geology == 'Granitic'),
                                                  value = value_tons_mi2_yr,
                                                  method = 'mge')


sedimentary_dist <- btbr_batch_distribution(data %>% dplyr::filter(geology == 'Sedimentary'),
                                                     value = value_tons_mi2_yr,
                                                     method = 'mge')

btbr_randomsample <- btbr_sediment_randomsamples(usfs = TRUE,
                                                           sedimentary_dist = sedimentary_dist[['lpearson']],
                                                           granitic_dist = granitic_dist[['lognorm']])

btbr_sedmod <- btbr_brm_sediment(btbr_randomsample, linear = TRUE)

btb_hucs_og <-  btbr_hucs()

btb_hucs <- btbr_hucs()

btb_hucs_all <- btbr_hucs(usfs = F)


unroaded_hucs <- btb_hucs_og %>%
             dplyr::filter(specsdelFS_jur_fs == 0) %>%
             sf::st_drop_geometry() %>%
             dplyr::select(spec_delFS = 'specsdelFS_jur_fs',
                           road_length = 'tlenFS',
                           ig_or_not,
                           huc12 = 'HUC_12') %>%
             dplyr::mutate(natural_erosion = 0,
                           og_spec = 0,
                           proportion = 0)

ls_5percent_usfs_hucs <- btb_hucs_og %>%
             dplyr::filter(fs_percent_land < 0.05) %>%
             sf::st_drop_geometry() %>%
             dplyr::select(spec_delFS = 'specsdelFS_jur_fs',
                           road_length = 'tlenFS',
                           ig_or_not,
                           huc12 = 'HUC_12') %>%
             dplyr::mutate(natural_erosion = NA_real_,
                           og_spec = NA_real_,
                           proportion = NA_real_)

fake_data <- btbr_randomsample %>% dplyr::bind_rows(unroaded_hucs) %>%
             dplyr::bind_rows(ls_5percent_usfs_hucs) %>%
             dplyr::mutate(natural_erosion = ifelse(ig_or_not == 'sedimentary',
                                                    as.numeric(exp(sedimentary_dist[['lpearson']]$estimate[['meanlog']])),
                                                    as.numeric(exp(granitic_dist[['lognorm']]$estimate[['meanlog']]))))

pps_sed <-  btbr_pp(data = fake_data, btbr_brm = btbr_sedmod, newdata = fake_data, indicator = 'sediment') # remember to change proportion....

# pps_final_sediment,
#              btb_hucs_og %>%
#              dplyr::select(huc12 = 'HUC_12') %>%
#              dplyr::left_join(pps_sed) %>%
#              sf::st_as_sf() %>%
#              geojsonio::geojson_write(pps_final_sediment %>% sf::st_transform(4326), file = 'docs/btb_hucs_og.geojson')),

#### temperature workflow

temp_query <- btbr_norwest_temperature(local = T, file_path = 'data/Final/NorWeST_PredictedStreamTempLines_Spokoot_v2.shp')

nhdplus <- nhdplusTools::get_nhdplus(sf::st_as_sfc(sf::st_bbox(btb_hucs_all)))

temp_query_nhdplus <-  temp_query %>% dplyr::left_join(nhdplus %>%
                                                                 dplyr::select(COMID = 'comid',
                                                                               qe_08,
                                                                               slope) %>%
                                                                 sf::st_drop_geometry())

huc_comid <- read.csv(system.file('data/comid_huc12.csv',package = 'btbr'))

wmt_norwest_temp <- temp_query_nhdplus %>%
                     dplyr::filter(COMID %in% huc_comid$comid,
                                   S1_93_11 > 0) %>%
                     dplyr::left_join(huc_comid, by = c('COMID' = 'comid')) %>%
                     dplyr::filter(!is.na(huc12))


temp_fs_intersect <- btbr:::btbr_temphuc_intersection(wmt_norwest_temp, btb_hucs_og)

wmt_norwest_temp_df <- wmt_norwest_temp %>%
             sf::st_drop_geometry() %>%
             dplyr::filter(!huc12 %in% temp_fs_intersect$huc12) %>%
             dplyr::bind_rows(temp_fs_intersect %>% sf::st_drop_geometry())

#    missing_temps, readr::read_rds('data/missing_temps')),
btbr_tempmod <- furrr::future_map(dplyr::group_nest(wmt_norwest_temp_df, huc12)$data,
                                           purrr::safely(~btbr_brm_temperature(.x)))

final_fits <- btbr_tempmod %>%
             purrr::keep(~length(.) != 0) %>%
             purrr::map(~.x[['result']])


temp_huc <- dplyr::group_by(wmt_norwest_temp_df, huc12) %>%
             dplyr::slice(1) %>%
             dplyr::ungroup() %>%
             dplyr::pull(huc12)


pps_final_temperature <- furrr::future_map2(temp_huc,
                                                    final_fits,
                                                    purrr::safely(~btbr_pp(data = dplyr::tibble(huc12 = .x,
                                                                                                average_temp = .y[['data']]$value), btbr_brm =  .y, indicator = 'temperature')))%>%
             purrr::keep(~length(.) != 0) %>%
             purrr::map(~.x[['result']]) %>%
             dplyr::bind_rows()


final_risk_step_1 <- btb_hucs %>%
             dplyr::mutate(FS_Land_btb = as.numeric(units::set_units(sf::st_area(.), 'mi^2'))) %>%
             dplyr::left_join(pps_sed %>% dplyr::select(HUC_12 = 'huc12',
                                                        fa_sed = 'fa',
                                                        far_sed = 'far',
                                                        fur_sed = 'fur',
                                                        final_risk_sed = 'final_risk')) %>%
             dplyr::left_join(pps_final_temperature %>%
                                dplyr::mutate(huc12 = as.character(huc12)) %>%
                                dplyr::select(HUC_12 = 'huc12',
                                              average_temp,
                                              fa_temp = 'fa',
                                              far_temp = 'far',
                                              fur_temp = 'fur',
                                              final_risk_temp = 'final_risk')) %>%
             sf::st_as_sf()



final_risk_step_1 %>% st_drop_geometry() %>% write_csv('data/final_risk_step_1.csv')

mapview::mapview(final_risk_step_1, zcol = c('final_risk_temp', 'final_risk_sed'))


library(arcgisbinding)
arc.check_product()
