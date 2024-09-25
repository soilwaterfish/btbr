# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)
library(future)
library(future.callr)
plan(callr)

# Set target options:
tar_option_set(
  packages = c("btbr", "furrr", "sf", "tidyverse", "units")
)

# Replace the target list below with your own:
list(

    #### sediment workflow
    tar_target(data,  btbr_tss()),

    tar_target(granitic_dist, btbr_batch_distribution(data %>% dplyr::filter(geology == 'Granitic'),
                                           value = value_tons_mi2_yr,
                                           method = 'mge')),


    tar_target(sedimentary_dist, btbr_batch_distribution(data %>% dplyr::filter(geology == 'Sedimentary'),
                                              value = value_tons_mi2_yr,
                                              method = 'mge')),

    tar_target(btbr_randomsample,  btbr_sediment_randomsamples(usfs = TRUE,
                                         sedimentary_dist = sedimentary_dist[['lpearson']],
                                         granitic_dist = granitic_dist[['lognorm']])),

    tar_target(btbr_sedmod, btbr_brm_sediment(btbr_randomsample, linear = TRUE)),

    tar_target(btb_hucs_og, btbr_hucs()),

    tar_target(btb_hucs, btbr_hucs()),

    tar_target(btb_hucs_all, btbr_hucs(usfs = F)),


    tar_target(unroaded_hucs, btb_hucs_og %>%
                 dplyr::filter(specsdelFS_jur_fs == 0) %>%
                 sf::st_drop_geometry() %>%
                 dplyr::select(spec_delFS = 'specsdelFS_jur_fs',
                               road_length = 'tlenFS',
                               ig_or_not,
                               huc12 = 'HUC_12') %>%
                 dplyr::mutate(natural_erosion = 0,
                               og_spec = 0,
                               proportion = 0)),

    tar_target(ls_5percent_usfs_hucs, btb_hucs_og %>%
                 dplyr::filter(fs_percent_land < 0.05) %>%
                 sf::st_drop_geometry() %>%
                 dplyr::select(spec_delFS = 'specsdelFS_jur_fs',
                               road_length = 'tlenFS',
                               ig_or_not,
                               huc12 = 'HUC_12') %>%
                 dplyr::mutate(natural_erosion = NA_real_,
                               og_spec = NA_real_,
                               proportion = NA_real_)),
    tar_target(fake_data, btbr_randomsample %>% dplyr::bind_rows(unroaded_hucs) %>%
                          dplyr::bind_rows(ls_5percent_usfs_hucs) %>%
                            dplyr::mutate(natural_erosion = ifelse(ig_or_not == 'sedimentary',
                                           as.numeric(exp(sedimentary_dist[['lpearson']]$estimate[['meanlog']])),
                                           as.numeric(exp(granitic_dist[['lognorm']]$estimate[['meanlog']]))))),

    tar_target(pps_sed, btbr_pp(data = fake_data, btbr_brm = btbr_sedmod, newdata = fake_data, indicator = 'sediment')), # remember to change proportion....

    # tar_target(pps_final_sediment,
    #              btb_hucs_og %>%
    #              dplyr::select(huc12 = 'HUC_12') %>%
    #              dplyr::left_join(pps_sed) %>%
    #              sf::st_as_sf() %>%
    #              geojsonio::geojson_write(pps_final_sediment %>% sf::st_transform(4326), file = 'docs/btb_hucs_og.geojson')),

    #### temperature workflow

    tar_target(temp_query, btbr_norwest_temperature(sf::st_bbox(sf::st_transform(btb_hucs_all, 4326)))),

    tar_target(nhdplus, nhdplusTools::get_nhdplus(sf::st_as_sfc(sf::st_bbox(btb_hucs_all)))),

    tar_target(temp_query_nhdplus, temp_query %>% dplyr::left_join(nhdplus %>%
                                                                     dplyr::select(COMID = 'comid',
                                                                                   qe_08,
                                                                                   slope) %>%
                                                                     sf::st_drop_geometry())%>%
                 dplyr::filter(slope <= 0.16,
                               qe_08 >= 0.2)),

    tar_target(huc_comid, read.csv(system.file('data/comid_huc12.csv',package = 'btbr'))),

    tar_target(wmt_norwest_temp, temp_query_nhdplus %>%
                                 dplyr::filter(COMID %in% huc_comid$comid,
                                               S1_93_11 > 0) %>%
                                 dplyr::left_join(huc_comid, by = c('COMID' = 'comid')) %>%
                                 dplyr::filter(!is.na(huc12))
               ),

    tar_target(temp_fs_intersect, btbr:::btbr_temphuc_intersection(wmt_norwest_temp, btb_hucs_og)),

    tar_target(wmt_norwest_temp_df, wmt_norwest_temp %>%
                                    sf::st_drop_geometry() %>%
                                    dplyr::filter(!huc12 %in% temp_fs_intersect$huc12) %>%
                                    dplyr::bind_rows(temp_fs_intersect %>% sf::st_drop_geometry())),

    tar_target(btbr_tempmod, furrr::future_map(dplyr::group_nest(wmt_norwest_temp_df, huc12)$data,
                                             purrr::safely(~btbr_brm_temperature(.x)))),
    tar_target(final_fits, btbr_tempmod %>%
                 purrr::keep(~length(.) != 0) %>%
                 purrr::map(~.x[['result']])
    ),

    tar_target(temp_huc, dplyr::group_by(wmt_norwest_temp_df, huc12) %>%
                         dplyr::slice(1) %>%
                         dplyr::ungroup() %>%
                         dplyr::pull(huc12)),

    tar_target(pps_final_temperature,furrr::future_map2(temp_huc,
                              final_fits,
                              purrr::safely(~btbr_pp(data = dplyr::tibble(huc12 = .x,
                                                                          average_temp = .y[['data']]$value), btbr_brm =  .y, indicator = 'temperature')))%>%
                 purrr::keep(~length(.) != 0) %>%
                 purrr::map(~.x[['result']]) %>%
                 dplyr::bind_rows()),


    tar_target(final_risk_step_1, btb_hucs %>%
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
                )
)
