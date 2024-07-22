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
  packages = c("btbr", "furrr", "sf", "tidyverse")
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

    tar_target(btbr_rs,  btbr_sediment_randomsamples(usfs = TRUE,
                                         sedimentary_dist = sedimentary_dist[['lpearson']],
                                         granitic_dist = granitic_dist[['lognorm']])),

    tar_target(btbr_sedmod, btbr_brm_sediment(btbr_rs, linear = TRUE)),

    tar_target(btb_hucs, btbr_hucs()),


    tar_target(unroaded_hucs, btb_hucs %>%
                 dplyr::filter(specdelFS_HA == 0) %>%
                 sf::st_drop_geometry() %>%
                 dplyr::select(spec_delFS = 'specdelFS_HA',
                               road_length = 'tlenFS',
                               ig_or_not,
                               huc12 = 'HUC_12') %>%
                 dplyr::mutate(natural_erosion = 0,
                               og_spec = 0,
                               proportion = 0)),

    tar_target(fake_data, btbr_rs %>% dplyr::bind_rows(unroaded_hucs) %>%
                            dplyr::mutate(natural_erosion = ifelse(ig_or_not == 'sedimentary',
                                           as.numeric(exp(sedimentary_dist[['lpearson']]$estimate[['meanlog']])),
                                           as.numeric(exp(granitic_dist[['lognorm']]$estimate[['meanlog']]))))),

    tar_target(pps_sed, btbr_pp(btbr_brm = btbr_sedmod, data = fake_data, indicator = 'sediment')), # remember to change proportion....

    tar_target(pps_final_sediment,
                 btb_hucs %>%
                 dplyr::select(huc12 = 'HUC_12') %>%
                 dplyr::left_join(pps_sed) %>%
                 sf::st_as_sf() %>%
                 geojsonio::geojson_write(pps_final_sediment %>% sf::st_transform(4326), file = 'docs/btb_hucs.geojson')),

    #### temperature workflow

    tar_target(temp_query, btbr_norwest_temperature(sf::st_bbox(btb_hucs))),

    tar_target(huc_comid, read.csv(system.file('data/comid_huc12.csv',package = 'btbr'))),

    tar_target(wmt_norwest_temp, temp_query %>%
                                 dplyr::filter(COMID %in% huc_comid$comid,
                                               S1_93_11 > 0) %>%
                                 dplyr::left_join(huc_comid, by = c('COMID' = 'comid')) %>%
                                 dplyr::filter(!is.na(huc12))
               ),

    tar_target(wmt_norwest_temp_df, wmt_norwest_temp %>%
                                    sf::st_drop_geometry()),

    tar_target(btbr_tempmod, furrr::future_map(dplyr::group_nest(wmt_norwest_temp_df, huc12)$data,
                                             purrr::safely(~btbr_brm_temperature(.x))))


)
