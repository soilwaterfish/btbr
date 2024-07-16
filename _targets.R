# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c("btbr", "dplyr")
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

    tar_target(btbr_sedmod, btbr_brm(btbr_rs, linear = TRUE)),


    tar_target(unroaded_hucs, btbr_hucs() %>%
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

    tar_target(pps, btbr_pp(btbr_brm = btbr_sedmod, data = fake_data, indicator = 'sediment')),

    tar_target(pps_final_sediment,
                 btbr_hucs() %>%
                 dplyr::select(huc12 = 'HUC_12') %>%
                 dplyr::left_join(pps) %>%
                 sf::st_as_sf() %>%
                 geojsonio::geojson_write(pps_final_sediment %>% sf::st_transform(4326), file = 'docs/btb_hucs.geojson'))

    #### temperature workflow


)
