test_that("testing btbr_tss()", {

expect_equal(nrow(btbr_tss(geology = 'all')), 30)

expect_equal(nrow(btbr_tss(geology = 'sedimentary')), 17)

expect_equal(nrow(btbr_tss(geology = 'granitic')), 13)

})

test_that("testing btbr_tss_distribution()", {

  data <- btbr_tss()

  data_dist <- btbr_batch_distribution(data, value_tons_mi2_yr, method = 'mge')

  expect_equal(length(data_dist), 5)

})
