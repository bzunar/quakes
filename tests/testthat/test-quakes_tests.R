# Data Wrangling

testthat::test_that("eq_clean_data outputs a clean data frame", {
    testthat::expect_s3_class({system.file("extdata", "signif.txt.bz2", package = "quakes") %>%
            readr::read_delim(., delim = "\t", guess_max = 10000) %>%
            quakes::eq_clean_data(.)}, "spec_tbl_df")
})

testthat::test_that("eq_clean_data formats the DATE column correctly", {
    testthat::expect_s3_class({system.file("extdata", "signif.txt.bz2", package = "quakes") %>%
            readr::read_delim(., delim = "\t", guess_max = 10000) %>%
            quakes::eq_clean_data(.) %>%
            dplyr::pull(., DATE)}, "Date")
})

testthat::test_that("eq_location_clean outputs a clean data frame", {
    testthat::expect_s3_class({system.file("extdata", "signif.txt.bz2", package = "quakes") %>%
            readr::read_delim(., delim = "\t", guess_max = 10000) %>%
            quakes::eq_clean_data(.) %>%
            quakes::eq_location_clean(.)}, "spec_tbl_df")
})

################################################################################
# Geoms

testthat::test_that("geom_timeline draws a timeline", {

    # load the data bundled with the package
    utils::data(usa_japan)

    # generate the ggplot object
    gg <- usa_japan %>%
        ggplot2::ggplot(ggplot2::aes(x = DATE, y = COUNTRY)) +
        quakes::geom_timeline(ggplot2::aes(colour = base::as.numeric(DEATHS),
                                           size = base::as.numeric(EQ_PRIMARY))) +
        ggplot2::theme_classic() +
        ggplot2::theme(legend.position = "bottom")

    # gg is a ggplot object and as such is a list of 9
    testthat::expect_is(gg,"ggplot")
    testthat::expect_equal(base::mode(gg), "list")
    testthat::expect_equal(base::length(gg), 9)

    # gg should have only one layer
    testthat::expect_equal(base::length(gg$layers), 1)

    # determine geom type of each layer
    testthat::expect_equal(base::class(gg$layers[[1]]$geom)[1], "GeomTimeline")

})


testthat::test_that("geom_timeline_labels plots labels", {

    # load the data bundled with the package
    utils::data(usa_japan)

    # generate the ggplot object
    gg <- usa_japan %>%
        ggplot2::ggplot(ggplot2::aes(x = DATE, y = COUNTRY)) +
        quakes::geom_timeline(ggplot2::aes(colour = base::as.numeric(DEATHS),
                                           size = base::as.numeric(EQ_PRIMARY))) +
        quakes::geom_timeline_label(n_max = 4) +
        ggplot2::theme_classic() +
        ggplot2::theme(legend.position = "bottom")

    # gg is a ggplot object and as such is a list of 9
    testthat::expect_is(gg, "ggplot")
    testthat::expect_equal(base::mode(gg), "list")
    testthat::expect_equal(base::length(gg), 9)

    # gg should have two layers
    testthat::expect_equal(base::length(gg$layers), 2)

    # determine geom type of each layer
    testthat::expect_equal(base::class(gg$layers[[1]]$geom)[1], "GeomTimeline")
    testthat::expect_equal(base::class(gg$layers[[1]]$geom)[2], "Geom")

})

################################################################################
# Leaflet

testthat::test_that("eq_map works", {

    # generate the leaflet map
    leaf <- usa_japan %>%
        quakes::eq_map(annot_col = "DATE")

    # leaf is a leaflet object and as such is a list of 9
    testthat::expect_is(leaf, "leaflet")
    testthat::expect_equal(base::mode(leaf), "list")
    testthat::expect_equal(base::length(leaf), 8)

    testthat::expect_equal(as.character(leaf$x$limits$lat[1]), "27.839")
    testthat::expect_equal(as.character(leaf$x$limits$lng[2]), "178.735")
})


testthat::test_that("eq_create_label works", {

    # generate the leaflet map
    labels <- usa_japan %>%
        quakes::eq_create_label()

    # leaf is a leaflet object and as such is a list of 9
    testthat::expect_is(labels, "character")
    testthat::expect_equal(base::length(labels), 20)

    testthat::expect_equal(labels[19], "<b>Location:</b> Near E Coast Honshu<br><b>Magnitude:</b>  6.9<br>")
    testthat::expect_equal(labels[8], "<b>Location:</b> Napa, Vallejo<br><b>Magnitude:</b>  6.1<br><b>Total deaths:</b>        1")
})

