## code to prepare `quake_subset` dataset goes here

usa_japan <- system.file("extdata", "signif.txt.bz2", package = "quakes") %>%
    readr::read_delim(delim = "\t", guess_max = 10000) %>%
    eq_clean_data() %>%
    filter(DATE >= as.Date("2014-01-05") & DATE <= as.Date("2018-01-05")) %>%
    filter(COUNTRY %in% c("USA", "JAPAN"))

usethis::use_data(usa_japan, overwrite = TRUE)
