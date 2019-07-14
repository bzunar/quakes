#' Convert Raw NOAA Data to a Clean Data Frame
#'
#' The function takes raw NOAA data frame and returns a clean data frame that has:
#' (1) A date column created by uniting the year, month, day and converting it to
#' the Date class; (2) LATITUDE and LONGITUDE columns converted to numeric class.
#'
#' @param df A data frame containing raw NOAA data.
#'
#' @return A clean data frame.
#'
#' @details The first function the user should run once they obtain the NOAA data set.
#'
#' @importFrom dplyr if_else mutate select
#' @importFrom lubridate ymd
#' @importFrom magrittr %>%
#' @importFrom readr read_delim
#' @importFrom rlang .data
#' @importFrom stringr str_sub
#'
#' @examples
#' system.file("extdata", "signif.txt.bz2", package = "quakes") %>%
#'     readr::read_delim(delim = "\t", guess_max = 10000) %>%
#'     eq_clean_data()
#'
#' @export
eq_clean_data <- function(df) {

    df %>%
        dplyr::mutate(BC = dplyr::if_else(stringr::str_sub(.data$YEAR, 1, 1) == '-', TRUE, FALSE),
                      YEAR = dplyr::if_else(stringr::str_sub(.data$YEAR, 1, 1) == '-',
                                            stringr::str_sub(.data$YEAR, 2, -1),
                                            base::as.character(.data$YEAR)),
                      YEAR = dplyr::if_else(base::nchar(.data$YEAR) == 2,
                                            base::paste0("00", .data$YEAR),
                                            .data$YEAR),
                      YEAR = dplyr::if_else(base::nchar(.data$YEAR) == 3,
                                            base::paste0("0", .data$YEAR),
                                            .data$YEAR),
                      DATE = dplyr::if_else(!.data$BC,
                                            lubridate::ymd(base::paste(.data$YEAR, .data$MONTH, .data$DAY, sep = "-"),
                                                           truncated = 2),
                                            base::as.Date(NA)),
                      LATITUDE = base::as.numeric(.data$LATITUDE),
                      LONGITUDE = base::as.numeric(.data$LONGITUDE)) %>%
        dplyr::select(-.data$BC)
}

#' Format Location Name
#'
#' The function cleans the LOCATION_NAME column by stripping out the country name
#' (including the colon) and converts names to title case (as opposed to all caps).
#'
#' @param df A data frame containing raw NOAA data.
#'
#' @return A data frame with formated LOCATION_NAME column.
#'
#' @details This function is needed for annotating visualisations.
#'
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @importFRom rlang .data
#' @importFrom stringr str_match str_to_title
#'
#' @examples
#' system.file("extdata", "signif.txt.bz2", package = "quakes") %>%
#'     readr::read_delim(delim = "\t", guess_max = 10000) %>%
#'     quakes::eq_clean_data() %>%
#'     quakes::eq_location_clean()
#'
#' @export
eq_location_clean <- function(df) {

    df %>%
        dplyr::mutate(LOCATION_NAME = stringr::str_match(.data$LOCATION_NAME,
                                                         pattern = ":\\s*(\\S+.*$)")[, 2],
               LOCATION_NAME = stringr::str_to_title(.data$LOCATION_NAME))
}

#' Plot a Time Line of Earthquakes
#'
#' A ggplot2 geom that plots a time line of earthquakes ranging from code{xmin}
#' to code{xmax} dates with a point for each earthquake. Optional aesthetics
#' include color, size, and alpha (for transparency). The x-aesthetic is a date
#' and an optional y-aesthetic is a factor indicating some stratification in
#' which case multiple time lines will be plotted for each level of the factor
#' (e.g. country).
#'
#' @inheritParams ggplot2::layer
#'
#' @param ... Other arguments passed on to [layer()]. These are
#'   often aesthetics, used to set an aesthetic to a fixed value, like
#'   `colour = "red"` or `size = 3`. They may also be parameters
#'   to the paired geom/stat.
#'
#' @importFrom ggplot2 aes ggplot layer theme theme_classic
#'
#' @examples
#' gg <- usa_japan %>%
#'     ggplot2::ggplot(ggplot2::aes(x = DATE, y = COUNTRY)) +
#'     ggplot2::theme_classic() +
#'     ggplot2::theme(legend.position = "bottom")
#'
#' gg + quakes::geom_timeline(ggplot2::aes(colour = base::as.numeric(DEATHS),
#'                                         size = base::as.numeric(EQ_PRIMARY)))
#'
#' @export
geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity",
                          show.legend = NA, inherit.aes = TRUE, ...) {
    ggplot2::layer(
        geom = GeomTimeline, mapping = mapping,
        data = data, stat = stat, position = position,
        show.legend = show.legend, inherit.aes = inherit.aes,
        params = base::list(na.rm = FALSE, ...)
    )
}

#' Ggproto Object Used By geom_timeline
#'
#' @rdname ggplot2-ggproto
#' @format NULL
#'
#' @importFrom ggplot2 aes Geom ggproto draw_key_point .pt .stroke
#' @importFrom grid gList gpar gTree pointsGrob
#' @importFrom scales alpha
#'
#' @usage NULL
#' @export
GeomTimeline <- ggplot2::ggproto("GeomTimeline", Geom,
                                 required_aes = base::c("x"),
                                 non_missing_aes = base::c("size", "shape", "colour", "y"),
                                 default_aes = ggplot2::aes(shape = 19, colour = "black", size = 1.5,
                                                            fill = NA, alpha = 0.3, stroke = 0.5, y = 0),
                                 draw_key = ggplot2::draw_key_point,
                                 draw_panel = function(data, panel_scales, coord) {

                                     coords <- coord$transform(data, panel_scales)

                                     ## contruct a segment grob
                                     time <- grid::segmentsGrob(x0 = base::min(coords$x),
                                                          x1 = base::max(coords$x),
                                                          y0 = coords$y,
                                                          y1 = coords$y,
                                                          gp = grid::gpar(col = scales::alpha(coords$colour, coords$alpha),
                                                                    fill = scales::alpha(coords$fill, coords$alpha),
                                                                    lwd = 2))

                                     ## construct a grid grob
                                     quake <- grid::pointsGrob(
                                         x = coords$x,
                                         y = coords$y,
                                         pch = coords$shape,
                                         gp = grid::gpar(
                                             col = scales::alpha(coords$colour, coords$alpha),
                                             fill = scales::alpha(coords$fill, coords$alpha),

                                             # Stroke is added around the outside of the point
                                             fontsize = coords$size * ggplot2:::.pt + coords$stroke * ggplot2:::.stroke / 2,
                                             lwd = coords$stroke * ggplot2:::.stroke / 2))

                                     grid::gTree(children = grid::gList(time, quake))

                                 }
)

#' Add Annotations to the Time Line of Earthquakes
#'
#' This geom adds a vertical line to each data point with a text annotation (e.g.
#' the location of the earthquake) attached to each line. The user can subset
#' to n_max number of earthquakes, where they take the n_max largest (by magnitude)
#' earthquakes. Aesthetics are x, which is the date of the earthquake and label
#' which takes the column name from which annotations will be obtained.
#'
#' @inheritParams ggplot2::layer
#'
#' @param ... Other arguments passed on to [layer()]. These are
#' often aesthetics, used to set an aesthetic to a fixed value, like
#' `colour = "red"` or `size = 3`. They may also be parameters
#' to the paired geom/stat.
#' @param n_max Number of earthquakes to label (ordered by their magnitude); an
#' optional aesthetics
#'
#' @importFrom dplyr top_n
#' @importFrom ggplot2 aes ggplot layer theme theme_classic
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @examples
#' gg <- usa_japan %>%
#'     eq_location_clean() %>%
#'     ggplot2::ggplot(ggplot2::aes(x = DATE, y = COUNTRY, label = LOCATION_NAME)) +
#'     ggplot2::theme_classic() +
#'     ggplot2::theme(legend.position = "bottom") +
#'     quakes::geom_timeline(ggplot2::aes(colour = base::as.numeric(DEATHS),
#'                                        size = base::as.numeric(EQ_PRIMARY)))
#'
#' gg + quakes::geom_timeline_label(n_max = 4)
#'
#' @export
geom_timeline_label <- function(mapping = NULL, data = NULL, stat = "identity",
                                position = "identity",
                                show.legend = NA, inherit.aes = TRUE, n_max = FALSE, ...) {

    ggplot2::layer(
        geom = GeomTimelineLabel, mapping = mapping,
        data = function(x) { if (!n_max) x else dplyr::top_n(x, n = n_max, wt = .data$EQ_PRIMARY) },
        stat = stat, position = position,
        show.legend = show.legend, inherit.aes = inherit.aes,
        params = base::list(na.rm = FALSE, ...)
    )
}

#' Ggproto Object Used By geom_timeline_label
#'
#' @rdname ggplot2-ggproto
#' @format NULL
#'
#' @importFrom ggplot2 aes draw_key_point Geom ggproto
#' @importFrom grid gpar gList gTree segmentsGrob textGrob
#' @importFrom scales alpha
#'
#' @usage NULL
#' @export
GeomTimelineLabel <- ggplot2::ggproto("GeomTimelineLabel", Geom,
                             required_aes = base::c("x", "label"),
                             default_aes = ggplot2::aes(colour = "black", y = 0,
                                               size = 3.88, angle = 45, hjust = 0.5,
                                               vjust = 0.5, alpha = NA, family = "",
                                               fontface = 1, lineheight = 1.2),
                             draw_key = ggplot2::draw_key_point,
                             draw_panel = function(data, panel_params, coord, parse = FALSE,
                                                   na.rm = FALSE, check_overlap = FALSE) {

                                 coords <<- coord$transform(data, panel_params)

                                 se <<- grid::segmentsGrob(x0 = coords$x,
                                                     x1 = coords$x,
                                                     y0 = coords$y,
                                                     y1 = coords$y + 0.1,
                                                     gp = grid::gpar(
                                                         col = scales::alpha(coords$colour, 0.2),
                                                         lwd = 2
                                                     ))

                                 lab <- data$label
                                 if (parse) {
                                     lab <- parse_safe(base::as.character(lab))
                                 }

                                 data <- coord$transform(data, panel_params)
                                 aleph <<- data
                                 if (base::is.character(data$vjust)) {
                                     data$vjust <- compute_just(data$vjust, data$y)
                                 }
                                 if (base::is.character(data$hjust)) {
                                     data$hjust <- compute_just(data$hjust, data$x)
                                 }

                                 te <- grid::textGrob(
                                     lab,
                                     data$x, coords$y + 0.11, default.units = "native",
                                     hjust = 0, vjust = data$vjust,
                                     rot = data$angle,
                                     gp = grid::gpar(
                                         col = scales::alpha(data$colour, data$alpha),
                                         fontsize = data$size * .pt,
                                         fontfamily = data$family,
                                         fontface = data$fontface,
                                         lineheight = data$lineheight
                                     ),
                                     check.overlap = check_overlap
                                 )

                                 grid::gTree(children = grid::gList(se, te))

                             }
)

#' Visualise Earthquake Epicenters on a Geographical Map
#'
#' The function maps the epicenters (LATITUDE/LONGITUDE) and annotates each point
#' with a pop-up window containing annotation data stored in a column of the data
#' frame. Each earthquake is shown with a circle; the radius of the circle is
#' proportional to the earthquake's magnitude (EQ_PRIMARY).
#'
#' @param df A clean data frame outputted by the function code{link{eq_clean_data}}
#' @param annot_col A column used for the annotation in the pop-up window
#'
#' @importFrom leaflet addTiles addCircleMarkers leaflet
#' @importFrom magrittr %>%
#'
#' @examples
#' usa_japan %>%
#'     quakes::eq_map(annot_col = "DATE")
#'
#' @export
eq_map <- function(df, annot_col) {

    leaflet::leaflet() %>%
        leaflet::addTiles() %>%
        leaflet::addCircleMarkers(data = df,
                                  lng = ~ LONGITUDE,
                                  lat = ~ LATITUDE,
                                  radius = ~ base::as.numeric(EQ_PRIMARY),
                                  weight = 2,
                                  popup = ~ base::eval(base::parse(text = annot_col)))
}

#' Create More Informative Pop-Ups for Geographic Map
#'
#' The function takes the dataset and creates an HTML label that can be used as
#' the annotation text in the leaflet map outptted by the code{link{eq_create_label}}
#' function.
#'
#' @param df A clean data frame outputted by the code{link{eq_clean_data}} function.
#'
#' @return A character vector of HTML_formatted labels.
#'
#' @details The function puts together a character string for each earthquake that
#' shows the location (which is cleaned inside this function by the
#' code{link{eq_location_clean}} function), the magnitude (EQ_PRIMARY), and the
#' total number of deaths (TOTAL_DEATHS). If an earthquake is missing values for
#' any of these, both the label and the value are skipped for that element of the tag.
#'
#' @importFrom dplyr if_else mutate pull
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @examples
#' usa_japan %>%
#'     quakes::eq_create_label()
#'
#' @export
eq_create_label <- function(df) {

    df %>%
        eq_location_clean() %>%
        dplyr::mutate(LABEL1 = dplyr::if_else(base::is.na(.data$LOCATION_NAME), "",
                                              base::paste0("<b>Location:</b> ", .data$LOCATION_NAME, "<br>")),
                      LABEL2 = dplyr::if_else(base::is.na(.data$EQ_PRIMARY), "",
                                              base::paste0("<b>Magnitude:</b> ", .data$EQ_PRIMARY, "<br>")),
                      LABEL3 = dplyr::if_else(base::is.na(.data$TOTAL_DEATHS), "",
                                              base::paste0("<b>Total deaths:</b> ", .data$TOTAL_DEATHS)),
                      LABEL = base::paste0(.data$LABEL1, .data$LABEL2, .data$LABEL3)) %>%
        dplyr::pull(.data$LABEL)
}
