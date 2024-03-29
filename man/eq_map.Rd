% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/funs.R
\name{eq_map}
\alias{eq_map}
\title{Visualise Earthquake Epicenters on a Geographical Map}
\usage{
eq_map(df, annot_col)
}
\arguments{
\item{df}{A clean data frame outputted by the function code{link{eq_clean_data}}}

\item{annot_col}{A column used for the annotation in the pop-up window}
}
\description{
The function maps the epicenters (LATITUDE/LONGITUDE) and annotates each point
with a pop-up window containing annotation data stored in a column of the data
frame. Each earthquake is shown with a circle; the radius of the circle is
proportional to the earthquake's magnitude (EQ_PRIMARY).
}
\examples{
usa_japan \%>\%
    quakes::eq_map(annot_col = "DATE")

}
