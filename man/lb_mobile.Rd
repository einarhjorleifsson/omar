% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lb.R
\name{lb_mobile}
\alias{lb_mobile}
\title{Logbook mobile (active) gear}
\usage{
lb_mobile(con, correct_gear = TRUE, trim = TRUE)
}
\arguments{
\item{con}{oracle connection}

\item{correct_gear}{a boolean (default TRUE) checks for lookup-table for
gear correction (adds variable "gidc" to the tibble)}

\item{trim}{trim variables returned (default TRUE)}
}
\value{
a sql tibble
}
\description{
Logbook mobile (active) gear
}
