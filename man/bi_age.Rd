% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/biota.R
\name{bi_age}
\alias{bi_age}
\title{by_age}
\usage{
bi_age(con, std = TRUE, trim = TRUE)
}
\arguments{
\item{con}{oracle connection}

\item{std}{create standardized/shortcut names (default is TRUE)}

\item{trim}{trim return only key variables (default is TRUE). only operational
if std is TRUE}
}
\value{
a tibble query
}
\description{
Reads biota.aldur_v
}
