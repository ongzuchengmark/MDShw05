% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/delay_ann.R
\name{delay_ann}
\alias{delay_ann}
\title{MTA NYC subway delay announcement}
\usage{
delay_ann(line_no = "0", delay_time = 0)
}
\arguments{
\item{line_no}{Subway line number.}

\item{delay_time}{Expected time delay in minutes.}
}
\description{
Allows one to report train delay in New York City subways. It will work for any of the 23 MTA lines in New York.
}
\examples{
delay_ann()
delay_ann("1", 5)
delay_ann("C", 10)
}
