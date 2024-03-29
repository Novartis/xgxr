% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xgx_breaks_time.R
\name{xgx_breaks_time}
\alias{xgx_breaks_time}
\title{Sets the default breaks for a time axis}
\usage{
xgx_breaks_time(data_range, units_plot, number_breaks = 5)
}
\arguments{
\item{data_range}{range of the data}

\item{units_plot}{units to use in the plot}

\item{number_breaks}{number of breaks to aim for (default is 5)}
}
\value{
numeric vector of breaks
}
\description{
\code{xgx_breaks_time} sets the default breaks for a time axis, 
given the units of the data and the units of the plot.
It is inspired by scales::extended_breaks
}
\details{
for the extended breaks function, weights is a set of 4 weights for
\enumerate{
\item simplicity - how early in the Q order are you
\item coverage - labelings that don't extend outside the data:
range(data) / range(labels)
\item density (previously granularity) - how close to the number of ticks 
do you get (default is 5)
\item legibility - has to do with fontsize and formatting to prevent 
label overlap
}
}
\examples{
xgx_breaks_time(c(0, 5), "hour")
xgx_breaks_time(c(0, 6), "hour")
xgx_breaks_time(c(-3, 5), "hour")
xgx_breaks_time(c(0, 24), "hour")
xgx_breaks_time(c(0, 12), "hour")
xgx_breaks_time(c(1, 4), "day")
xgx_breaks_time(c(1, 12), "day")
xgx_breaks_time(c(1, 14), "day")
xgx_breaks_time(c(1, 50), "day")
xgx_breaks_time(c(1000, 3000), "day")
xgx_breaks_time(c(-21, 100), "day")
xgx_breaks_time(c(-1, 10), "week")

}
\references{
Talbot, Justin, Sharon Lin, and Pat Hanrahan.
"An extension of Wilkinson’s algorithm for positioning tick labels on axes."
IEEE Transactions on visualization and 
computer graphics 16.6 (2010): 1036-1043.
}
