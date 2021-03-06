% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{chrono_strati_label}
\alias{chrono_strati_label}
\title{add label and tick}
\usage{
chrono_strati_label(
  offset = 6,
  tick_length = 1,
  label_offset = 2,
  line_params = list(),
  size = 3.5,
  ...
)
}
\arguments{
\item{offset}{numeric scalar. How much is the axis higher than bar.}

\item{tick_length}{numeric scalar. How long is the tick.}

\item{label_offset}{numeric scalar.  How much is the label higher than the axis.}

\item{line_params}{other arguments passed on to \code{\link[ggplot2:geom_line]{ggplot2::geom_line()}} and \link[ggplot2:annotate]{ggplot2::annotate('segment')}}

\item{size, ...}{other arguments passed on to \code{\link[ggplot2:geom_text]{ggplot2::geom_text()}}}
}
\description{
add label and tick
}
