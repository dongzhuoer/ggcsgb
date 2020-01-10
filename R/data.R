#' @title demo period data from Cretaceous to Ordovician
#'
#' @format A data frame with 4 rows and 10 variables:
#'
#'   - period: period name - begin: when the period begins, million years ago -
#'   color: hex code
#'
#' @details the first and last period name is ignored. The `begin` of last
#'   period is also ignored, the that of first period is very important. Since
#'   you have to specify the _end_ of second period.
#'
#'   It may sounds wired, but it is really hard to build a "normal" data.frame
#'   since there are n period and n+1 time point. I think repeating n-1 time
#'   point twice is not a good idea so I designed above format.
#'
#' @source [http://www.stratigraphy.org/index.php/ics-chart-timescale]
#"Phanerozoic"