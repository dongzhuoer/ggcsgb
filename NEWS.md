# ggcsgb 0.4.0

## Breaking changes

1. Rename to ggcsgb (**gg**plot **c**hrono**s**trati**g**raphic **b**ackground).
1. `chrono_strati_arg()` abandons `start` parameter, which is calculated by `max(ggplot2::ggplot_build(plot)$data[[1]]$xend)` internally.
1. Example tree files are replaced by `ape::rtree()`, since they are not attached to publications of Prof. Xie



# ggcsgb 0.3.0

## New features

1. add documentation via pkgdown



# ggcsgb 0.2.1

## Minor improvements and fixes

1. use ggplot2 release version



# ggcsgb 0.2.0

## New features

1. support `+ .gg` syntax



# ggcsgb 0.1.0

## New features

1. complete three layers, can work for Wang2017

