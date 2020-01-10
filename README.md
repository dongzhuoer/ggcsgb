# ggcsgb
[![Build Status](https://travis-ci.com/dongzhuoer/ggcsgb.svg?token=CyRgUWsqWCctKvAxMXto&branch=master)](https://travis-ci.com/dongzhuoer/ggcsgb)


## Overview

This package mimics ggplot2’s `+` syntax to add [**c**hrono**s**trati**g**raphic](http://www.stratigraphy.org/index.php/ics-chart-timescale) **b**ackground for [chronogram](https://en.wikipedia.org/wiki/Phylogenetic_tree#Chronogram) (coalescent time tree). 



## Installation

```r
if (!('remotes' %in% .packages(T))) install.packages('remotes');
remotes::install_github('dongzhuoer/ggcsgb');
```



## Usage

refer to `vignette('ggcsgb')`.

You also look at this **ggtree** guide: [ggtree: Elegant Graphics for Phylogenetic Tree Visualization and Annotation](https://guangchuangyu.github.io/ggtree-book/)




## `data-raw/chromostratigraphic_chart.csv`

- adjust according to Chinese version: 
    + `Pennsylvanian` and `Mississippian` become epoch prefix
    + `Precambrian` becomes eon prefix
    + `Hadean` becomes an eon
- Age and begin raw has been calibrated
