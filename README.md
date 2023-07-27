
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Implementation of the InSiGHTS framework

<!-- badges: start -->
<!-- badges: end -->

This R-package provides a IIASA implementation of the InSiGHTS Index of
Habitat Availability. The index captures the amount of suitable habitat
within the current or a future range of a species. This range can be
taken either from existing range maps (e.g. IUCN) or from estimates
obtained through species distribution models.

In it’s basic configuration, the InSiGHTS framework combines the
climatic suitability from a SDM with a area of habitat (AOH) refinement
to obtain the suitable habitat for each time steps. The InSiGHTS Index
of Habitat Availability can then be defined for any given species $s$
and timestep $t$ as:

$Insights_{s,t} = \frac{AOH_{s,t} - AOH_{s, t_{ref}}}{AOH_{s, t_{ref}}}$,
where $t_{ref}$ indicates a reference or starting year.

More information on the InSiGHTS framework can be found in [Rondini and
Visconti 2015](https://doi.org/10.1111/cobi.12532), [Visconti et
al. 2016](https://doi.org/10.1111/conl.12159) or [Baisero et
al. (2021)](https://doi.org/10.1016/j.oneear.2020.05.015).

The package is part of the
[IIASA-BEC](https://iiasa.ac.at/programs/bnr/bec) suite of biodiversity
indicators and is coupled with the
[ibis.iSDM](https://iiasa.github.io/ibis.iSDM/) species distribution
model.

## Installation

You can install the development version of Insights from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("iiasa/insights")
```

The package depends on the [aoh](https://prioritizr.github.io/aoh/) and
the [ibis.iSDM](https://iiasa.github.io/ibis.iSDM/) package, both of
which are currently only available via github.

## Basic usage and examples

*To be done*

Note that the package is mostly for internal use and full
reproducibility is not the primary goal of this package.

**Citations:**

P. Visconti, M. Bakkenes, D. Baisero, T. Brooks, S.H.M. Butchart, L.
Joppa, R. Alkemade, M. Di Marco, L. Santini, M. Hoffmann, C. Rondinini
Projecting global biodiversity indicators under future development
scenarios Conserv. Lett., 9 (2016), pp. 5-13
[DOI](https://doi.org/10.1111/conl.12159)

C. Rondinini and P. Visconti, Scenarios of large mammal loss in Europe
for the 21st century Conserv. Biol., 29 (2015), pp. 1028-1036
[DOI](https://doi.org/10.1111/cobi.12532)

## Acknowledgement <a href="https://iiasa.ac.at"><img src="man/figures/IIASA_PNG_logo_blue.png" alt="IIASA" align="right" width="300"/></a>

**InSiGHTS** is developed and maintained by the Biodiversity, Ecology
and Conservation group at the International Institute for Applied
Systems Analysis (IIASA), Austria.
