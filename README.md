
<!-- README.md is generated from README.Rmd. Please edit that file -->

# shortIRT

<!-- badges: start -->

<!-- badges: end -->

The goal of shortIRT is to simple tool for the development of static
short test forms (STFs) in an Item Response Theory (IRT) based
framework. Specifically, two main procedures are considered:

1.  The typical IRT-based procedure for the development of STFs (here
    denoted as benchmark procedure, BP), according to which the most
    informative items are selected without considering any specific
    level of the latent trait

2.  The IRT procedure based on the definition of levels of interest of
    the latent trait (i.e., $\theta$ targets, here denoted as
    $\theta$-target procedure). The selected items are those most
    informative in respect to the $\theta$ targets. This procedure can
    be further categorized according to the methodology used for the
    definition of the $\theta$ targets:

    1.  Equal interval procedure (EIP): The latent trait is divided in
        $n + 1$ (where $n$ is the number of items to be included in the
        STF) intervals of equal width and the central points of each
        interval are the $\theta$ targets
    2.  Unequal interval procedure (UIP): The latent trait is clustered
        in $n$ clusters (where $n$ is the number of items to be included
        in the STF) and centroids of each clusters are the $\theta$
        targets
    3.  User defined procedure (UDP): The user manually defines the
        $\theta$ targets to which the STF should tend to. They might
        also be the same $\theta$ values (e.g., for the development of a
        screening STF with a cut-off point).

## Installation

You can install the development version of shortIRT from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("OttaviaE/shortIRT")
```
