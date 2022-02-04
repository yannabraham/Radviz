# Radviz

[![R-CMD-check](https://github.com/yannabraham/Radviz/workflows/R-CMD-check/badge.svg)](https://github.com/yannabraham/Radviz/actions)
[![CRAN version](https://www.r-pkg.org/badges/version/Radviz)](https://cran.r-project.org/package=Radviz)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/Radviz)](https://cran.r-project.org/package=Radviz)
[![DOI](https://zenodo.org/badge/46946711.svg)](https://zenodo.org/badge/latestdoi/46946711)

The Radviz package implements the concept of dimensional anchors to visualize multivariate datasets in a 2D projection, as originally described in [Hoffman *et al* 1999](https://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.135.889). It implements 3 optimization method for the order of anchors:

 - the original implementation based on [Di Caro *et al* (2012)](https://link.springer.com/chapter/10.1007/978-3-642-13672-6_13)
 - the Freeviz method based on [Demsar *et al.* (2007)](https://doi.org/10.1016/j.jbi.2007.03.010)
 - a new graph layout method, based on Freeviz

## Installation

To install the CRAN version run

```
install.packages('Radviz')
```

Development version of Radviz can be installed using the following command

```
devtools::install_github('yannabraham/Radviz')
```

Or, if you want to build the vignette:

```
devtools::install_github('yannabraham/Radviz',build_vignettes = TRUE)
```
