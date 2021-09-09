# slicotr - An R interface to the SLICOT library.

A wrapper for the SLICOT control library, leaning heavily on the slycot python project.

SLICOT provides a large number of robust and stable numerical algorithms for computations of relevance to control theory and linear dynamic systems. 

Of particular note are the solvers for Riccati, Lyapunov and Sylvester equations.

## Installation

You can install the development version from github using devtools:

``` r
# install.packages("devtools")
devtools::install_github("rjhknight/slicotr")
```

The installation requires compiliation of a large number of fortran files. 

- For installation on Windows, ensure RTools is installed (https://cran.r-project.org/bin/windows/Rtools/)
- For installation on macOS, install Apple Xcode 10.1 and gFortran from https://github.com/fxcoudert/gfortran-for-macOS/releases 
 

## References
- http://slicot.org/
- https://github.com/python-control/Slycot
