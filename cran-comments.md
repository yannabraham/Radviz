## New release of Radviz package

This is release 0.9.3 of Radviz, where we have made the following changes

 * fixed the plotting function to enable use of interactive frameworks eg plotly
 * added a scaling option to do.radviz following Artur & Minghim 2019
 * implemented anchor.filter as a separate function to make it easier to use in
 different plots
 * updated plot functions to avoid code duplication
 * updated the cosine function to make it faster
 * minor corrections to documentation and default values

## Test environments

 * local windows 10, R 4.1.2
 * On GitHub
     * windows-latest (R-release)
     * macos-latest (R-release)
     * ubuntu-latest (R-release, R-devel)
 * Through R-hub
     * Windows Server 2022, R-devel, 64 bit
     * Ubuntu Linux 20.04.1 LTS, R-release, GCC
     * Fedora Linux, R-devel, clang, gfortran
     * Debian Linux, R-devel, GCC ASAN/UBSAN
     
## R CMD check results

There were no ERRORS or WARNINGS

 * for r-hub with `Windows Server 2008 R2 SP1, R-devel, 32/64 bit`, 
 `Ubuntu Linux 16.04 LTS, R-release, GCC`, `Fedora Linux, R-devel, clang, gfortran`,
 `win-builder-devel` and `win-builder-release` there are NOTES about possibly 
 misspelled words in DESCRIPTION
 
       Demsar (18:70)
       Freeviz (19:56)
  
  those words are spelled correctly. 

 * for Debian Linux, R-devel, GCC ASAN/UBSAN there are errors in the compilation of the RcppArmadillo package that prevent the test to complete

## Reverse dependencies

This is an update, with no reverse dependencies.
