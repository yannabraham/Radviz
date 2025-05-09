## New release of Radviz package

This is release 0.9.4 of Radviz, where we have made the following changes

 * minor corrections to documentation and default values

## Test environments

* aarch64-apple-darwin20, R 4.4.3
* macosx.latest (release) (github actions)
* ubuntu.latest (devel, release and oldrel-1) (github actions)
* windows.latest (release) (github actions)
* win-builder (devel and release)
     
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
