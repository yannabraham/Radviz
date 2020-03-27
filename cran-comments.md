## New release of Radviz package

This is release 0.9.1 of Radviz, where we have made the following changes

 * fixed compilation errors for C++ code
 * import hexbin package to support ggplot2-driven geom_hex
 * anchors can now be filtered based on length
 * rescaling will issue a warning if points extend beyond some anchors
 * rescaling uses projected coordinates rather than trying to compute them again
 * Radviz object will include the transformation used in the creation of the object

## Test environments

* local windows 10, R 3.6.1
* Ubuntu 16.04.6 LTS (on travis-ci), R 3.6.1
* win-builder (release) 
* win-builder (devel)
* R-hub
    * Windows Server 2008 R2 SP1, R-devel, 32/64 bit
    * Ubuntu Linux 16.04 LTS, R-release, GCC
    * Fedora Linux, R-devel, clang, gfortran
    * Debian Linux, R-devel, GCC ASAN/UBSAN

## R CMD check results

There were no ERRORS or WARNINGS

 * for r-hub with `Windows Server 2008 R2 SP1, R-devel, 32/64 bit`, `Ubuntu Linux 16.04 LTS, R-release, GCC`,
 `Fedora Linux, R-devel, clang, gfortran`, `win-builder-devel` and `win-builder-release` there are NOTES about 
 possibly mis-spelled words in DESCRIPTION
 
       Demsar (18:70)
       Freeviz (19:56)
  
  those words are spelled correctly. 

## Reverse dependencies

This is an update, with no reverse dependencies.
