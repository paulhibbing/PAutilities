
## Resubmission

This is an updated version of PAutilities, in which the
    following changes have been made:
    
* Added a function (weight_status) to classify body mass index for adults
    using CDC cutoffs. (This can also be used as a wrapper for the existing
    function that classifies youth BMI based on CDC percentiles.)
    
* Added Rcpp functions related to rolling windows (`get_indices` to extract
    indices for rolling windows; `rolling_groups` to extract rolling
    subsequences of raw data).
    
* Removed dependency on orphaned `clues` package (resulting in changes to
    unit test cache)

## Test environments

* local Windows 10 install, R 3.5.0
* ubuntu 16.04.6 (on travis-ci; devel and release)
* win-builder (devel and release)
* macOS 10.13.6 High Sierra (on R-hub; devel)
* Debian Linux 9.3.0-11 (on R-hub; devel)

## R CMD check results

0 ERRORs | 0 WARNINGs | 0 NOTEs

## Reverse dependencies

* AGread (0 errors | 0 warnings | 0 notes)
