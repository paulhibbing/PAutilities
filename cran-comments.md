
## Resubmission

This is an updated version of PAutilities, in which the
    following changes have been made:
    
* Added paired equivalence testing functionality, including plot method
* Added a function to classify activity intensity from metabolic equivalents and
    (if available) posture
* Added a wrapper for `base::rle` that gives a data frame with original `rle`
    information, plus start/stop indices for each run
* Added various performance indicators to `summary.transition`
* Converted `summary.transition` output to an S4 framework, and added
    addition/subraction methods
* Added spurious curve generation functions
* Adjusted Transition Pairing Method to make it more robust (e.g. to deal with
    missing values)

## Test environments

* local Windows 10 install, R 3.5.0
* ubuntu 16.04.6 (on travis-ci; devel and release)
* win-builder (devel and release)

## R CMD check results

0 ERRORs | 0 WARNINGs | 0 NOTEs

## Reverse dependencies

* AGread (0 errors | 0 warnings | 0 notes)
