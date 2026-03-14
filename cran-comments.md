
## Submission

This is an updated submission with the following changes:

-   Reversed operand order when calculating bias in `ba_analysis`
-   Added a reintegration function
-   Updated ggplot2 code and dependency
-   Modernized `roxygen2` usage

## Test environments

-   local Windows 11 install, R 4.5.3
-   win-builder (devel and release)
-   Linux on R-hub

## R CMD check results

0 ERRORs \| 0 WARNINGs \| 1 NOTEs

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Paul R. Hibbing <paulhibbing@gmail.com>'

  Found the following (possibly) invalid DOIs:
     DOI: 10.1136/bmj.313.7049.106
       From: DESCRIPTION
       Status: Forbidden
       Message: 403
    
  [This is a false positive. The link has already been used successfully on CRAN in PAutilities version 1.2.1]
  
## Reverse dependencies

* No adverse changes for any reverse dependencies (n = 2)
