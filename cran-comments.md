## Resubmission

This is effectively a resubmission of `PAutilities`, which was previously
archived when one of its dependencies became unavailable (`matchingMarkets`).
The latter dependency is now available again, and thus we are resubmitting.
Changes are as follows:

-   Minor tweaks (e.g., URL fixes and updates to work with latest R versions)
-   Removed revdep since there are currently no reverse dependencies
-   Updated .onAttach to indicate newer build version of R

## Test environments

-   local Windows 10 install, R 4.2.2
-   win-builder (devel and release)
-   macOS on R-hub
-   Linux on R-hub

## R CMD check results

0 ERRORs \| 0 WARNINGs \| 1 NOTEs

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Paul R. Hibbing <paulhibbing@gmail.com>'

  New submission

  Package was archived on CRAN

  CRAN repository db overrides:
  X-CRAN-Comment: Archived on 2023-04-19 as issues were not corrected in time.
    
  [The issues have been fixed, per the above explanation]
