
## Resubmission

This is an updated version of PAutilities, in which the
    following changes have been made:
    
* Removed unused dependency on package `AGread`
* Added support for calculation of RMR via sliding window analysis
* Added process management utility for easy messaging and timekeeping
* Added tools to compare objects more specifically than via `all.equal`
* Added `na.rm = TRUE` to the mean bias line on Bland-Altman plots
* Added shape control as an option for Bland-Altman plots
* Cleaned up TPM summary method
* Added rejection of non-consecutive pairings to the TPM

## Test environments

* local Windows 10 install, R 3.5.0
* ubuntu 14.04.5 (on travis-ci; devel and release)
* win-builder (devel and release)

## R CMD check results

0 ERRORs | 0 WARNINGs | 0 NOTEs

## Reverse dependencies

There are currently no reverse dependencies for PAutilities.
