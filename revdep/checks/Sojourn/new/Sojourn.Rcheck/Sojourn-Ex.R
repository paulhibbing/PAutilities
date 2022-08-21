pkgname <- "Sojourn"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('Sojourn')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("apply_youth_sojourn")
### * apply_youth_sojourn

flush(stderr()); flush(stdout())

### Name: apply_youth_sojourn
### Title: Apply the youth Sojourn method
### Aliases: apply_youth_sojourn

### ** Examples

data(example_data, package = "Sojourn")



cleanEx()
nameEx("compute.bouts.info")
### * compute.bouts.info

flush(stderr()); flush(stdout())

### Name: compute.bouts.info
### Title: Summarize outcomes from data processed using the Sojourn method
### Aliases: compute.bouts.info

### ** Examples

data(example_data, package = "Sojourn")
example_data <- soj_3x_original(
  example_data$axis1,
  example_data$axis2,
  example_data$axis3,
  example_data$Vector.Magnitude
)

compute.bouts.info(example_data$METs)




cleanEx()
nameEx("enhance_actigraph")
### * enhance_actigraph

flush(stderr()); flush(stdout())

### Name: enhance_actigraph
### Title: Combine ActiGraph and activPAL data
### Aliases: enhance_actigraph

### ** Examples

data(SIP_ag, package = "Sojourn")
data(SIP_ap, package = "Sojourn")
combined_data <- enhance_actigraph(SIP_ag, SIP_ap)
utils::head(combined_data)



cleanEx()
nameEx("get_youth_sojourns")
### * get_youth_sojourns

flush(stderr()); flush(stdout())

### Name: get_youth_sojourns
### Title: Label Sojourns in a data stream according to the youth-specific
###   algorithm
### Aliases: get_youth_sojourns

### ** Examples

data(example_data, package = "Sojourn")
get_youth_sojourns(example_data$Vector.Magnitude,
  Output = "Counts", Site = "Hip")



cleanEx()
nameEx("input_demographic")
### * input_demographic

flush(stderr()); flush(stdout())

### Name: input_demographic
### Title: Interactively input demographic information
### Aliases: input_demographic

### ** Examples

if (interactive()) {
  input_demographic()
}



cleanEx()
nameEx("read_AP")
### * read_AP

flush(stderr()); flush(stdout())

### Name: read_AP
### Title: Read an activPAL events file
### Aliases: read_AP

### ** Examples

ap_file <- system.file(
"extdata/sampledata_Events.csv",
package = "Sojourn"
)
if (isTRUE(requireNamespace("data.table"))) {
  ap_data <- read_AP(ap_file)
  utils::head(ap_data)
}



cleanEx()
nameEx("soj_1x_original")
### * soj_1x_original

flush(stderr()); flush(stdout())

### Name: soj_1x_original
### Title: Invoke the original uni-axial Sojourn method
### Aliases: soj_1x_original

### ** Examples

data(example_data, package = "Sojourn")
results_1x <- soj_1x_original(example_data$axis1)
utils::head(results_1x)



cleanEx()
nameEx("soj_3x_original")
### * soj_3x_original

flush(stderr()); flush(stdout())

### Name: soj_3x_original
### Title: Invoke the original triaxial Sojourn method
### Aliases: soj_3x_original

### ** Examples

data(example_data, package = "Sojourn")
results_3x <- soj_3x_original(
  example_data$axis1,
  example_data$axis2,
  example_data$axis3,
  example_data$Vector.Magnitude
)

utils::head(results_3x)




cleanEx()
nameEx("sojourn_3x_SIP")
### * sojourn_3x_SIP

flush(stderr()); flush(stdout())

### Name: sojourn_3x_SIP
### Title: Triaxial Sojourn method for the SIP method
### Aliases: sojourn_3x_SIP

### ** Examples

data(SIP_ag, package = "Sojourn")
data(SIP_ap, package = "Sojourn")
data <- Sojourn::enhance_actigraph(SIP_ag, SIP_ap)
utils::head(sojourn_3x_SIP(data))



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
