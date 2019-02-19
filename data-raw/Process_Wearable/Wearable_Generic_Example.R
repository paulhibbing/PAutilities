rm(list = ls())
devtools::load_all()

counts <- system.file("extdata", "Example.csv", package = "AGread")

imu <-
  system.file("extdata",
    "TestID_LeftWrist_IMU.csv",
    package = "AGread")

raw <-
  system.file("extdata",
    "TestID_LeftWrist_RAW.csv",
    package = "AGread")

test <- process_wearable(
  file = counts, IMU = imu, RAW = raw, device = "ActiGraph",
  Count_Args = c(skip = 11)
)

test2 <- process_wearable(IMU = imu, RAW = raw, device = "ActiGraph")
