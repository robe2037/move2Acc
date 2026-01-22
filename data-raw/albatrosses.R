options("keyring_backend" = "env")

move2::movebank_store_credentials(
  Sys.getenv("MOVEBANK_USERNAME"),
  Sys.getenv("MOVEBANK_PASSWORD")
)

# Galapagos Albatrosses
d1 <- as.Date("2008-07-27")

albatrosses <- move2::movebank_download_study(
  2911040, 
  sensor_type_id = c("gps", "acceleration"),
  timestamp_start = as.POSIXct(d1),
  timestamp_end = as.POSIXct(d1) + 3600
)

usethis::use_data(albatrosses, overwrite = TRUE)
