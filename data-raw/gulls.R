options("keyring_backend" = "env")

move2::movebank_store_credentials(
  Sys.getenv("MOVEBANK_USERNAME"),
  Sys.getenv("MOVEBANK_PASSWORD")
)

# LBBG_ZEEBRUGGE - Lesser black-backed gulls
d2 <- as.Date("2021-03-03")

gulls <- move2::movebank_download_study(
  985143423,
  sensor_type_id = c("gps", "acceleration"),
  timestamp_start = as.POSIXct(d2),
  timestamp_end = as.POSIXct(d2 + 1)
)

saveRDS(gulls, "inst/extdata/gulls.rds")