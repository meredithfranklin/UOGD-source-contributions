### Author: Meredith Franklin 
### Date: 12/17/2025
### This code reads in the data from github
### Merges data on VOCs at 10 minutes within an hour, matched with the other compounds at that 10 minutes

library(tidyverse)
library(lubridate)

# base raw GitHub URL
base_url <- "https://raw.githubusercontent.com/meredithfranklin/UOGD-source-contributions/main/data/VOC-sampling-window"

# file names on GitHub
filenames <- c(
  "LNM_h2s_so2_voc_merge.csv",
  "Boulder_AIR_LNM_ch4_vocwindow_finalized_Oct30.csv",
  "Boulder_AIR_LNM_nox_finalized.csv",
  "Boulder_AIR_LNM_co_finalized.csv",
  "Boulder_AIR_LNM_o3_finalized.csv",
  "Boulder_AIR_LNM_rd_finalized.csv",
  "Boulder_AIR_LNM_met_finalized_w_rain_wvmr_vocwindow.csv"
)

# read from GitHub
read_skip_second_github <- function(filename) {
  url <- file.path(base_url, filename)
  lines <- readLines(url)
  lines <- lines[-2]  # remove second row
  read_csv(paste(lines, collapse = "\n"))
}

# put files into a list
data_list <- map(filenames, read_skip_second_github)

# merge by time_utc and create mountain
hourly_data <- reduce(data_list, full_join, by = "time_utc") %>%
  rename(co2 = co2_ppm) %>%
  mutate(
    datetime_mountain = with_tz(
      as.POSIXct(time_utc, tz = "UTC",
                 format = "%Y-%m-%d %H:%M:%OS"),
      tzone = "America/Denver"
    )
  ) %>%
  filter(
    date(datetime_mountain) >= as.Date("2023-05-01"),
    date(datetime_mountain) <= as.Date("2024-05-31")
  ) %>%
  arrange(datetime_mountain) %>%
  select(datetime_mountain, everything())

write.csv(hourly_data, "merged_for_nmf.csv", row.names = F)