
# This script downloads the monthly maximum temperature and monthly precipitation
# for november 2012 through march 2013 from the CHELSA climate data base and stores
# it in the "data/maxtemp" and "data/precipitation" folders, respectively. URLS for
# downloads are up-to-date and working as of April 27th, 2021.
# Script takes 3-5 minutes to run
# CHELSA data is publicly available and free at https://chelsa-climate.org/

# Attach packages
library(purrr)

# get urls of climate data
tmax_urls   <- readLines("code/chelsa_tmax.txt")
precip_urls <- readLines("code/chelsa_precip.txt")

#use a list of months to name files after the month their data was observed
months <- c(
  "nov_2012",
    "dec_2012",
    "jan_2013",
    "feb_2013",
    "mar_2013"
  )

# download maximum temperature files
purrr::walk2(
  .x = tmax_urls,
  .y = months,
  .f = ~download.file(
    url = .x,
    destfile = paste0("data/maxtemp/", .y, ".tif"),
    mode = "wb"
  )
)

# download average precipitation files
purrr::walk2(
  .x = precip_urls,
  .y = months,
  .f = ~download.file(
    url = .x,
    destfile = paste0("data/precipitation/", .y, ".tif"),
    mode = "wb"
  )
)
