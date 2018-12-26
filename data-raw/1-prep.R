usethis::use_package("dplyr")
usethis::use_package("zoo")
usethis::use_package("data.table", "suggests")
usethis::use_package("feather", "suggests")


# BLSdatafolder <- "C://Users//pellettc//Documents//Data//BLS//"

NAICS <- read.csv("industry_titles.csv",
                  stringsAsFactors=FALSE) %>%
  mutate(industry_code = as.character(industry_code))
## This has been edited lightly to break up the 31-33, 44-45, and 48-49 NAICS codes
## into different rows (31, 32, 33, etc.)
## There were entries in this table representing a range of codes with a hyphen
## I fixed that in Excel

own_titles <- tribble(
  ~own_code, ~ownership_title,
  "0",   "Total Covered",
  "1",   "Federal Government",
  "2",   "State Government",
  "3",   "Local Government",
  "4",   "International Government",
  "5",   "Private")



# QCEW2016 <- read_QCEW_singlefile(2016)
# QCEW2016 %<>% join_QCEW_titles()
# QCEW2016_monthly <-  convert_QCEW_to_monthly(QCEW2016)

# QCEW_monthly %<>% filter(month < 2016)
# QCEW_monthly <- bind_rows(QCEW_monthly, QCEW2016_monthly, QCEW2017_monthly)
# rm(QCEW2016, QCEW2016_monthly, QCEW2017_monthly)

## save the csv singlefiles as .RDS to conserve memory
# for(i in 2016:2017) {
#   f <- paste0(as.character(i), ".q1-q4.singlefile")
#   fread(paste0(f, '.csv')) %>% saveRDS(paste0(f, '.RDS'))}
## The .csv 's are 38.4GB
## the .RDS 's are 5.8GB
## I'm going to delete the .csv 's

# QCEW_monthly %<>% left_join(own_titles)

# QCEW_monthly %<>% filter(area_fips %in% as.character(45000:45999))

write_feather(QCEW_monthly, 'data-raw\\QCEW_monthly')

## QCEW_monthly <- feather::read_feather('data-raw//QCEW_monthly')

# filter(QCEW_monthly, month >= 2012) %>%
#   mutate(month=as.yearmon(month)) %>%
#   mutate(month=as_date(month)) %>%
#   spread(test, month, employees) %>%
#   write_excel_csv("QCEW_monthly.csv")


### agglevel 75, 3 digit NAIC codes by county and ownership
### seems to match the best with the SC DEW Projections. (actually, maybe not by ownership...)
filter(QCEW_monthly, agglvl_code=='75' & month >= 1995) %>%
  select(Area = area_title, fips=area_fips, Industry = industry_title,
         NAICS=industry_code, Ownership=ownership_title, month, employees) %>%
  write_excel_csv("QCEW_monthly.csv")


