download_QCEW_singlefile <- function(year) {
  http <- paste0("https://data.bls.gov/cew/data/files/", year,
                 "/csv/", year, "_qtrly_singlefile.zip")
  # download http
  # unzip http
}

## read a csv
read_QCEW_singlefile <- function(
  year, q = 'q1-q4',
  filter_area_fips = c(as.character(45000:45999), ## SC
                       # as.character(37000:37999), ## NC
                       # as.character(13000:13999), ## GA
                       'US000'), ## US Total
  select_columns = c('area_fips', 'own_code', 'industry_code',
                     'agglvl_code', 'year', 'qtr',
                     'qtrly_estabs', 'month1_emplvl', 'month2_emplvl',
                     'month3_emplvl', 'avg_wkly_wage')) {

  fread(paste0(as.character(year), ".", q, ".singlefile.csv")) %>%
    select(one_of(select_columns)) %>%
    filter(area_fips %in% filter_area_fips)
}


join_QCEW_titles <- function(QCEW){
  ## Read in titles for industry and area codes
  ## Join them to the data
  area_titles <- read.csv(
    'https://data.bls.gov/cew/doc/titles/area/area_titles.csv',
    stringsAsFactors=FALSE) %>%
    mutate(area_title = str_replace(area_title, "South Carolina", "SC"))

  NAICS <- read.csv(
    'https://data.bls.gov/cew/doc/titles/industry/industry_titles.csv',
    stringsAsFactors=FALSE) %>%
    mutate(industry_title = str_replace(industry_title, "NAICS ", ""))

  agglevels <- read.csv(
    "https://data.bls.gov/cew/doc/titles/agglevel/agglevel_titles.csv",
    stringsAsFactors=FALSE) %>%
    mutate(agglvl_code = as.character(agglvl_code))

  own_titles <- tribble(
      ~own_code, ~ownership_title,
      "0",   "Total Covered",
      "1",   "Federal Government",
      "2",   "State Government",
      "3",   "Local Government",
      "4",   "International Government",
      "5",   "Private")

  QCEW %>% left_join(area_titles) %>% left_join(NAICS) %>%
    left_join(agglevels) %>% left_join(own_titles)
}

## separate quarterly reported values (establishments and wages)
## from monthly reported values (employees)
# QCEW_qtrly <- select(QCEW, -month1_emplvl, -month2_emplvl, -month3_emplvl)

convert_QCEW_to_monthly <- function(QCEW) {
  QCEW_monthly <- select(QCEW, -avg_wkly_wage, -qtrly_estabs) %>%
    rename(`0`=month1_emplvl, `1`=month2_emplvl, `2`=month3_emplvl) %>%
    gather(key='month_of_qtr', value='employees', `0`, `1`, `2`) %>%
    mutate(qtr = as.numeric(qtr),
           month_of_qtr = as.numeric(month_of_qtr),
           year = as.numeric(year))
  QCEW_monthly$qtr_adjustment <- recode(QCEW_monthly$qtr,
                                        `1`=0,
                                        `2`=3,
                                        `3`=6,
                                        `4`=9)
  QCEW_monthly %>% mutate(month=yearmon(year + (month_of_qtr+qtr_adjustment)/12)) %>%
    select(-qtr_adjustment, -qtr, -month_of_qtr, -year)
}

# test <- read.csv('QCEW_monthly.csv')
