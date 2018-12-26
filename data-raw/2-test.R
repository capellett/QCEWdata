############## I haven't run any of the code below in a while
############## but i think it means that agglevel 75 is what i need.


####### Reviewing the QCEW

dnr_load(QCEW)
dnr_load(NAICS)

# NAICS <- read.csv(
#   'https://data.bls.gov/cew/doc/titles/industry/industry_titles.csv',
#   stringsAsFactors=FALSE) %>%
#   mutate(industry_title = str_replace(industry_title, "NAICS ", ""))

## This test indicates that summarizing different agglevels yields different results.
QCEWtest <- function(x, y) {
  filter(x, agglvl_code==y) %>%
  group_by(Area, Date) %>%
  summarise(Employees= sum(Employees, na.rm=TRUE)) %>%
  ungroup() %>% arrange(Area, Date) }

test0 <- QCEWtest(QCEW2, '70')
test1 <- QCEWtest(QCEW2, '71')
test2 <- QCEWtest(QCEW2, '72')
test3 <- QCEWtest(QCEW2, '73')
test4 <- QCEWtest(QCEW2, '74')
test5 <- QCEWtest(QCEW2, '75')
test6 <- QCEWtest(QCEW2, '76')
test7 <- QCEWtest(QCEW2, '77')
test8 <- QCEWtest(QCEW2, '78')

## This test shows which results from QCEWtest() are causing the summary different results
Qtest2 <- function(a, b) {
  left_join(
  anti_join(a, b),
  anti_join(b, a),
  by=c("Area", "Date"))
}

t0 <- Qtest2(test0, test1)
t1 <- Qtest2(test1, test2)
t2 <- Qtest2(test2, test3)
t3 <- Qtest2(test3, test4)
t4 <- Qtest2(test4, test5)
t5 <- Qtest2(test5, test6)
t6 <- Qtest2(test6, test7)
t7 <- Qtest2(test7, test8)

## How many NAIC Industries are there at each aggregation level?
filter(QCEW2, agglvl_code=="70")$Industry %>% unique() # 1: Total
filter(QCEW2, agglvl_code=="71")$Industry %>% unique() # 1: Total
filter(QCEW2, agglvl_code=="72")$Industry %>% unique() # 2: Goods, services
filter(QCEW2, agglvl_code=="73")$Industry %>%
  unique() # 11 + "1029 Unclassified"
filter(QCEW2, agglvl_code=="74")$Industry %>%
  unique() # 20 + "99 Unclassified"
filter(QCEW2, agglvl_code=="75")$Industry %>%
  unique() # 100 + "999 Unclassified"
filter(QCEW2, agglvl_code=="76")$Industry %>%
  unique() # 318 + "9999 Unclassified"
filter(QCEW2, agglvl_code=="77")$Industry %>%
  unique() # 725 + "99999 Unclassified"
filter(QCEW2, agglvl_code=="78")$Industry %>%
  unique() # 1248

y <- QCEW2 %>%
  filter(Ownership!="Total Covered") %>%
  mutate(NAIC = str_sub(NAIC, 1, 2)) %>%
  left_join(NAICS, by=c("NAIC"="industry_code")) %>%
  group_by(industry_title, County, Date) %>%
  summarise(Employees = sum(Employees, na.rm=TRUE)) %>%
  rename(Industry=industry_title) %>%
  ungroup() %>%
  # filter(Date >= 2012) %>%
  select(Industry, County, Date, Employees)

# saveRDS(y, "81-QCEW-App//QCEW.rds")
# saveTemp(y, "QCEW-81.rds")

# y$Industry <- sub("^\\w+\\s+\\w+\\s+", "", y$Industry)

QCEW2 %<>% filter(agglvl_code < 76 # & Ownership!="Total Covered"
                ) %>%
  dplyr::select(Industry, NAIC, Ownership, Employees,
         Date, agglvl_title, County=Area)

dnr_save(QCEW2)

# rm(y, QCEW)
