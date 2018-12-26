########### Reviewing the DEW projections


dnr_load(QCEW2)
QCEW <- QCEW2 %>%
  filter(!(agglvl_code %in% c("76", "77", "78"))) ## These agglvls aren't in DEW projections

dnr_load(NAICS)

CEW <- mutate(QCEW, Year = year(Date)) %>%
  group_by(Industry, NAIC, agglvl_title, County, Year, Date) %>%
  summarise(Employees = sum(Employees, na.rm=TRUE)) %>%
  ## This summarises over the different ownership categories. Bad idea?
  ungroup() %>% group_by(Year, NAIC, County, Industry, agglvl_title) %>%
  summarise(Employees = mean(Employees, na.rm=TRUE)) %>%
  ## This summarises monthly data to yearly, good idea.
  ungroup() %>% left_join(DEWcountyJoin) # %>%
  # left_join(NAICS, by=c("NAIC"="industry_code"))

## There are 297 unique NAICs with Federal Government ownership
## 212 with State Government ownership
## 321 with Local Government ownership.

## DEW has NAIC code 1028 Government, 90 Government,
## 91 Federal Government, 92 Total State Government, 93 Total Local Government
## 1028 Government == 90 Government == Federal + State + Local

## QCEW has NAIC codes 1028 and 92, both called Public Administration

## I need to join agglevel_codes to DEW
## probably good to pull them out of the QCEW
# agglvls <- select(QCEW, agglvl_code, agglvl_title, NAIC, Industry, Ownership) %>%
#   unique() %>% arrange(agglvl_code, Ownership) ## 307 entries

dnr_load(DEWproj)



CEW2 <- group_by(CEW, Year, WDA, NAIC, Industry, agglvl_title) %>%
  # group_by(CEW, Year, WDA, NAIC, industry_title) %>%
  summarise(Employees = sum(Employees, na.rm=TRUE)) %>%
  ungroup() # %>%
  # semi_join(DEW2, by="NAIC")

anti_join(CEW2, DEW2, by="NAIC") %>% .[,c("Industry", "agglvl_title")] %>% unique()
## 20 industries. mostly government.

inner_join(CEW2, DEW2, suffix=c(".CEW", ".DEW"), by=c("WDA", "Year", "NAIC")) %>%
  ggplot(aes(y=Employees.CEW, x=Employees.DEW, tooltip=Industry.CEW, color=WDA)) +
  geom_abline(intercept = 0, slope = 1) + #geom_text() +
  # coord_fixed(xlim=c(1000,150000), ylim=c(1000,150000)) +
  scale_x_log10() + scale_y_log10() +
  facet_wrap(~agglvl_title, scales="free") +
  geom_point_interactive() -> g

ggiraph(code=print(g))

## Make a widget to review this! select WDA, see timeseries
## leaflet.minicharts animated barchart
## with integrated timeseries chart ...

z <- bind_rows(QCEW=CEW2, DEW=DEW2, .id='Source')

## Separate NAIC subsectors from supersectors
subsectors <- filter(z, startsWith(Industry, "NAIC")) %>%
  dplyr::select(NAIC) %>% unique() %>% .$NAIC


################# DEW subsector plot

### {r DEW-subsectorPlot, fig.height=18, out.height="1000%"}

## Subsector Plot
# z %>% filter(Year >= 2000 & !is.na(WDA)) %>%
#   mutate(Level = nchar(NAIC)) %>% # -> z2
#   saveRDS("83-DEW-App//z.rds")
## The above is wrong, nchar(NAIC) isn't what I need
## Instead, should join agglvls to DEW


z %>%
# filter(NAIC %in% subsectors) %>%
  filter(Year >= 2000 & !is.na(WDA)) %>%
  ggplot(aes(x=Year, y=log10(Employees), color=NAIC,
              tooltip=Industry, linetype=Source)) +
  facet_wrap(~WDA, scales='free', ncol=4) +
  theme(legend.position='none') +
  scale_linetype_manual(values=c("dotted", "solid")) +
  geom_line_interactive() -> g

ggiraph(code=print(g), zoom_max=10)

####### Comments on the Join
### 1 digit NAIC code


#### 2 digit NAIC code
## DEW has Educational Service (state, local, and private) (2 digit NAIC)
## So, probably want to add up educational for all ownership in QCEW

## DEW also has Government, total State Government, total Local government, Federal Gov.
## at the 2 digit NAIC
## So, probably want to add up those ownership levels in the QCEW


#### 3 Digit NAIC code
## Y axis is skewed by 101 (goods) and 102 (services)
## those probably don't belong with the rest of the 3 digit codes.

#### 4 digit NAIC code
## Natural resources and mining seems consistently higher in DEW than QCEW

rm(DEW, DEW2, CEW, CEW2, z, DEWcountyJoin, QCEW, g, NAICS)



###################### QCEW join tests


## a function to see the NAIC categories in QCEW and DEW at each county agglvl
QCEWjoinTest <- function(agglvl) {
 filter(QCEW, agglvl_code==agglvl)[,c("NAIC", "Industry")] %>% unique() %>%
    left_join(unique(DEW[,c("NAIC","industry_title")]))
}

## These results indicate that we can join the QCEW to DEW projections
## at multiple different agg levels. And we can drill in to the QCEW beyond
View(QCEWjoinTest("72")) # Domain works for 2 of 2
View(QCEWjoinTest("73")) # Supersector works for 12 of 12 (including unclassified)
View(QCEWjoinTest("74")) # Sector works for 17 of 21 (48-49, 31-33, 44-45, and 99 fail)
View(QCEWjoinTest("75")) # NAIC3 works for 82 of 100 (postal, gov., some ag and mining fail)


View(QCEWjoinTest("76")) # NAIC4 works for 2 of 318; 4911 postal service and 1133 logging.
View(QCEWjoinTest("77")) # NAIC5 works for 0 of 726
View(QCEWjoinTest("78")) # NAIC6 works for 0 of 1248
