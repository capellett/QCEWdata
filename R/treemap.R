# NAICStreemap <- filter(NAICS, nchar(industry_code) == 6) %>%
#   mutate(NAICS2 = str_sub(industry_code, 1, 2),
#          NAICS3 = str_sub(industry_code, 1, 3),
#          NAICS4 = str_sub(industry_code, 1, 4),
#          NAICS5 = str_sub(industry_code, 1, 4)) %>%
#   rename(NAICS6 = industry_code, title6 = industry_title) %>%
#   left_join(NAICS, by=c('NAICS2'='industry_code')) %>%
#   rename(title2 = industry_title) %>%
#   left_join(NAICS, by=c('NAICS3'='industry_code')) %>%
#   rename(title3 = industry_title) %>%
#   left_join(NAICS, by=c('NAICS4'='industry_code')) %>%
#   rename(title4 = industry_title) %>%
#   left_join(NAICS, by=c('NAICS5'='industry_code')) %>%
#   rename(title5 = industry_title) %>%
#   select(title2, title3, title4, title5, title6) %>%
#   mutate(size=1)
