# postcode district populaion & add to drivers

# from here https://www.nomisweb.co.uk/census/2011/ks101ew

library(dplyr)

pcode_dist_pops <- read.csv("pcode dist 2011 pop.csv") %>%
  janitor::clean_names()

names(pcode_dist_pops)[5] <- "all_usual_residents"

pcode_dist_pops <- pcode_dist_pops %>%
  select(-c(rural_urban))

# add to licenses

licenses <- readRDS("pcode dist driving licenses.RDS")
licenses2 <- left_join(licenses, pcode_dist_pops, 
                      by = c("pcode_district" = "geography")
) %>%
  mutate(full_licenses_per_resident = full_total/ all_usual_residents)


# save for app

saveRDS(licenses2,
        "./vehicles_licenses/pcode dist driving licenses.RDS")
