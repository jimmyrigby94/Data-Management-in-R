library(tidyverse)

# Read the data
clinician<-read_csv("suppl/clinician.csv")

# Cleaning Data
clean_clinician<-clinician%>%
  filter(attrition!=1)%>% # Removing patients that no longer attend therapy
  select(-Pull_Date)%>% # Dropping Variables with no Variance
  rename(beck1 = Q1.1, beck2 = Q1.2, beck3 = Q1.3, swb1 = Q2.1, swb2 = Q2.2, swb3 = Q2.3)%>% # renaming to meaningful variables
  filter_at(.vars = vars(beck1, beck2, beck3, swb1, swb2, swb3), .vars_predicate = any_vars(!is.na(.)))%>% # Removing variables if all missing
  mutate(beck_arith = (beck1+beck2+beck3)/3, swb_arith = (swb1+swb2+swb3)/3)%>% #Using arithmetic methods for means
  rowwise()%>% # Creating scale scores with missing data
  mutate(beck_fun = mean(c(beck1, beck2, beck3), na.rm = TRUE), swb_fun = mean(c(beck1, beck2, beck3), na.rm = TRUE))%>% # Using functional means
  ungroup()%>%
  mutate(dep_cdf = cume_dist(beck_fun))

# Extracting Group level Features
 clean_clinician<-clean_clinician%>%
    group_by(Therapist)%>%
    mutate(dep_cdf_grp = mean(beck_fun, na.rm = TRUE),
           attr_count = n())%>%
   ungroup()
 
 # Summary Tables
 
 clean_clinician%>%
   summarise_at(vars(beck_fun, swb_fun), )

