## @knitr query2

task2 <- dbGetQuery(con, "SELECT a.subject_id, EXTRACT(EPOCH FROM (a.dischtime - a.admittime)) / 86400 AS loa, 
b.los FROM admissions a 
INNER JOIN icustays b ON a.subject_id=b.subject_id
ORDER BY a.subject_id")
data2 <- inner_join(x = data[ , c("subject_id", "admitage_yrs", "sex", "vaccinated")], y = task2, by = "subject_id")
data2$age_cat <- ifelse(data2$admitage_yrs >= 60, '60+', '<60')
data2 <-data2 %>%  filter(loa>=0)

sex_loa <- data2 %>% group_by(sex) %>% summarize(min = round(min(loa, na.rm=TRUE),2), 
                                                 q25 = round(quantile(loa, probs = .25, na.rm = TRUE),2),
                                                 median = round(median(loa),2),
                                                 q75 = round(quantile(loa, probs = .75, na.rm=TRUE),2),
                                                 max = round(max(loa, na.rm=TRUE),2)) %>% ungroup() %>% mutate_at(vars(sex), funs(as.character(.))) %>%
  bind_rows(summarise(sex = "All Gender", data2, min = round(min(loa, na.rm=TRUE),2), 
                      q25 = round(quantile(loa, probs = .25, na.rm = TRUE),2),
                      median = round(median(loa, na.rm =TRUE),2),
                      q75 = round(quantile(loa, probs = .75, na.rm=TRUE),2),
                      max = round(max(loa, na.rm=TRUE),2)))

vac_loa <- data2 %>% group_by(vaccinated) %>% summarize(min = round(min(loa, na.rm=TRUE),2), 
                                                        q25 = round(quantile(loa, probs = .25, na.rm = TRUE),2),
                                                        median = round(median(loa),2),
                                                        q75 = round(quantile(loa, probs = .75, na.rm=TRUE),2),
                                                        max = round(max(loa, na.rm=TRUE),2)) %>% ungroup() %>% mutate_at(vars(vaccinated), funs(as.character(.))) %>%
  bind_rows(summarise(vaccinated = "All Vaccinated", data2, min = round(min(loa, na.rm=TRUE),2), 
                      q25 = round(quantile(loa, probs = .25, na.rm = TRUE),2),
                      median = round(median(loa, na.rm =TRUE),2),
                      q75 = round(quantile(loa, probs = .75, na.rm=TRUE),2),
                      max = round(max(loa, na.rm=TRUE),2)))

age_cat_loa <- data2 %>% group_by(age_cat) %>% summarize(min = round(min(loa, na.rm=TRUE),2), 
                                                         q25 = round(quantile(loa, probs = .25, na.rm = TRUE),2),
                                                         median = round(median(loa),2),
                                                         q75 = round(quantile(loa, probs = .75, na.rm=TRUE),2),
                                                         max = round(max(loa, na.rm=TRUE),2)) %>% ungroup() %>% mutate_at(vars(age_cat), funs(as.character(.))) %>%
  bind_rows(summarise(age_cat = "All Age", data2, min = round(min(loa, na.rm=TRUE),2), 
                      q25 = round(quantile(loa, probs = .25, na.rm = TRUE),2),
                      median = round(median(loa, na.rm =TRUE),2),
                      q75 = round(quantile(loa, probs = .75, na.rm=TRUE),2),
                      max = round(max(loa, na.rm=TRUE),2)))

sex_los <- data2 %>% group_by(sex) %>% summarize(min = round(min(los, na.rm=TRUE),2), 
                                                 q25 = round(quantile(los, probs = .25, na.rm = TRUE),2),
                                                 median = round(quantile(los, probs = .5, na.rm =TRUE),2),
                                                 q75 = round(quantile(los, probs = .75, na.rm=TRUE),2),
                                                 max = round(max(los, na.rm=TRUE),2)) %>% ungroup() %>% mutate_at(vars(sex), funs(as.character(.))) %>%
  bind_rows(summarise(sex = "All Gender", data2, min = round(min(los, na.rm=TRUE),2), 
                      q25 = round(quantile(los, probs = .25, na.rm = TRUE),2),
                      median = round(quantile(los, probs = .5, na.rm =TRUE),2),
                      q75 = round(quantile(los, probs = .75, na.rm=TRUE),2),
                      max = round(max(los, na.rm=TRUE),2)))

vac_los <- data2 %>% group_by(vaccinated) %>% summarize(min = round(min(los, na.rm=TRUE),2), 
                                                        q25 = round(quantile(los, probs = .25, na.rm = TRUE),2),
                                                        median = round(quantile(los, probs = .5, na.rm =TRUE),2),
                                                        q75 = round(quantile(los, probs = .75, na.rm=TRUE),2),
                                                        max = round(max(los, na.rm=TRUE),2)) %>% ungroup() %>% mutate_at(vars(vaccinated), funs(as.character(.))) %>%
  bind_rows(summarise(vaccinated = "All Vaccinated", data2, min = round(min(los, na.rm=TRUE),2), 
                      q25 = round(quantile(los, probs = .25, na.rm = TRUE),2),
                      median = round(quantile(los, probs = .5, na.rm =TRUE),2),
                      q75 = round(quantile(los, probs = .75, na.rm=TRUE),2),
                      max = round(max(los, na.rm=TRUE),2)))

age_cat_los <- data2 %>% group_by(age_cat) %>% summarize(min = round(min(los, na.rm=TRUE),2), 
                                                         q25 = round(quantile(los, probs = .25, na.rm = TRUE),2),
                                                         median = round(quantile(los, probs = .5, na.rm =TRUE),2),
                                                         q75 = round(quantile(los, probs = .75, na.rm=TRUE),2),
                                                         max = round(max(los, na.rm=TRUE),2)) %>% ungroup() %>% mutate_at(vars(age_cat), funs(as.character(.))) %>%
  bind_rows(summarise(age_cat = "All Age", data2, min = round(min(los, na.rm=TRUE),2), 
                      q25 = round(quantile(los, probs = .25, na.rm = TRUE),2),
                      median = round(quantile(los, probs = .5, na.rm =TRUE),2),
                      q75 = round(quantile(los, probs = .75, na.rm=TRUE),2),
                      max = round(max(los, na.rm=TRUE),2)))

## @knitr table3-4

loa <- rbind(sex_loa,         # Rename columns & rbind
             setNames(vac_loa, names(sex_loa)))
loa <- rbind(loa,         # Rename columns & rbind
             setNames(age_cat_loa, names(loa)))
names(loa)[1] <- "Stratification"

los <- rbind(sex_los,         # Rename columns & rbind
             setNames(vac_los, names(sex_los)))
los <- rbind(los,         # Rename columns & rbind
             setNames(age_cat_los, names(los)))
names(los)[1] <- "Stratification"
