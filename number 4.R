## @knitr query4

task4 <- dbGetQuery(con,"SELECT DISTINCT ON (a.subject_id) a.subject_id, string_agg(DISTINCT a.icd9_code, ', ') as icd9, 
string_agg(DISTINCT b.short_title, ', ') as short_title,
CASE
    WHEN MAX(c.deathtime) BETWEEN MAX(d.intime)-interval'6 hours' AND MAX(d.outtime)+interval'6 hours' THEN 'Death'
	ELSE 'Not Death'
END AS icu_expire, d.first_careunit, d.los
FROM
  diagnoses_icd a
INNER JOIN d_icd_diagnoses b ON a.icd9_code=b.icd9_code
INNER JOIN admissions c ON a.subject_id = c.subject_id
INNER JOIN icustays d ON a.subject_id = d.subject_id
GROUP BY a.subject_id, d.outtime, c.deathtime, d.first_careunit, d.los
ORDER BY a.subject_id, d.outtime DESC, c.deathtime DESC")

task4 <- task4 %>% filter(str_detect(icd9, 'V450'))

data4 <- inner_join(x = data[ , c("subject_id", "admitage_yrs", "sex", "vaccinated")], y = task4, by = "subject_id")

data4 <- data4 %>% filter(icu_expire == 'Death')

## @knitr table5

data4$short_title <- stringr::str_trunc(data4$short_title, 40)
data4$icd9 <- stringr::str_trunc(data4$icd9, 40)
data4$los <- round(data4$los,2)
table4 <- data.frame(id = data4$subject_id,
                     gender_age = paste(data4$sex,'(',data4$admitage_yrs,')'), 
                     first_careunit_days = paste(data4$first_careunit,'(',round(data4$los,2),')'), 
                     icd9 = data4$icd9, short_title = data4$short_title)
