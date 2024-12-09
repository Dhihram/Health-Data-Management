## @knitr query3

task3 <- dbGetQuery(con,"SELECT DISTINCT ON (a.subject_id) a.subject_id,
CASE
    WHEN a.deathtime BETWEEN c.intime-interval'6 hours' AND c.outtime+interval'6 hours' THEN 'Death'
	ELSE 'Not Death'
END AS icu_expire,
c.los, EXTRACT(EPOCH FROM (a.dischtime - a.admittime)) / 86400 AS loa,
    d.icd9_code,
    e.short_title FROM admissions a 
INNER JOIN icustays c ON a.subject_id=c.subject_id
INNER JOIN diagnoses_icd d ON a.subject_id=d.subject_id
INNER JOIN d_icd_diagnoses e ON d.icd9_code=e.icd9_code
WHERE d.icd9_code LIKE 'V450%'
ORDER BY a.subject_id, c.outtime DESC, a.deathtime DESC")

data3 <- inner_join(x = data[ , c("subject_id", "sex", "vaccinated")], y = task3, by = "subject_id")

## @knitr plot

p1 <- ggplot(data3, aes(x=sex, y=los)) + geom_violin(trim=FALSE, fill='grey', color="firebrick")+
  geom_boxplot(width=0.1) + labs(title="By Sex") + xlab(' ') + ylab('ICU Stay (Days)') +theme_minimal(base_size = 7)
p2 <- ggplot(data3, aes(x=icu_expire, y=los)) + geom_violin(trim=FALSE, fill='grey', color="firebrick")+
  geom_boxplot(width=0.1) + labs(title="By ICU Expire") + xlab(' ') + ylab('') + theme_minimal(base_size = 7)
p3 <- ggplot(data3, aes(x=vaccinated, y=los)) + geom_violin(trim=FALSE, fill='grey', color="firebrick")+
  geom_boxplot(width=0.1) + labs(title="By Vaccinated") + xlab(' ') + ylab('') + theme_minimal(base_size = 7)

p1+p2+p3