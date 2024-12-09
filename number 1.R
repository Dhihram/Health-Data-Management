## @knitr query

task1 <- dbGetQuery(con, "SELECT DISTINCT ON (subject_id) subject_id, insurance, language, marital_status, 
ethnicity FROM admissions
ORDER BY subject_id, admittime DESC")

data1 <-left_join(data[ , c("subject_id",'admitage_yrs', "sex", "vaccinated")], task1, by='subject_id')

data1$language[is.na(data1$language)] <- 'UNKNOWN'
data1$marital_status[is.na(data1$marital_status)] <- 'UNKNOWN (DEFAULT)'
data1$ethnicity[is.na(data1$ethnicity)] <- 'UNKNOWN'
data1$insurance[is.na(data1$insurance)] <- 'Unknown'

data1 <- data1 %>%
  mutate(age_group = cut(admitage_yrs, breaks = c(0, 17, 44, 64, 74, 120), 
                         labels = c("0-17", "18-44", "45-64", "65-74", "75+"), 
                         include.lowest = TRUE))

data1$ethnicity_group <- ifelse(str_detect(data1$ethnicity, "WHITE"), "White", 
                                ifelse(str_detect(data1$ethnicity, "MULTI"), "Mixed", 
                                       ifelse(str_detect(data1$ethnicity, "ASIAN"), "Asian", 
                                              ifelse(str_detect(data1$ethnicity, "BLACK"), "Black",
                                                     ifelse(str_detect(data1$ethnicity, 'UNKNOWN'), 'Unknown', 'Others'
                                                     )))))
data1$language_group <- ifelse(data1$language == "ENGL", 'English', 
                               ifelse(data1$language == "UNKNOWN", 'Unknown', 'Non-English')) 

data1$marital_status <- str_to_title(data1$marital_status)
data1$marital_status[data1$marital_status == 'Unknown (Default)'] <- 'Unknown'

## @knitr table1-2

data1$vaccinated <- str_to_title(data1$vaccinated)

label(data1$admitage_yrs) <- "Age"
label(data1$sex)<-"Gender"
label(data1$age_group)<-"Age Group"
label(data1$vaccinated)    <- "Vaccination"
label(data1$language_group) <- "Language"
label(data1$ethnicity_group) <- "Ethnicity"
label(data1$marital_status) <- "Marital Status"
label(data1$vaccinated) <- "Vaccinated"
label(data1$insurance) <- "Insurance"
units(data1$admitage_yrs)   <- "years"

render.categorical <- function(x, ...) {
  c("", sapply(stats.apply.rounding(stats.default(x)), function(y) with(y,
                                                                        sprintf("%s (%s%%)", prettyNum(FREQ, big.mark=","), PCT))))
}

render.strat <- function (label, n, ...) {
  sprintf("<span class='stratlabel'>%s<br><span class='stratn'>(N=%s)</span></span>", 
          label, prettyNum(n, big.mark=","))
}

tab1 <- table1(~sex + age_group + insurance + language_group, data=data1, render.categorical=render.categorical, render.strat=render.strat, 
               caption = 'Demographics and Vaccine Status (1)')
tab2 <- table1(~ethnicity_group + marital_status + vaccinated, render.categorical=render.categorical, render.strat=render.strat,
               data=data1, caption = 'Demographics and Vaccine Status (2)')
