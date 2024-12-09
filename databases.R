## @knitr mimic
mimic <- getPass(msg = "Please enter the MIMIC password!")
con <- dbConnect(RPostgres::Postgres(), 
                 dbname = "mimic", 
                 host = "healthdatascience.lshtm.ac.uk", 
                 port = 5432,
                 user = "student",
                 password = mimic)

## @knitr odk
odk <- getPass(msg = "Please enter the ODK password!")
ruODK::ru_setup(
  svc = "https://odk-survey.lshtm.ac.uk/v1/projects/80/forms/HDM_Assessment.svc",
  un = "chrissyhroberts.tutor@gmail.com",
  pw = odk)
fq_svc <- ruODK::odata_service_get()

form_tables <- ruODK::odata_service_get()
data <- odata_submission_get()
data$subject_id <- as.numeric(gsub('_', '', data$subject_id))
data <- data[order(data$subject_id),]
data$vaccinated <- ifelse(data$vaccinated == 'NO', '0', '1+ doses')
