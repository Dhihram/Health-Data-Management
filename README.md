# Health Data Management Patient Database

This is the repository for database management of patient database from UK hospital where patient data is
captured in the MIMIC III database. This is the task from Hospital management to generate an executive
summary report synthesising recent patient data, survey data they provided, and ur World in Data for COVID-19 data.

This repo contains:
* `README` as the readme file </p>
* `executive_report.qmd` contains libraries and quarto files to render the executive report. this file will use all R files for the pipelines </p>
* `databases.R` contains the access to ODK and MIMIC databases. Those requires password to access both databases </p>
* `number1.R` is Patients Demographic task including query and tables </p>
* `number2.R` is Patient's Hospital Stays taks including query and tables </p>
* `number3.R` is Patient with Cardiac Devices ICU Periods task including query and plots </p>
* `number4.R` is List of Expire Patients with Cardiac Devices task including query and table </p>
* `number5.R` is COVID-19 in Community task including Our World in Data data query and plots </p>
* `feedback_notes.pdf` is the notes to data warehouse manager </p>
