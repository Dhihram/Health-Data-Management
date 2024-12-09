## @knitr query5

task5_2 <- read.csv('https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv')
task5_2 <- task5_2 %>% filter(location == 'United Kingdom')
task5_2$date <- as.Date(task5_2$date)
task5_2 <- data.frame(date = task5_2$date, cases = task5_2$new_cases, cases_smooth = task5_2$new_cases_smoothed,
                      death = task5_2$new_deaths, death_smooth = task5_2$new_deaths_smoothed)

data5 <- filter(task5_2, between(date, as.Date("2021-01-01"), as.Date("2021-03-31")))

month_cases <- data5 %>%
  group_by(month = lubridate::floor_date(date, 'month')) %>%
  summarize(month_cases = sum(cases), month_death = sum(death))
dif <- month_cases %>%
  mutate(dif_cases = month_cases - lag(month_cases), 
         percent_dif_cases = round((dif_cases/month_cases)*100,1),
         dif_death = month_death - lag(month_death), 
         percent_dif_death = round((dif_death/month_death)*100,1)) %>% ungroup()
dif$dif_cases <- prettyNum(dif$dif_cases,big.mark=",", preserve.width="none")
dif$dif_text1 <- paste(dif$dif_cases,'(',dif$percent_dif_cases,'%',')')
dif$dif_death <- prettyNum(dif$dif_death,big.mark=",", preserve.width="none")
dif$dif_text2 <- paste(dif$dif_death,'(',dif$percent_dif_death,'%',')')

## @knitr plot2

p1 <- ggplot(data = data5, mapping = aes(x = date, y = cases)) +
  geom_line(linewidth=0.8) +geom_ribbon(data=subset(data5,date>='2021-03-01' & date<='2021-03-31'),aes(ymax=cases),ymin=0,
                                        fill=ifelse(last(dif$dif_cases) > 0, 'red', 'darkgreen'),colour=NA,alpha=0.5)+ scale_y_continuous(labels=comma)+
  labs(title="Daily Cases Jan-Mar 2021") + xlab('Date') + ylab(' ')+ 
  geom_ribbon(data=subset(data5,date>='2021-02-01' & date<='2021-03-01'),aes(ymax=cases),ymin=0,
              fill=ifelse(last(dif$dif_cases) > 0, 'red', 'darkgreen'),colour=NA,alpha=0.2)
p1 <- p1 + annotation_custom(textGrob('Difference from Previous Month:', gp = gpar(col = 'black', fontsize = 7)), 
                             xmin = max(data5$date)-50, xmax = max(data5$date), ymin = max(data5$cases)-20, ymax = max(data5$cases)) +
  annotation_custom(textGrob(last(dif$dif_text1), gp = gpar(col = ifelse(last(dif$dif_cases) > 0, 'red', 'darkgreen'), fontsize = 10, fontface = 'bold')), 
                    xmin = max(data5$date)-50, xmax = max(data5$date), ymin = max(data5$cases)-100, ymax = max(data5$cases)-15000) + theme_minimal(base_size = 8)

p2 <- p <- ggplot(data = data5, mapping = aes(x = date, y = death)) +
  geom_line(linewidth=0.8) +geom_ribbon(data=subset(data5,date>='2021-03-01' & date<='2021-03-31'),aes(ymax=death),ymin=0,
                                        fill=ifelse(last(dif$dif_death) > 0, 'red', 'darkgreen'),colour=NA,alpha=0.5)+ scale_y_continuous(labels=comma)+
  labs(title="Daily Deaths Jan-Mar 2021",caption = 'Source: Our World in Data (12/31/2023)') + xlab('Date') + ylab(' ')+ 
  geom_ribbon(data=subset(data5,date>='2021-02-01' & date<='2021-03-01'),aes(ymax=death),ymin=0,
              fill=ifelse(last(dif$dif_death) > 0, 'red', 'darkgreen'),colour=NA,alpha=0.2)
p2 <- p + annotation_custom(textGrob('Difference from Previous Month:', gp = gpar(col = 'black', fontsize = 7)), 
                            xmin = max(data5$date)-50, xmax = max(data5$date), ymin = max(data5$death)-20, ymax = max(data5$death)) +
  annotation_custom(textGrob(last(dif$dif_text2), gp = gpar(col = ifelse(last(dif$dif_death) > 0, 'red', 'darkgreen'), fontsize = 10, fontface = 'bold')), 
                    xmin = max(data5$date)-50, xmax = max(data5$date), ymin = max(data5$death)-10, ymax = max(data5$death)-300) + theme_minimal(base_size = 8)

p1+p2