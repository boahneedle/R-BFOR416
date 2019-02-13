#IM GONNA PREDICT THE WEATHER
#WEATHER STATION ID: 725180-14735
#PREREQs install.packages("GSODR")
library(GSODR)
library(ggplot2)
library(lubridate)
source('forecast_functions.R')
#STORE THE last ten years in well last_ten_years
last_ten_years <-  get_GSOD(years = 2009:2019, station = '725180-14735')

saveRDS(object = last_ten_years, file = 'last_ten_alb.rds')

last_ten_years <- readRDS('last_ten_alb.rds')
current_year <- get_GSOD(years = 2019, station = '725180-14735')
#Subset new_rows
new_rows <- subset(current_year, !(YEARMODA %in% last_ten_years$YEARMODA))
last_ten_years <- rbind(last_ten_years, new_rows)
last_ten_years$I_PRCP <- ifelse(last_ten_years$PRCP > 0, 1, 0)
saveRDS(object = last_ten_years, file = 'last_ten_alb.rds')

#SHOW DATA FOR LAST TEN YEARS
qplot(x = last_ten_years$YEARMODA, y = last_ten_years$MAX)

#
yoy <- ggplot(last_ten_years, aes(x=YDAY, y=MAX, color=YEAR))
##########yo2 <- ggplot(current_year, aes(x=YDAY, y=MAX, color=YEAR))
#HISTORICAL_YEAR 
yoy + geom_point() + geom_smooth()
#CURRENT_YEAR
##########yo2 + geom_point() + geom_smooth()



# GET DATES I WANT TO PREDICT
#Take the systemdate, add one to it store this in prediction_start
prediction_start <- Sys.Date() - days(3)
#Prediction starts with previous 7 days
#Take the current date, subtract 7 from dates
start_prev_seven <- prediction_start - days(7)
#set the scope of the days that we want to evaluate, this would be start & start +6 then filter by days
eval.dates <- seq(prediction_start, prediction_start + 6, by='days')
#convert to data frame, store into eval_df_base
eval_df_base <- as.data.frame(eval.dates)
str(eval_df_base)




forecast_name <- 'acapece'
prev_seven <- subset(last_ten_years, YEARMODA >= start_prev_seven & 
                       YEARMODA < prediction_start)
mean.max <- mean(prev_seven$MAX) 
mean.min <- mean(prev_seven$MIN)
mean.prcp <- mean(prev_seven$I_PRCP)

pred.df.list <- list()
pred.df.list[['max']] <- forecast_df_builder(eval.dates, 'acapece', 'MAX', 
                                             rep(mean.max, 7))
pred.df.list[['min']] <- forecast_df_builder(eval.dates, 'acapece', 
                                             'MIN', rep(mean.min, 7))
pred.df.list[['i_prcp']] <- forecast_df_builder(eval.dates, 'acapece', 
                                                'I_PRCP', rep(mean.prcp, 7))
pred.df <- do.call(rbind, pred.df.list)

write.csv(pred.df,"2-10forecast.csv")


