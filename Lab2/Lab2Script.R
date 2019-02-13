#Set csData to read the excel sheet
csData <- read.csv('Most-Recent-Cohorts-Scorecard-Elements.csv')
#Read the columns in csData 1:6
str(csData[,1:6])
#Show the column ACTMT75
#This will print out the columns as Characters because there are null values, when R sees strings in a column it sets the "categories" to Factors
str(csData$ACTMT75)
#This will show the unique "categories", still including NULL values and using Factor
levels(csData$ACTMT75)

#Reset the variable to now not accept NULL values
csData <- read.csv('Most-Recent-Cohorts-Scorecard-Elements.csv', na.strings = c('NULL','NA'))
#Check to make sure that the ACT column now filters out SQL Null, This will now show the ACTMT75 Column with only variables and not characters like "null"
#This should now correctly print as integers
str(csData$ACTMT75)

#Count the number of rows that have "na" 
sum(is.na(csData$ACTMT75))

#count the number of rows that DONT have "na"
sum(!is.na(csData$ACTMT75))

#Read the name of colleges
str(csData$INSTNM)

#Set the INSTNM column to read as characters, This is only changing a specific column
csData[, 'INSTNM'] <- sapply(csData[, 'INSTNM'], as.character)

#Print
str(csData$INSTNM)

#set the variable not operating to a subset of csData which shows the non opperating colleges
notoperating <- subset(csData, CURROPER == 0)

nrow(notoperating)

#Specifying ever further, Not look for not operating and in NY
notOperating.NY <- subset(csData, CURROPER == 1 & STABBR == 'NY')
unique(notOperating.NY$INSTNM)
nrow(notOperating.NY)


#WORKING WITH COLUMNS
#Using percentage of degrees awarded in the following areas
csData$BCSE <- csData$PCIP11 + csData$PCIP14 + csData$PCIP52

#Which schools produce lots of BCSE students (at least 80% of graduates come from these fields)?
highBCSE <- subset(csData, BCSE > 0.8)

head(highBCSE[c('INSTNM',"BCSE")])

# The data frame is not sorted. We can sort it (note the minus sign in -highBCSE\$BCSE, this orders in descending order).
head(highBCSE[order(-highBCSE$BCSE), c('INSTNM',"BCSE")], n = 10)


#This shows the number of rows of schools in new york 
nrow(ny.data <- subset(csData, STABBR == 'NY'))

#Create a histogram of the ACTCMMID (Midpoint cumulative ACT score) column
hist(ny.data$ACTCMMID)

#Now show for all data
hist(csData$ACTCMMID)

# Show the means of New york Schools versus US Population, remove na values = TRUE
mean(ny.data$ACTCMMID, na.rm = TRUE)
mean(csData$ACTCMMID, na.rm = TRUE)

#Structure is showing that it is parsing info as a Factor, We will fix that
str(csData$MD_EARN_WNE_P10)

#Convert the output of previous command, Convert the factors to characters THEN convert to numeric values
# Factors should not be directly converted to numeric values as it could give bad data?
csData$MD_EARN_WNE_P10 <- as.numeric(as.character(csData$MD_EARN_WNE_P10))
#Find the correlation of the median ACT score and the salary 10 years after graduation
cor(csData$ACTCMMID, csData$MD_EARN_WNE_P10, use = 'complete.obs')

saveRDS(csData, 'Scorecard Data Cleaned.rds')
#The institution URL is listed as a factor. This converts the column to characters
#Difference between this and sapply function? 
#QUESTION 1
csData$INSTURL <- as.character(csData$INSTURL)
str(csData$INSTURL)

#QUESTION 2
schools <- subset(csData, PBI == 1 | HBCU == 1)
aggregate(SAT_AVG~PBI+HBCU,schools,mean)
#Which schools produce lots of BCSE students (at least 80% of graduates come from these fields)?
#mean(schools[c('SAT_AVG")])
hist(schools$SAT_AVG)
hist(schools[schools$PBI == 1,"SAT_AVG"])

#Run a correlation analysis on variables that might be interesting to you.
#QUESTION 3
cor(csData$MD_EARN_WNE_P10, csData$LOCALE, use = "complete.obs" )
