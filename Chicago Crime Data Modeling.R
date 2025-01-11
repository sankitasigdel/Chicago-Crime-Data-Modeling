#loading data given by professor
load("crimefive.RData")

# crime.five data is loaded
#viewing top values in crime.five data
head(crime.five)
#lower end of the dataset
tail(crime.five)

#type of variable
typeof(crime.five)

#checking how many columns and rows are there in the dataset
ncol(crime.five)

nrow(crime.five)

######  •	DATA PREPARATION AND PROCESSING

#**** •	Correcting Data Format issues
# Problem 1: 
#*Some values in the columns were just " "
#*For instance for column Location.Description

#checking unqiue values in a column Location.Description
unique(crime.five$Location.Description)

#Required Library for filter function
library(dplyr)
#Filter to show only rows with Location.Description = ""
filtered.crime.data <- crime.five %>%
  filter(
    Location.Description == ""
  )


#checking the distict data for the new filtered dataset with "" on Location.Description
sapply(filtered.crime.data,function(x) n_distinct(x))

#count number of nulls in each columns for the new filtered dataset with "" on Location.Description
null.counts.in.original.dataset2 <- colSums(is.na(filtered.crime.data))
null.counts.in.original.dataset2

#Filtering final dataset without Location.Description ""
final.crime.data <- crime.five %>%
  filter(
    Location.Description != ""
  )


#**  •	Start with Summary of the data
#summary of the data
summary(final.crime.data)

#*****  •	Handling Missing Values
#*Problem 1:

#count number of nulls in each columns in filtered data
null.counts.in.filtered <- colSums(is.na(final.crime.data))
null.counts.in.filtered

#Cleaning dataset without null values 
final.crime.five <- na.omit(final.crime.data)

#Checking final dataset and it's null values....null values are removed
null.counts.in.original.dataset.final.crime.five <- colSums(is.na(final.crime.five))
null.counts.in.original.dataset.final.crime.five

# Solution: Remove the values from missing columns


# ****  •	Removing Duplicate Values (no duplicate values)

#count number of duplicates in each row (No duplicates)
sum(duplicated(final.crime.five))

#checking datatype for duplicates question
str(final.crime.five)

library(dplyr)
#no of distict values in the dataset
#referred website : https://www.r-bloggers.com/2022/06/how-to-count-distinct-values-in-r/
sapply(final.crime.five,function(x) n_distinct(x))

#unique value in each column to check if there is any formmating issues in the dataset
table(final.crime.five$Location.Description)


#**** •	Handling noisy data location, negative or zero values in (IUCR, ) with negative values 
#Check for negative or zero values in columns
final.crime.five[final.crime.five$IUCR <= 0, ]
final.crime.five[final.crime.five$Beat <= 0, ]
final.crime.five[final.crime.five$District <= 0, ]
final.crime.five[final.crime.five$Ward <= 0, ]
final.crime.five[final.crime.five$Community.Area <= 0, ]
final.crime.five[final.crime.five$FBI.Code <= 0, ]
final.crime.five[final.crime.five$Year <= 0, ]


#** •	Exploratory Data Analysis


# •	Descriptive Statistics
#summary of the data
summary(final.crime.five)


# •	Data visualization (Scatter plot, Correlation, Histogram)

#Checking datatype of each variables 
str(final.crime.five)

#create a correlation matrix of each numeric variables (except ID column)
corr.mat <- cor(final.crime.five[,-1] %>% select_if(is.numeric), use="complete.obs", method = "pearson")

#--From the correlation matrix we can see that Beat and Distrrict are highly correlated to each other 

library(ggplot2) #for plotting to see relationship of Beat and District (using scattered plot to show the data)
#creating a scattered chart---> have attached a picture with this R code/ Takes some time to run
ggplot(final.crime.five, aes(x = Beat, y = District)) + geom_point(color = "blue", size = 1)+ labs(
    title = "Relationship Between Beat and District",
    x = "Beat",
    y = "District"
  ) #if this code shows any error it is because the space in the plot area.. 
#adjust the plot area in output pane if this shows any error


#***** Analyzing crimes in each year

typeof(final.crime.five$Year)
unique(final.crime.five$Year)
#plotting yearly statistics in histogram 
hist(final.crime.five$Year, breaks = 20, 
     xlab = "Year", #naming X axis title
     ylab = "Number of occurance(Frequency)", #naming Y axis title
     main = "Number of Crimes",
     col= "blue") 


#Aggregating values to see how many crimes occur in each year
agg.crime.peryear <- aggregate(final.crime.five$ID,
                            list(final.crime.five$Year),
                            FUN = length)
#Renaming column headers in agg.crime.peryear
colnames(agg.crime.peryear) <- c("Year", "Count of Crimes")

#Line Graph for Crime over the years
#Referred: https://r-graph-gallery.com/line-chart-ggplot2.html 
ggplot(agg.crime.peryear, aes(x = Year, y = `Count of Crimes`)) +
  geom_line() + # Drawing the line
  geom_point() + # Adding points to the line
  labs(title = "Crimes Over the Years", x = "Year", y = "Number of Crimes") 

#Aggregating values to see crimes by each year
agg.crime.byyear <- aggregate(final.crime.five$ID,
                            list(final.crime.five$Year,final.crime.five$Primary.Type),
                            FUN = length)

#Renaming column headers in agg.crime.byyear
colnames(agg.crime.byyear) <- c("Year","Crime Type" ,"Count of Crimes")

#bar plot for Crime over the years by crime type ---> this does not look good.
library(ggplot2)
ggplot(agg.crime.byyear, aes(x = Year, `Count of Crimes`, fill = `Crime Type`)) +
  geom_bar(stat = "identity", position = "dodge") +  # Bar plot with bars for each crime type
  labs(title = "Crimes Over the Years", x = "Year", y = "Number of Crimes")


#Aggregating values to see how many crimes occur by types
agg.crime.count <- aggregate(final.crime.five$ID,
                               list(final.crime.five$Primary.Type),
                               FUN = length)
#Renaming column headers in agg.crime.peryear
colnames(agg.crime.count) <- c("Crime Type", "Count of Crimes")

#Duplicating Date column for split
#Referred: https://stackoverflow.com/questions/22030252/duplicate-a-column-in-data-frame-and-rename-it-to-another-column-name 
final.crime.five <- final.crime.five %>% 
  mutate(Date2 = Date,
         Date3 = Date)



library(tidyr) #For using separate function -splitting the date, time and AM/PM
#Referred: https://stackoverflow.com/questions/4350440/split-data-frame-string-column-into-multiple-columns 
#Splitting the column in R
#Splitting Date column "01/27/2020 09:10:00 AM" to 01/27/2020, 09:10:00, AM with "" separator
final.crime.five <- final.crime.five %>% separate(Date3, c('Date_', 'Time',"AM/PM"), " ")

# conversion of date variable into  
# POSIXct format  
#Referred: https://www.geeksforgeeks.org/convert-dataframe-column-to-datetime-in-r/ 
final.crime.five$Time <- as.POSIXct(final.crime.five$Date2, format = "%m/%d/%Y %I:%M:%S %p")

#Aggregating data to Morning, Afternoon, Evening and Night
"Morning(4:00AM-12:00PM), 
Afternoon(12:00PM – 5:00PM),
Evening(5:00PM to 9:00PM), 
Night (9:00PM to 3:59AM)"


#condition check and add time
final.crime.five$Timeofday <- ifelse(
  format(final.crime.five$Time, "%H:%M") >= "04:00" & format(final.crime.five$Time, "%H:%M") < "12:00"  & final.crime.five$`AM/PM` == "AM", "Morning",
  ifelse(
    format(final.crime.five$Time, "%H:%M") >= "12:00" & format(final.crime.five$Time, "%H:%M") < "17:00" & final.crime.five$`AM/PM` == "PM", "Afternoon",
    ifelse(
      format(final.crime.five$Time, "%H:%M") >= "17:00" & format(final.crime.five$Time, "%H:%M") < "21:00" & final.crime.five$`AM/PM` == "PM", "Evening",
      "Night"
    )
  )
)


#condition check and add time
final.crime.five$Date_ <- as.Date(final.crime.five$Date_, format = "%m/%d/%Y")
#condition for week
final.crime.five$Weekday <- weekdays(final.crime.five$Date_)

#Aggregate
agg.crime.byweekday <- aggregate(final.crime.five$ID,
                               list(final.crime.five$Weekday),
                               FUN = length)

#Renaming column headers
colnames(agg.crime.byweekday) <- c("Weekday", "CountofCrimes")


#plotting the bar graph for crimes by Weekday
barplot(agg.crime.byweekday$CountofCrimes,
        xlab = "Weekday",
        ylab = "Count of Crimes" ,
        main = "Crimes by Weekday",
        names.arg = agg.crime.byweekday$Weekday,
        col = "red")


#Splitting Date column "01/27/2020" to 01, 27, 2020 with "/" separator
final.crime.five <- final.crime.five %>% separate(Date_, c('Year', 'Month',"Day"), "-")
#Splitting Date column "09:10:00 AM" to  09, 10, 00 with ":" separator
#final.crime.five <- final.crime.five %>% separate(Time, c('Hour', 'Minute',"Second"), ":")


#aggregating to monthly data in R


#Referred : https://www.r-bloggers.com/2024/01/conquering-daily-data-how-to-aggregate-to-months-and-years-like-a-pro-in-r/
agg.crime.bymonth <- aggregate(final.crime.five$ID,
                               list(final.crime.five$Month),
                               FUN = length)


#Renaming column headers
colnames(agg.crime.bymonth) <- c("Month", "CountofCrimes")

#Naming months to Jan, Feb
agg.crime.bymonth <- agg.crime.bymonth %>% 
  mutate(
    MonthName = case_when(
      agg.crime.bymonth$Month == "01" ~ "January",
      agg.crime.bymonth$Month == "02" ~ "Feburary",
      agg.crime.bymonth$Month == "03" ~ "March",
      agg.crime.bymonth$Month == "04" ~ "April",
      agg.crime.bymonth$Month == "05" ~ "May",
      agg.crime.bymonth$Month == "06" ~ "June",
      agg.crime.bymonth$Month == "07" ~ "July",
      agg.crime.bymonth$Month == "08" ~ "August",
      agg.crime.bymonth$Month == "09" ~ "September",
      agg.crime.bymonth$Month == "10" ~ "October",
      agg.crime.bymonth$Month == "11" ~ "November",
      agg.crime.bymonth$Month == "12" ~ "December"
    )
  )


#plotting the bar graph for crimes by month
barplot(agg.crime.bymonth$CountofCrimes,
        xlab = "Month",
        ylab = "Count of Crimes" ,
        main = "Crimes by Month",
        names.arg = agg.crime.bymonth$MonthName,
        col = "red")

  

#Aggregating data to Morning, Afternoon, Evening and Night
"Morning(4:00AM-12:00PM), 
Afternoon(12:00PM – 5:00PM),
Evening(5:00PM to 9:00PM), 
Night (9:00PM to 3:59AM)"

#checking for any unique value for column Timeoftheday
unique(final.crime.five$Timeofday)


#aggregating by time of the day
agg.crime.bytime <- aggregate(final.crime.five$ID,
                               list(final.crime.five$Timeofday),
                               FUN = length)

#Renaming column headers
colnames(agg.crime.bytime) <- c("Timeoftheday", "CountofCrimes")

#plotting the bar graph for crimes by time of the day
barplot(agg.crime.bytime$CountofCrimes,
        xlab = "Time of the day",
        ylab = "Count of Crimes" ,
        main = "Crimes by time of the day",
        names.arg = agg.crime.bytime$Timeoftheday,
        col = "red")

# •	Data aggregation

# DATA AGGREGATION OF Primary.Type Column to more general crime type
#Value based on Primary.Type 
#https://www.geeksforgeeks.org/case-when-statement-in-r-dplyr-package-using-case_when-function/
"1. Violent Crime: Assault, Battery, Homicide, Kidnapping, Intimidation,Stalking, 
                  Criminal Sexual Assault, Crim Sexual Assault, Offense Involving Children,
                  Sex Offense, Public Indecency, Weapon Violation, Robbery
2. Property Crime: Theft, Motor Vehicle Theft,Burglary,Arson,Criminal Damage,Criminal Trespass
3. Fraud: Deceptive Practice,Interference with Public Officer,Obscenity, Liquor Law Violation, 
          Gambli ng, Concealed Carry License Violation
4. Drugs Related: Narcotics, Other Narcotic Violation
5. Public Offense: Public Peace Violation, Prostitution, Human Trafficking, Ritualism,
                  Non-Criminal, Non-Criminal (Subject Specified)
6. Unclassified Crime: Other Offense"   

#adding a new column with aggregated data
final.crime.five <- final.crime.five %>% 
                  mutate(
                    Aggregated.Crime = case_when(
                      Primary.Type %in% c("ASSAULT", "BATTERY", "HOMICIDE" ,"KIDNAPPING" ,"INTIMIDATION" ,
                        "STALKING" , "CRIMINAL SEXUAL ASSAULT" , "ROBBERY", "CRIM SEXUAL ASSAULT" , "OFFENSE INVOLVING CHILDREN" ,
                        "SEX OFFENSE" , "PUBLIC INDECENCY", "WEAPONS VIOLATION") ~ "Violent Crime",
                      Primary.Type %in% c( "THEFT" , "MOTOR VEHICLE THEFT" , "BURGLARY" , "ARSON", "CRIMINAL DAMAGE" , 
                                           "CRIMINAL TRESPASS") ~ "Property Crime",
                      Primary.Type %in% c("DECEPTIVE PRACTICE" , "INTERFERENCE WITH PUBLIC OFFICER" , "OBSCENITY" , "LIQUOR LAW VIOLATION",
                        "GAMBLING" , "CONCEALED CARRY LICENSE VIOLATION") ~ "Fraud",
                      Primary.Type %in% c("NARCOTICS" , "OTHER NARCOTIC VIOLATION") ~ "Drug Crime",
                      Primary.Type %in% c("PUBLIC PEACE VIOLATION" , "PROSTITUTION" , "HUMAN TRAFFICKING" , "RITUALISM" , "NON-CRIMINAL" ,
                        "NON-CRIMINAL (SUBJECT SPECIFIED)") ~ "Public Offence",
                      Primary.Type %in% "OTHER OFFENSE" ~ "Unclassified Crime",
                    )
                  )

 
#checking unqiue values in a column
unique(final.crime.five$Aggregated.Crime)

#aggregating by count by aggregated crime type
agg.crime.byaggcrime <- aggregate(final.crime.five$ID,
                              list(final.crime.five$Aggregated.Crime),
                              FUN = length)

#Renaming column headers
colnames(agg.crime.byaggcrime) <- c("CrimeAgg", "CountofCrimes")

#plotting the bar graph for Aggregated crime
barplot(agg.crime.byaggcrime$CountofCrimes,
        xlab = "Aggregated crime type",
        ylab = "Count of Crimes" ,
        main = "Aggregated crime",
        names.arg = agg.crime.byaggcrime$CrimeAgg,
        col = "red")


#Aggregating values to see crimes by each year for aggregated crime column
agg.crime.byyearagg <- aggregate(final.crime.five$ID,
                                 list(final.crime.five$Year,final.crime.five$Aggregated.Crime),
                                 FUN = length)

#Renaming column headers in agg.crime.byyear
colnames(agg.crime.byyearagg) <- c("Year","Crime Type" ,"Count of Crimes")

#bar plot for Crime over the years by crime type ---> this does not look good.
library(ggplot2)
ggplot(agg.crime.byyearagg, aes(x = Year, y = `Count of Crimes`, fill = `Crime Type`)) +
  geom_bar(stat = "identity", position = "dodge") +  # Bar plot with bars for each crime type
  labs(title = "Crimes Over the Years", x = "Year", y = "Number of Crimes")


#adding a column if the value is sunday and saturday (weekend) and if Mon-Friday (weekday)
final.crime.five.weekend <- final.crime.five %>% 
  mutate(
    isWeekend = case_when(
      Weekday %in% c("Monday","Tuesday","Wednesday","Thursday","Friday") ~ "No",
      Weekday %in% c( "Saturday" , "Sunday") ~ "Yes",
    )
  )

#adding a column if the value it is weekend or not
final.crime.five <- final.crime.five %>% 
  mutate(
    isWeekend = case_when(
      Weekday %in% c("Monday","Tuesday","Wednesday","Thursday","Friday") ~ "Yes",
      Weekday %in% c( "Saturday" , "Sunday") ~ "No",
    )
  )

#adding a column if the value it is violent or not
final.crime.five <- final.crime.five %>% 
                          mutate(
                            isviolent = case_when(
                              Primary.Type %in% c("BATTERY", "ASSAULT", "WEAPONS VIOLATION", "ROBBERY", "CRIM SEXUAL ASSAULT", 
                                                  "CRIMINAL SEXUAL ASSAULT", "OFFENSE INVOLVING CHILDREN", "SEX OFFENSE", "STALKING", 
                                                  "KIDNAPPING", "INTIMIDATION", "HOMICIDE", "HUMAN TRAFFICKING", "PUBLIC INDECENCY") ~ "Yes",
                              Primary.Type %in% c("THEFT","DECEPTIVE PRACTICE","MOTOR VEHICLE THEFT", "CRIMINAL DAMAGE","OTHER OFFENSE",
                                                  "NARCOTICS","CRIMINAL TRESPASS","BURGLARY","INTERFERENCE WITH PUBLIC OFFICER","ARSON",
                                                  "PROSTITUTION","PUBLIC PEACE VIOLATION","LIQUOR LAW VIOLATION","CONCEALED CARRY LICENSE VIOLATION",
                                                  "GAMBLING","OBSCENITY","NON-CRIMINAL","NON-CRIMINAL (SUBJECT SPECIFIED)","OTHER NARCOTIC VIOLATION",
                                                  "RITUALISM") ~ "No",
                            )
                          )


#Aggregating values to see number of violent and non violent crime
agg.crime.byviolent<- aggregate(final.crime.five$ID,
                                list(final.crime.five$isviolent),
                                FUN = length)

#Renaming column headers in agg.crime.byyear
colnames(agg.crime.byviolent) <- c("IsViolent","Number of Crimes")

#plotting the bar graph for Aggregated isviolent crime
barplot(agg.crime.byviolent$`Number of Crimes`,
        xlab = "IsViolent Crime?",
        ylab = "Count of Crimes" ,
        main = "IsViolent?",
        names.arg = agg.crime.byviolent$IsViolent,
        col = "red")


#Aggregating values to see number of violent and non violent crime
agg.crime.byweekend<- aggregate(final.crime.five$ID,
                                list(final.crime.five$isWeekend),
                                FUN = length)

#Renaming column headers in agg.crime.byyear
colnames(agg.crime.byweekend) <- c("IsWeekend","Number of Crimes")

#plotting the bar graph for Aggregated IsWeekend crime
barplot(agg.crime.byweekend$`Number of Crimes`,
        xlab = "IsWeekend Crime?",
        ylab = "Count of Crimes" ,
        main = "IsWeekend?",
        names.arg = agg.crime.byweekend$IsWeekend,
        col = "red")

#checking unique values in Timeofday--found 14 rows with NA so removed those
unique(final.crime.five$Timeofday)

unique(final.crime.five$Primary.Type)

#removing NA values from final data
final.crime.five <- na.omit(final.crime.five)

#counting if there are any null values in the columns..After this there is no null values in this column
null.countfinal<- colSums(is.na(final.crime.five))
null.countfinal


#checking unqiue values in District, ward and community area 
# we will choose the variable which has least number of unique values for analysis 

length(unique(final.crime.five$District)) # 23 unique values
length(unique(final.crime.five$Ward)) # 50 unique values
length(unique(final.crime.five$Community.Area)) #77 unique values

#***We have decided to use district for one of the variables to predict if the crime is violent or not

unique(final.crime.five$isviolent)
#we will use isViolent as target variable
#converting the isviolent variable to factor 
final.crime.five$isviolent <- as.factor(ifelse(final.crime.five$isviolent == "Yes", 1, 0))
#-------------------------------------------------------------------------------------------------------------------------------------------

#Data Modeling
#our final clean data is final.crime.five

set.seed(123) #for splitting same data

#load libraries for modeling
library(caTools)
library(caret)

split.data <- sample.split(final.crime.five, SplitRatio = 0.5) #50% of the values will be for training and rest Testing

train.data <- subset(final.crime.five, split.data == TRUE) #splitting data in train.data object

test.data <- subset(final.crime.five, split.data == FALSE) #splitting data in test.data object


#run logistic regression model

#MODEL 1

# using variables - isWeekend, Timeofday, Latitude, Longitude, District
model.logistic1 <- glm(isviolent ~ isWeekend + Timeofday + Latitude + Longitude + District, 
                       family = binomial(link = "logit"), 
                       data = train.data)

print(summary(model.logistic1), show.residuals = T) #summary with residuals

#Log of odds ratio in this line,...now need to have thresold to give 0 and 1 values
predicted.results.model1 <- predict(model.logistic1,
                             newdata = test.data, type = "response")


#We would use prediction function using this library
library(ROCR)

new.format.model1 <- prediction(predicted.results.model1, test.data$isviolent)


#predicted and original side by side
new.data.model1 <- data.frame(predicted.results.thresold = predicted.results.model1,
                       original = test.data$isviolent)

#threshold in the values
new.data.model1 <- new.data.model1 %>% 
  mutate(predicted.result.final =
           ifelse(new.data.model1$predicted.results.thresold> 0.5, 1,0))

#confusion matrix for model 1
confusion.matrix.model1 <- confusionMatrix(as.factor(new.data.model1$predicted.result.final),as.factor(new.data.model1$original))
confusion.matrix.model1

#ROC calculation for model
performance.model1 <- performance(new.format.model1, 
                                 measure = "tpr", x.measure = "fpr")
performance.model1

#ROC curve plotting
plot(performance.model1,
     main = "Logstic Regression Model 1 - 
     isWeekend, Timeofday, Latitude, Longitude, District")

#AUC for model 1
area.under.curve1 <- performance(new.format.model1, measure = "auc")
area.under.curve1 <- area.under.curve1@y.values[1]
print(paste("Logistic Regression Model 1 AUC= ", area.under.curve1))

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#MODEL 2

# using variables - isWeekend, Latitude, Longitude, District
model.logistic2 <- glm(isviolent ~ isWeekend + Latitude + Longitude + District, 
                       family = binomial(link = "logit"), 
                       data = train.data)

print(summary(model.logistic2), show.residuals = T) #summary with residuals

#Log of odds ratio in this line,...now need to have thresold to give 0 and 1 values
predicted.results.model2 <- predict(model.logistic2,
                                    newdata = test.data, type = "response")


#We would use prediction function using this library
library(ROCR)

new.format.model2 <- prediction(predicted.results.model2, test.data$isviolent)


#predicted and original side by side
new.data.model2 <- data.frame(predicted.results.thresold = predicted.results.model2,
                              original = test.data$isviolent)

#threshold in the values
new.data.model2 <- new.data.model2 %>% 
  mutate(predicted.result.final =
           ifelse(new.data.model2$predicted.results.thresold> 0.5, 1,0))

#confusion matrix for model 2
confusion.matrix.model2 <- confusionMatrix(as.factor(new.data.model2$predicted.result.final),as.factor(new.data.model2$original))
confusion.matrix.model2

#ROC calculation for model 2
performance.model2 <- performance(new.format.model2, 
                                  measure = "tpr", x.measure = "fpr")
performance.model2

#ROC curve plotting
plot(performance.model2,
     main = "Logstic Regression Model 2 - 
     isWeekend, Latitude, Longitude, District")

#AUC for model 2
area.under.curve2 <- performance(new.format.model2, measure = "auc")
area.under.curve2 <- area.under.curve2@y.values[1]
print(paste("Logistic Regression Model 2 AUC= ",area.under.curve2))
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#MODEL 3

# using variables - Timeofday, Latitude, Longitude, District
model.logistic3 <- glm(isviolent ~ Timeofday + Latitude + Longitude + District, 
                       family = binomial(link = "logit"), 
                       data = train.data)

print(summary(model.logistic3), show.residuals = T) #summary with residuals

#Log of odds ratio in this line,...now need to have thresold to give 0 and 1 values
predicted.results.model3 <- predict(model.logistic3,
                                    newdata = test.data, type = "response")


#We would use prediction function using this library
library(ROCR)

new.format.model3 <- prediction(predicted.results.model3, test.data$isviolent)


#predicted and original side by side
new.data.model3 <- data.frame(predicted.results.thresold = predicted.results.model3,
                              original = test.data$isviolent)

#threshold in the values
new.data.model3 <- new.data.model3 %>% 
  mutate(predicted.result.final =
           ifelse(new.data.model3$predicted.results.thresold> 0.5, 1,0))

#confusion matrix for model 3
confusion.matrix.model3 <- confusionMatrix(as.factor(new.data.model3$predicted.result.final),as.factor(new.data.model3$original))
confusion.matrix.model3

#ROC calculation for model 3
performance.model3 <- performance(new.format.model3, 
                                  measure = "tpr", x.measure = "fpr")
performance.model3

#ROC curve plotting
plot(performance.model3,
     main = "Logistic Regression Model 3 - 
     Timeofday, Latitude, Longitude, District")

#AUC for model 3
area.under.curve3 <- performance(new.format.model3, measure = "auc")
area.under.curve3 <- area.under.curve3@y.values[1]
print(paste("Logistic Regression Model 3 AUC= ",area.under.curve3))
#----------------------------------------------------------------------------------------------------------------------------------------------------

#DECISION TREE

#load libraries for decision tree
library(rpart)
library(rpart.plot)
library(ROCR)

# Train decision tree Model 1
model.tree.1 <- rpart(isviolent ~ isWeekend + Timeofday + Latitude + Longitude + District, 
                      data = train.data, 
                      method = "class")

# Get probabilities for each class for Model 1
predicted.prob.tree1 <- predict(model.tree.1, newdata = test.data, type = "prob")

# Extract probabilities for the positive class for Model 1
positive.prob.tree1 <- predicted.prob.tree1[, 2]

# Predicted class based on threshold > 0.5 = 1 for Model 1
predicted.class.tree1 <- ifelse(positive.prob.tree1 > 0.5, 1, 0)

# Ensure levels of predicted and actual match 
#making everything as a factor
new.data.modeltree1 <- data.frame(
              predicted.class = factor(predicted.class.tree1, levels = levels(test.data$isviolent)),
              original = factor(test.data$isviolent, levels = levels(test.data$isviolent)),
              predicted.prob = positive.prob.tree1
            )

# Confusion matrix for Model 1
confusion.matrix.1 <- confusionMatrix(as.factor(new.data.modeltree1$predicted.class),
                                      as.factor(test.data$isviolent))
print(confusion.matrix.1)

# Prepare data for ROC calculation for Model 1
new.format.model.1 <- prediction(positive.prob.tree1, test.data$isviolent)

# ROC curve performance for Model 1
performance.model.1 <- performance(new.format.model.1, measure = "tpr", x.measure = "fpr")

# Plot ROC curve for Model 1
plot(performance.model.1,
     main = "Decision Tree Model 1 - 
     isWeekend, Timeofday, Latitude, Longitude, District")

# Calculate AUC for Model 1
area.under.curve.1 <- performance(new.format.model.1, measure = "auc")
auc_value <- area.under.curve.1@y.values[[1]]
print(paste("Decision Tree Model 1 AUC= ",auc_value))

#----------------------------------------------------------------------------------------------------------------------------------------------------

#MODEL 2 (Decision Tree)

# using variables - isWeekend, Latitude, Longitude, District
# Train decision tree Model 2
model.tree.2 <- rpart(isviolent ~ isWeekend + Latitude + Longitude + District, 
                      data = train.data, 
                      method = "class")


# Get probabilities for each class for Model 2
predicted.prob.tree2 <- predict(model.tree.2, newdata = test.data, type = "prob")

# Extract probabilities for the positive class 
positive.prob.tree2 <- predicted.prob.tree2[, 2]

# Predicted class based on threshold > 0.5 = 1 for Model 2
predicted.class.tree2 <- ifelse(positive.prob.tree2 > 0.5, 1, 0)

# Ensure levels of predicted and actual match 
#making everything as a factor for Model 2
new.data.modeltree2 <- data.frame(
  predicted.class = factor(predicted.class.tree2, levels = levels(test.data$isviolent)),
  original = factor(test.data$isviolent, levels = levels(test.data$isviolent)),
  predicted.prob = positive.prob.tree2
)

# Confusion matrix for Model 2
confusion.matrix.2 <- confusionMatrix(as.factor(new.data.modeltree2$predicted.class),
                                      as.factor(test.data$isviolent))
print(confusion.matrix.2)

# Prepare data for ROC calculation for Model 2
new.format.model.2 <- prediction(positive.prob.tree2, test.data$isviolent)

# ROC curve performance for Model 2
performance.model.2 <- performance(new.format.model.2, measure = "tpr", x.measure = "fpr")

# Plot ROC curve for Model 2
plot(performance.model.2,
     main = "Decision Tree Model 2 - 
     isWeekend, Latitude, Longitude, District")

# Calculate AUC for Model 2
area.under.curve.2 <- performance(new.format.model.2, measure = "auc")
auc_value.2 <- area.under.curve.2@y.values[[1]]
print(paste("Decision Tree Model 2 AUC= ", auc_value.2))

#----------------------------------------------------------------------------------------------------------------------------------------------------
#MODEL 3

# using variables - Timeofday, Latitude, Longitude, District
# Train decision tree Model 3
model.tree.3 <- rpart(isviolent ~ Timeofday + Latitude + Longitude + District, 
                      data = train.data, 
                      method = "class")


# Get probabilities for each class for Model 3
predicted.prob.tree3 <- predict(model.tree.3, newdata = test.data, type = "prob")

# Extract probabilities for the positive class for Model 3
positive.prob.tree3 <- predicted.prob.tree3[, 2]

# Predicted class based on threshold  > 0.5 = 1 for Model 3
predicted.class.tree3 <- ifelse(positive.prob.tree3 > 0.5, 1, 0)

# Ensure levels of predicted and actual match 
#making everything as a factor for Model 3
new.data.modeltree3 <- data.frame(
  predicted.class = factor(predicted.class.tree3, levels = levels(test.data$isviolent)),
  original = factor(test.data$isviolent, levels = levels(test.data$isviolent)),
  predicted.prob = positive.prob.tree3
)

# Confusion matrix for Model 3
confusion.matrix.3 <- confusionMatrix(as.factor(new.data.modeltree3$predicted.class),
                                      as.factor(test.data$isviolent))
print(confusion.matrix.3)

# Prepare data for ROC calculation for Model 3
new.format.model.3 <- prediction(positive.prob.tree3, test.data$isviolent)

# ROC curve performance for Model 3
performance.model.3 <- performance(new.format.model.3, measure = "tpr", x.measure = "fpr")

# Plot ROC curve for Model 3
plot(performance.model.3,
     main = "Decision Tree Model 3 - 
     Timeofday, Latitude, Longitude, District")

# Calculate AUC for Model 3
area.under.curve.3 <- performance(new.format.model.3, measure = "auc")
auc_value.3 <- area.under.curve.3@y.values[[1]]
print(paste("Decision Tree Model 3 AUC= ", auc_value.3))