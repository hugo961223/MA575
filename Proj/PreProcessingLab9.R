# October 31, 2018
# Lab 9
# Multiple Linear Regression (Training, Validation, Testing)
# Pre-processing file

# Read data from cvs file with ";" instead of ","
pollutiondata <- read.csv2("AirQualityData.csv",header=TRUE)

# Replace missing data i.e. -200 with NA
pollutiondata[pollutiondata==-200]<-NA

# Force Temperature and pollutant to be a numeric vector
Temperature = as.numeric(as.character(pollutiondata$Temperature))
Time = as.numeric(pollutiondata$Time)
RelativeHumidity = as.numeric(as.character(pollutiondata$RelativeHumidity))
AbsoluteHumidity = as.numeric(as.character(pollutiondata$AbsoluteHumidity))
SensorCO = as.numeric(as.character(pollutiondata$PT08.S1.CO.))
GroundCO = as.numeric(as.character(pollutiondata$CO.GT.))
GroundCO[GroundCO == -200]<-NA

# Load Date also
#Date = as.Date(pollutiondata$Date, "%m%d%y")
Date = as.Date(pollutiondata$Date, "%d/%m/%Y");

# Create temporary data set with Temperature and Titania pollutant
tempdataset <- data.frame(SensorCO, GroundCO, Temperature, RelativeHumidity, AbsoluteHumidity, Time, Date)
rm(Temperature,SensorCO,GroundCO,RelativeHumidity, AbsoluteHumidity, Time, Date)

# Data with NA
Dataset<-tempdataset[complete.cases(tempdataset), ]
