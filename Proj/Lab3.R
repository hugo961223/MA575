# September 16, 2018
# Lab 3, Data Visualization
# Run step by step 

# Read data from cvs file with ";" instead of ","
pollutiondata <- read.csv2("AirQualityData.csv",header=TRUE)

# Load visualization package
library(carData)
library(car)
# Note: if not available on Rstudio install car from Tools -> Install packages


# The data uses the label -200 as missing data
# Replace -200 with NA
pollutiondata[pollutiondata==-200]<-NA

# The data is not numeric but it factor levels
# Force Temperature and pollutant to be a numeric vector
Temperature = as.numeric(as.character(pollutiondata$Temperature))
RelativeHumidity = as.numeric(as.character(pollutiondata$RelativeHumidity))
AbsoluteHumidity = as.numeric(as.character(pollutiondata$AbsoluteHumidity))
SensorCO = as.numeric(as.character(pollutiondata$PT08.S1.CO.))
GroundCO = as.numeric(as.character(pollutiondata$CO.GT.))

# Force again -200 as NA
GroundCO[GroundCO == -200]<-NA

# Plot scatterplot of GroundCO vs SensorCO
scatterplot(GroundCO,SensorCO)

# Scatterplot with plot command also
plot(SensorCO,GroundCO,ylab="Ground Truth (CO)", xlab="Sensor CO")

# Scatterplot matrix all the variables
scatterplotMatrix(~GroundCO + SensorCO + Temperature + AbsoluteHumidity + RelativeHumidity)



