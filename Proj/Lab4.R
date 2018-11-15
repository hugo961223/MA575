# September 23, 2018
# Lab 4, Ordinary Least Squares; Pollution example

# Read data from cvs file with ";" instead of ","
pollutiondata <- read.csv2("AirQualityData.csv",header=TRUE)

# Replace missing data i.e. -200 with NA
pollutiondata[pollutiondata==-200]<-NA

# Force Temperature and pollutant to be a numeric vector
Temperature = as.numeric(as.character(pollutiondata$Temperature))
RelativeHumidity = as.numeric(as.character(pollutiondata$RelativeHumidity))
AbsoluteHumidity = as.numeric(as.character(pollutiondata$AbsoluteHumidity))
SensorCO = as.numeric(as.character(pollutiondata$PT08.S1.CO.))
GroundCO = as.numeric(as.character(pollutiondata$CO.GT.))
GroundCO[GroundCO == -200]<-NA

# Create temporary data set with Temperature and Carbon Monoxide pollutant
tempdataset <- data.frame(SensorCO, GroundCO, Temperature, RelativeHumidity)
rm(Temperature,SensorCO,GroundCO,RelativeHumidity)

# Remove rows with NA
Dataset<-tempdataset[complete.cases(tempdataset), ]
attach(Dataset)

# Scatterplot
plot(SensorCO,GroundCO,ylab="Ground Truth (CO)", xlab="Sensor CO")

# Ordinary LS
m.ols <- lm(GroundCO~SensorCO)

#95% confidence intervals of OLS
summary(m.ols)
round(confint(m.ols,level=0.95),7)

# Quadratic LS
m.quadls <- lm(GroundCO~SensorCO + I(SensorCO^2))
summary(m.quadls)

# Scatter plot
plot(SensorCO,GroundCO,ylab="Ground Truth (CO)", xlab="Sensor CO")
abline(lsfit(SensorCO,GroundCO),col="blue")
SensorCONew<-seq(600,2200,len=100)
lines(SensorCONew,predict(m.quadls,newdata=data.frame(SensorCO=SensorCONew)),col="red")
legend(800, 11, legend=c("OLS", "Quad LS"), col=c("blue", "red"), lty=1, cex=1.5)

# Clean up
detach(Dataset)

# Exercise: Perform linear regression on your project data. Choose your response and covariate.