#Bike Sharing
#Load data in
library(summarytools)
library(dplyr)
library(plotly)
library(lubridate)
library(tidyr)
library(corrplot)
library(polycor)

bikedata <- read.csv("C:\\Users\\hugo1\\Documents\\ma575\\Proj\\day.csv",header=T)
head(bikedata)
view(dfSummary(bikedata))
hist(bikedata$casual)

# visulization graphs
# long to wide

p <- plot_ly(bikedata, x = ~dteday, y = ~casual, type = 'bar',text = ~casual, textposition = 'auto', name = 'casual') %>%
  add_trace(y = ~registered,text = w~registered, textposition = 'auto', name = 'registered') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'stack')

p

# the counts have grown for both registered and casual  

#split the data into training and validation 
head(bikedata)
dim(bikedata) 
#731 rows in total, 50% , 50% 
total_cnt_sum <- bikedata %>% 
  group_by(yr) %>% 
  summarize(total_cnt = sum(cnt))

bikedata <- bikedata %>% 
  left_join(total_cnt_sum) %>% 
  mutate(Perentage = cnt/total_cnt)
bikedata_train <- bikedata %>% filter(yr == 0)
bikedata_test <- bikedata %>% filter(yr == 1)

corr_data <- bikedata %>% select(-instant, -dteday)
c <- cor(corr_data)
round(c,3)
res1 <- cor.mtest(corr_data, conf.level = .95)
## corrplot 0.84 loaded
corrplot(round(c,3),p.mat = res1$p, sig.level = .05, type = "upper")

corr_data$workingday = as.factor(corr_data$workingday)
corr_data$holiday = as.factor(corr_data$holiday)
corr_data$mnth = as.factor(corr_data$mnth)
corr_data$season = as.factor(corr_data$season)
corr_data$yr = as.factor(corr_data$yr)
corr_data$weekday = as.factor(corr_data$weekday)


#Heterogenous correlation matrix
res2 <- hetcor(corr_data)

#adjust correlation matrix
corrMatrix <- res2$correlations
diag(corrMatrix) <- 0

corrplot(corrMatrix, method = "number", type = "upper", sig.level =0.01)

# From this correlation plot, we can see that there are four pairs 
# of features like $season and $mnth, $holiday and $workingday,
# $temp and $atemp, $hum and $weathersit are relatively correlated
# Therefore basically we will not put them into model simultaneously.
# And from this plot, we can see that features like $weekday, $workingday and $hum
# do not affect $cnt much. At last, we choose $season, $holiday, $weekday, $weathersit, $atemp
# and $windspeed these features as predictors.

# predict the number of casual bike rides based on season, temperature, humidity and windspeed
lm_bike <- lm(Perentage ~ season + holiday + weekday + weathersit + atemp + windspeed
              ,  data = bikedata_train)
summary(lm_bike)
# R^2 is 0.7492
# P value of t-test for features $season, $weathersit, $atemp, $windspeed
# are very small so these four features are significant from this summary output.
# Meanwhile, features like $holiday, $weekday are not significant in this model.
# And P value for F-test is also very small therefore this model is meaningful.


# Predict the values of the test set: pred
pred <- predict(lm_bike, se.fit = TRUE, bikedata_test)
head(pred)

ResMLSValidation <- bikedata_test$Perentage - pred$fit

ResMLS <- resid(lm_bike)
par(mfrow=c(1,1))
plot(bikedata_train$Perentage, ResMLS,xlab="count", ylab="Residuals", col="blue")

points(bikedata_test$Perentage, ResMLSValidation, xlab="count", ylab="Residuals",col="red")
# Residuals are pretty uniform in this plot, so we can deduce that this model can
# generalize very well.

mean((ResMLS)^2)   # Mean Square Error for training data

mean((ResMLSValidation)^2)  # Mean Square Error for validation data
# MSE for training data and validation data are also very close so it kind of proves
# the deduction above.

# Standarized Residuals
StanResMLS <- rstandard(lm_bike)
par(mfrow=c(1,1))
plot(bikedata_train$Perentage ,StanResMLS, xlab="Percentage", ylab="Standardized Residuals", col="blue") +abline(h=2,lty=2) + abline(h=-2,lty=2) 
# Since most residuals are between -2 and 2, and they do not have any patterns
# from this plot, we can deduce that this model is not bad.


q1 <- qqnorm(StanResMLS, plot.it = TRUE) # Test of Normality for 
qqline(StanResMLS,lty = 2)               # Standarized Residuals of QMLS and QuartLS
# The Q-Q plot almost lies on the diagonal line except the begining part, so 
# maybe there is some part of data that this model can not generalize that well,
# but basically this model can capture the main features of true model.

par(mfrow=c(1,1))
hist(StanResMLS,100) # Histogram of QMLS and QuartLS
# The shape of this histogram is similar to the shape of Normal Distribution,
# so we can assert that this model is fairly good.