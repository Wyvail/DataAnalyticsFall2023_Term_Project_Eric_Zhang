#Read in state level expected annual losses due to risk factors
eal_data <- read.csv("NRI_Table_States.csv", header=TRUE)
summary(eal_data)
eal_data <- eal_data[1:51,]

#sort on score and take risks that are likely to affect solar/wind
eal_sorted <- eal_data[order(eal_data$EAL_SCORE),]
risks <- eal_sorted[,c("STATE", "STATEABBRV", "AREA", "EAL_SCORE", "EAL_RATNG", "HAIL_EALS","HRCN_EALS","LTNG_EALS","SWND_EALS","TRND_EALS","ERQK_EALS")]
summary(risks)
#filter out to only continental
risks <- risks[-c(2, 10, 13),]

#fill in na with 0
risks[is.na(risks)] <- 0

summary(risks)

#filter out small states 
risksfr <- risks[-which(risks$AREA < 25000), ]

#choose top ten
risksfr <- risksfr[c(1:10),]

#Numerical data from EAL
numerical <- risksfr[,c("AREA", "EAL_SCORE", "HAIL_EALS","HRCN_EALS","LTNG_EALS","SWND_EALS","TRND_EALS","ERQK_EALS")]

hail <- numerical[order(numerical$HAIL_EALS),]

hail2 <- subset(hail, select=-c(AREA))

boxplot(hail2)
#data(iris)

pairs(numerical)

#d2 <- read.csv("3539964.csv")
#d2 <- d2[which(d2$AWND != 0),]

#State level climate data
wy <- read.csv("wyoming.csv", header = TRUE)
summary(wy)
wy1 <- wy[1:112,] #Wind speed at 50 meters
wy2 <- wy[113:224,] #Wind speed at 50 meters range
wy3 <- wy[225:336,] #All sky downward irradiance
wy4 <- wy[337:448,] #Clear sky downward irradiance

#Calculating averages for each value
wy1m = mean(wy1$ANN)
wy2m = mean(wy2$ANN)
wy3m = mean(wy3$ANN)
wy4m = mean(wy4$ANN)

mt <- read.csv("montana.csv", header = TRUE)

mt1 <- mt[1:(nrow(mt)/4),] #Wind speed at 50 meters
mt2 <- mt[((nrow(mt)/4+1)):(2*(nrow(mt)/4)),] #Wind speed at 50 meters range
mt3 <- mt[(2*(nrow(mt)/4)+1):(3*(nrow(mt)/4)),] #All sky downward irradiance
mt4 <- mt[(3*(nrow(mt)/4)+1):nrow(mt),] #Clear sky downward irradiance

mt1m = mean(mt1$ANN)
mt2m = mean(mt2$ANN)
mt3m = mean(mt3$ANN)
mt4m = mean(mt4$ANN)

id <- read.csv("idaho.csv", header = TRUE)

id1 <- id[1:(nrow(id)/4),] #Wind speed at 50 meters
id2 <- id[((nrow(id)/4+1)):(2*(nrow(id)/4)),] #Wind speed at 50 meters range
id3 <- id[(2*(nrow(id)/4)+1):(3*(nrow(id)/4)),] #All sky downward irradiance
id4 <- id[(3*(nrow(id)/4)+1):nrow(id),] #Clear sky downward irradiance

id1m = mean(id1$ANN)
id2m = mean(id2$ANN)
id3m = mean(id3$ANN)
id4m = mean(id4$ANN)

nm <- read.csv("new_mexico.csv", header = TRUE)

nm1 <- nm[1:(nrow(nm)/4),] #Wind speed at 50 meters
nm2 <- nm[((nrow(nm)/4+1)):(2*(nrow(nm)/4)),] #Wind speed at 50 meters range
nm3 <- nm[(2*(nrow(nm)/4)+1):(3*(nrow(nm)/4)),] #All sky downward irradiance
nm4 <- nm[(3*(nrow(nm)/4)+1):nrow(nm),] #Clear sky downward irradiance

nm1m = mean(nm1$ANN)
nm2m = mean(nm2$ANN)
nm3m = mean(nm3$ANN)
nm4m = mean(nm4$ANN)

az <- read.csv("arizona.csv", header = TRUE)

az1 <- az[1:(nrow(az)/4),] #Wind speed at 50 meters
az2 <- az[((nrow(az)/4+1)):(2*(nrow(az)/4)),] #Wind speed at 50 meters range
az3 <- az[(2*(nrow(az)/4)+1):(3*(nrow(az)/4)),] #All sky downward irradiance
az4 <- az[(3*(nrow(az)/4)+1):nrow(az),] #Clear sky downward irradiance

az1m = mean(az1$ANN)
az2m = mean(az2$ANN)
az3m = mean(az3$ANN)
az4m = mean(az4$ANN)

nd <- read.csv("north_dakota.csv", header = TRUE)

nd1 <- nd[1:(nrow(nd)/4),] #Wind speed at 50 meters
nd2 <- nd[((nrow(nd)/4+1)):(2*(nrow(nd)/4)),] #Wind speed at 50 meters range
nd3 <- nd[(2*(nrow(nd)/4)+1):(3*(nrow(nd)/4)),] #All sky downward irradiance
nd4 <- nd[(3*(nrow(nd)/4)+1):nrow(nd),] #Clear sky downward irradiance

nd1m = mean(nd1$ANN)
nd2m = mean(nd2$ANN)
nd3m = mean(nd3$ANN)
nd4m = mean(nd4$ANN)

sd <- read.csv("south_dakota.csv", header = TRUE)

sd1 <- sd[1:(nrow(sd)/4),] #Wind speed at 50 meters
sd2 <- sd[((nrow(sd)/4+1)):(2*(nrow(sd)/4)),] #Wind speed at 50 meters range
sd3 <- sd[(2*(nrow(sd)/4)+1):(3*(nrow(sd)/4)),] #All sky downward irradiance
sd4 <- sd[(3*(nrow(sd)/4)+1):nrow(sd),] #Clear sky downward irradiance

sd1m = mean(sd1$ANN)
sd2m = mean(sd2$ANN)
sd3m = mean(sd3$ANN)
sd4m = mean(sd4$ANN)

wi <- read.csv("wisconsin.csv", header = TRUE)

wi1 <- wi[1:(nrow(wi)/4),] #Wind speed at 50 meters
wi2 <- wi[((nrow(wi)/4+1)):(2*(nrow(wi)/4)),] #Wind speed at 50 meters range
wi3 <- wi[(2*(nrow(wi)/4)+1):(3*(nrow(wi)/4)),] #All sky downward irradiance
wi4 <- wi[(3*(nrow(wi)/4)+1):nrow(wi),] #Clear sky downward irradiance

wi1m = mean(wi1$ANN)
wi2m = mean(wi2$ANN)
wi3m = mean(wi3$ANN)
wi4m = mean(wi4$ANN)

ne <- read.csv("nebraska.csv", header = TRUE)

ne1 <- ne[1:(nrow(ne)/4),] #Wind speed at 50 meters
ne2 <- ne[((nrow(ne)/4+1)):(2*(nrow(ne)/4)),] #Wind speed at 50 meters range
ne3 <- ne[(2*(nrow(ne)/4)+1):(3*(nrow(ne)/4)),] #All sky downward irradiance
ne4 <- ne[(3*(nrow(ne)/4)+1):nrow(ne),] #Clear sky downward irradiance

ne1m = mean(ne1$ANN)
ne2m = mean(ne2$ANN)
ne3m = mean(ne3$ANN)
ne4m = mean(ne4$ANN)

me <- read.csv("maine.csv", header = TRUE)

me1 <- me[1:(nrow(me)/4),] #Wind speed at 50 meters
me2 <- me[((nrow(me)/4+1)):(2*(nrow(me)/4)),] #Wind speed at 50 meters range
me3 <- me[(2*(nrow(me)/4)+1):(3*(nrow(me)/4)),] #All sky downward irradiance
me4 <- me[(3*(nrow(me)/4)+1):nrow(me),] #Clear sky downward irradiance

me1m = mean(me1$ANN)
me2m = mean(me2$ANN)
me3m = mean(me3$ANN)
me4m = mean(me4$ANN)

#Adding average wind speed, all sky irradiance, and clear sky irradiance to numerical dataframe
numerical$AVG_WIND_SPEED <- c(wy1m, me1m, mt1m, id1m, nm1m, nd1m, sd1m, az1m, wi1m, ne1m)
numerical$AVG_ALL_IRR <- c(wy3m, me3m, mt3m, id3m, nm3m, nd3m, sd3m, az3m, wi3m, ne3m)
numerical$AVG_CLR_IRR <- c(wy4m, me4m, mt4m, id4m, nm4m, nd4m, sd4m, az4m, wi4m, ne4m)

#Preparation of risks dataframe for KNN
risks$AVG_WIND_SPEED <- NA


library(naivebayes)
library(ggplot2)
library(e1071)
library(randomForest)

#Target wind potential - 1 if has potential, 0 if not
risksfr$WIND_POTE <- factor(c(1,0,1,1,0,1,1,1,0,1))

#knn test set preparation
knntest <- risks[,c("AREA", "EAL_SCORE", "HAIL_EALS","LTNG_EALS","SWND_EALS","TRND_EALS","ERQK_EALS")]
knntest$AVG_WIND_SPEED <- NA
knntest$AVG_ALL_IRR <- NA
knntest$AVG_CLR_IRR <- NA
num2 <- numerical
num2$STATE <- risksfr$STATE
for (i in 1:nrow(risks)) {
  if (risks$STATE[i] %in% num2$STATE) {
    current <- risks$STATE[i]
    crow <- filter(num2, STATE %in% c(current))
    str(crow)
    knntest$AVG_WIND_SPEED[i] <- crow$AVG_WIND_SPEED
    knntest$AVG_ALL_IRR[i] <- crow$AVG_ALL_IRR
    knntest$AVG_CLR_IRR[i] <- crow$AVG_CLR_IRR
    
    #print(risks$STATE[i], filter(risksfr, STATE == risks$STATE[i]))
    #print(risksfr[toString(i),])
  }
  #print(risks["46",])
}


knntest$AVG_WIND_SPEED[which(is.na(knntest$AVG_WIND_SPEED))] <- mean(knntest$AVG_WIND_SPEED, na.rm = TRUE)
knntest$AVG_ALL_IRR[which(is.na(knntest$AVG_ALL_IRR))] <- mean(knntest$AVG_ALL_IRR, na.rm = TRUE)
knntest$AVG_CLR_IRR[which(is.na(knntest$AVG_CLR_IRR))] <- mean(knntest$AVG_CLR_IRR, na.rm = TRUE)
knntest

#plotting of points in larger form
qplot(EAL_SCORE, AVG_WIND_SPEED, data=numerical, color = risksfr$EAL_RATNG)

#support vector machine model and plot
svm1 <- svm(risksfr$WIND_POTE ~ EAL_SCORE + AVG_WIND_SPEED, data = numerical, cost = 10, kernel="polynomial")

summary(svm1)

plot(svm1, data = numerical, EAL_SCORE ~ AVG_WIND_SPEED, slice = list(AREA=mean(AREA), HAIL_EALS = mean(HAIL_EALS), LTNG_EALS = mean(LTNG_EALS), SWND_EALS = mean(SWND_EALS), TRND_EALS = mean(TRND_EALS), ERQK_EALS = mean(ERQK_EALS)))
predict(svm1, data=knntest)

#conversion to factor
risks$EAL_RATNG <- factor(risks$EAL_RATNG)

#pair scatter plot
pairs(numerical)

#removal of hurricane - irrelevant
numerical <- subset(numerical, select = -c(HRCN_EALS))

#logistic model
logistic <- glm(risksfr$WIND_POTE ~ EAL_SCORE + AVG_WIND_SPEED, data = numerical, family="binomial")
summary(logistic)
newdf <- data.frame(EAL_SCORE = knntest$EAL_SCORE, AVG_WIND_SPEED = knntest$AVG_WIND_SPEED)

probabilities <- logistic %>% predict(newdf, type="response")
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)
predicted.classes
#knn model
library(class)
initial_k <- sqrt(nrow(numerical))
set.seed(42)
knn.3 = knn(numerical, knntest, risksfr$WIND_POTE, 3)
knn.4 = knn(numerical, knntest, risksfr$WIND_POTE, 4)
knn.5 = knn(numerical, knntest, risksfr$WIND_POTE, 5)

summary(knn.3)
summary(knn.4)
summary(knn.5)

#linear regression model and best fit line plot
linear <- lm(AVG_ALL_IRR ~ AREA, data = numerical)
summary(linear)
residuals(linear)
plot(numerical$AREA, numerical$AVG_ALL_IRR)
abline(linear)

