#Implement multiple linear and svm regression on 95-14 LR data at O.C. level

#library for svm
install.packages('e1071')
library(e1071)

#read data and remove unnecessary attributes
LR1 <- read.csv("price data/LR1.csv")
LR2 <- read.csv("price data/LR2.csv")
LR3 <- read.csv("price data/LR3.csv")
LR4 <- read.csv("price data/LR4.csv")
LR <- rbind(LR1, LR2, LR3, LR4)
LR.mod <- LR

LR.mod$X <- NULL
LR.mod$address1 <- NULL
LR.mod$address2 <- NULL
LR.mod$address3 <- NULL
LR.mod$address4 <- NULL
LR.mod$local_authority <- NULL
LR.mod$borough_code <- NULL
LR.mod$county <- NULL
LR.mod$date_processed <- NULL
LR.mod$inner_outer <- NULL
LR.mod$lsoa11 <- NULL
LR.mod$msoa11 <- NULL
LR.mod$month <- NULL
LR.mod$oa11 <- NULL
LR.mod$transaction_id <- NULL
LR.mod$ward_code <- NULL
colnames(LR.mod2)[which(names(LR.mod2) == "V28")] <- "Postcode.sector"

#Change variables to continuous
LR.mod2$tenure <- factor(as.numeric(LR.mod2$tenure))
LR.mod2$record_status <- factor(as.numeric(LR.mod2$record_status))
LR.mod2$borough_name <- factor(as.numeric(LR.mod2$borough_name))
LR.mod2$whether_new <- factor(as.numeric(LR.mod2$whether_new))
LR.mod2$town <- factor(as.numeric(LR.mod2$town))
LR.mod2$year_month <- factor(as.numeric(LR.mod2$year_month))
LR.mod2$property_type <- factor(as.numeric(LR.mod2$property_type))
LR.mod2$year <- factor(as.numeric(LR.mod2$year))


#Select outward code and get all EXISTING sectors
out.code <- "TW7"
for (i in 0:9) {
  temp.sector <- paste(out.code, i)
  temp.sector2 <- toString(gsub(" ", "", paste(out.code, ".", i), fixed = TRUE))
  print(temp.sector2)
  temp.df <- split(LR.mod2,LR.mod2$Postcode.sector)[[temp.sector]]
  if(!is.null(temp.df)) {
    assign(temp.sector2, temp.df)
  }
}


#combine all NW1 sales
NW1 <- rbind(NW1.1,NW1.2,NW1.3,NW1.4,NW1.5,NW1.6,NW1.7,NW1.8,NW1.9,NW1.0)

#combine all TW7 sales
TW7 <- rbind(TW7.4,TW7.5,TW7.6,TW7.7)

#reformat data
TW7$town = NULL
TW7$borough_name = NULL
TW7$Postcode.sector <- factor(as.numeric(as.factor(NW1$Postcode.sector)))


#fit linear regression to chose NW1 sector

#Chose sector
test.data <- N5
test.data <- test.data[sample(nrow(test.data)),]

#remove data made obsolete
test.data$borough_name <- NULL
test.data$record_status <- NULL
test.data$town <- NULL
test.data$Postcode.sector <- NULL
test.data$record_status <- NULL

#split data into train and test sets 90:10
index <- sample(1:nrow(test.data),round(0.90*nrow(test.data)))
train <- test.data[index,]
test <- test.data[-index,]

#fit generalised linear model
lm.fit <- glm(price~., data=train)
lm.predict <- predict(lm.fit,test)

#calculate errors
lm.RMSE <- sqrt(sum((lm.predict - test$price)^2)/nrow(test))
lm.MAPE <- 100/length(lm.predict) * sum(abs((test$price-lm.predict)/test$price))

#fit svm to same sector
svm.fit <- svm(price~., data=train, kernel="radial")
svm.predict <- predict(svm.fit,test)

#calculate errors
svm.RMSE <- sqrt(sum((svm.predict - test$price)^2)/nrow(test))
svm.MAPE <- 100/length(svm.predict) * sum(abs((test$price-svm.predict)/test$price))

#compare errors
MAPE.improvement <- lm.MAPE - svm.MAPE
RMSE.improvement <- lm.RMSE - svm.RMSE
print(paste("MAPE.improvement:", MAPE.improvement, "RMSE.improvement:", RMSE.improvement))
print(paste("MAPE :    GLM:     ", lm.MAPE, "   SVM:     ", svm.MAPE))
print(paste("RMSE :    GLM:     ", lm.RMSE, "   SVM:     ", svm.RMSE))
