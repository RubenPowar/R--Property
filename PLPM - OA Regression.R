#Implement multiple linear and svm regression on 95-14 LR data at l/msoa level


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

#Clean up variable names, and remove unwanted data
LR.mod$X <- NULL
LR.mod$address1 <- NULL
LR.mod$address2 <- NULL
LR.mod$address3 <- NULL
LR.mod$address4 <- NULL
LR.mod$borough_code <- NULL
LR.mod$county <- NULL
LR.mod$date_processed <- NULL
LR.mod$inner_outer <- NULL
LR.mod$ward_code <- NULL
LR.mod$transaction_id <- NULL
LR.mod$local_authority <- NULL
colnames(LR.mod2)[which(names(LR.mod2) == "V28")] <- "Postcode.sector"

LR.mod2 <- LR.mod

#Only to be done if postcode sector isnt present===============================

GetPostcodeSector <- function(postcode) {
  return(substr(postcode, 1, nchar(postcode)-2))
}

#LR.mod$Postcode.sector <- "c0"
for (i in 1:nrow(LR.mod)) {
  #col.num <- as.integer(which(colnames(LR.mod)=="post_code"))
  postcode <- toString(LR.mod[i, 16])
  LR.mod[i, 28] = toString(GetPostcodeSector(postcode))
}

#==============================================================================

#Change variables to continuous
LR.mod2$tenure <- factor(as.numeric(LR.mod2$tenure))
LR.mod2$record_status <- factor(as.numeric(LR.mod2$record_status))
LR.mod2$borough_name <- factor(as.numeric(LR.mod2$borough_name))
LR.mod2$whether_new <- factor(as.numeric(LR.mod2$whether_new))
LR.mod2$town <- factor(as.numeric(LR.mod2$town))
LR.mod2$year_month <- factor(as.numeric(LR.mod2$year_month))
LR.mod2$property_type <- factor(as.numeric(LR.mod2$property_type))
LR.mod2$year <- factor(as.numeric(LR.mod2$year))


E01002675 <- split(LR.mod2,LR.mod2$lsoa11)[["E01002675"]]
E02000534 <- split(LR.mod2,LR.mod2$msoa11)[["E02000534"]]
E00013288 <- split(LR.mod2,LR.mod2$oa11)[["E00013288"]]

OArea <- E00013288
OArea$borough_name <- NULL
OArea$lsoa11 <- NULL
OArea$msoa11 <- NULL
OArea$oa11 <- NULL
OArea$record_status <- NULL
OArea$V28 <- NULL
OArea$post_code <- NULL
OArea$town <- NULL
OArea$year_month <- as.numeric(OArea$year_month)
OArea$year <- as.numeric(OArea$year)



#Chose sector
test.data <- OArea
test.data <- test.data[sample(nrow(test.data)),]

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








