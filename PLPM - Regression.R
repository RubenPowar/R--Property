#Implement multiple linear regression on 95-14 basic LR Data


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

LR.mod2 <- LR.mod

GetPostcodeSector <- function(postcode) {
  return(substr(postcode, 1, nchar(postcode)-2))
}

#LR.mod$Postcode.sector <- "c0"
for (i in 1:nrow(LR.mod)) {
  #col.num <- as.integer(which(colnames(LR.mod)=="post_code"))
  postcode <- toString(LR.mod[i, 16])
  LR.mod[i, 28] = toString(GetPostcodeSector(postcode))
}

#Change variables to continuous
LR.mod2$tenure <- factor(as.numeric(LR.mod2$tenure))
LR.mod2$record_status <- factor(as.numeric(LR.mod2$record_status))
LR.mod2$borough_name <- factor(as.numeric(LR.mod2$borough_name))
LR.mod2$whether_new <- factor(as.numeric(LR.mod2$whether_new))
LR.mod2$town <- factor(as.numeric(LR.mod2$town))
LR.mod2$year_month <- factor(as.numeric(LR.mod2$year_month))
LR.mod2$property_type <- factor(as.numeric(LR.mod2$property_type))
LR.mod2$year <- factor(as.numeric(LR.mod2$year))


#Price in thousands (rounded)
LR.mod2$price <- round(LR.mod2$price, -3)/1000

#Check data is complete
apply(LR.mod,2,function(x) sum(is.na(x)))

#get sales from one borough
test_x = split(LR.mod2,LR.mod2$borough_name)[[10]]
test_y = split(LR.mod2,LR.mod2$borough_name!=10)[['TRUE']]


#get sales from NW1
colnames(LR.mod2)[which(names(LR.mod2) == "V28")] <- "Postcode.sector"
NW1.1 = split(LR.mod2,LR.mod2$Postcode.sector)[["NW1 1"]]
NW1.2 = split(LR.mod2,LR.mod2$Postcode.sector)[["NW1 2"]]
NW1.3 = split(LR.mod2,LR.mod2$Postcode.sector)[["NW1 3"]]
NW1.4 = split(LR.mod2,LR.mod2$Postcode.sector)[["NW1 4"]]
NW1.5 = split(LR.mod2,LR.mod2$Postcode.sector)[["NW1 5"]]
NW1.6 = split(LR.mod2,LR.mod2$Postcode.sector)[["NW1 6"]]
NW1.7 = split(LR.mod2,LR.mod2$Postcode.sector)[["NW1 7"]]
NW1.8 = split(LR.mod2,LR.mod2$Postcode.sector)[["NW1 8"]]
NW1.9 = split(LR.mod2,LR.mod2$Postcode.sector)[["NW1 9"]]
NW1.0 = split(LR.mod2,LR.mod2$Postcode.sector)[["NW1 0"]]

#combine all NW1 sales
NW1 <- rbind(NW1.1,NW1.2,NW1.3,NW1.4,NW1.5,NW1.6,NW1.7,NW1.8,NW1.9,NW1.0)

#reformat data
NW1$town = NULL
NW1$Postcode.sector <- factor(as.numeric(as.factor(NW1$Postcode.sector)))

#remove data with only one value
NW1.1$borough_name <- NULL
NW1.1$record_status <- NULL
NW1.1$town <- NULL
NW1.1$Postcode.sector <- NULL

#fit linear regression to random NW1 sector

test.data <- NW1.1[sample(nrow(NW1.1)),]
index <- sample(1:nrow(test.data),round(0.95*nrow(test.data)))
train <- test.data[index,]
test <- test.data[-index,]
lm.fit <- glm(price~., data=train)
lm.predict <- predict(lm.fit,test)
lm.RMSE <- sqrt(sum((lm.predict - test$price)^2)/nrow(test))

#fit svm to same sector
test.data <- NW1.1[sample(nrow(NW1.1)),]
index <- sample(1:nrow(test.data),round(0.95*nrow(test.data)))
train <- test.data[index,]
test <- test.data[-index,]
svm.fit <- svm(price~., data=train, cost=2, epsilon=0.2)
svm.predict <- predict(svm.fit,test)
svm.RMSE <- sqrt(sum((svm.predict - test$price)^2)/nrow(test))

#improve svm
# perform a grid search
tuneResult <- tune(svm, price ~ .,  data = test.data,
                   ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9)))
print(tuneResult)
plot(tuneResult)

#finer grid search
temp.seq <- seq(0.1,0.3,0.01)
temp.seq2 <- seq(0.5,15,0.5)
tuneResult2 <- tune(svm, price ~ .,  data = test.data,
                   ranges = list(epsilon = temp.seq, cost = temp.seq2))
print(tuneResult2)
plot(tuneResult2)

#pick best model
tunedModel <- tuneResult2$best.model
tunedModelY <- predict(tunedModel, test.data)

error <- test.data$price - tunedModelY  
tunedModelRMSE <- sqrt(mean(error^2))



data1 <- test_x
data1$borough_name <- NULL
data1$record_status <- NULL
data1$town <- NULL
index <- sample(1:nrow(data1),round(0.50*nrow(data1)))
train <- data1[index,]
test <- data1[-index,]
lm.fit <- glm(price~., data=train)
summary(lm.fit)
pr.lm <- predict(lm.fit,test)
MSE.lm <- sum((pr.lm - test$price)^2)/nrow(test)

#fit a linear regression model
index <- sample(1:nrow(LR.mod4),round(0.75*nrow(LR.mod4)))
train <- LR.mod4[index,]
test <- LR.mod4[-index,]
lm.fit <- glm(price~., data=train)
summary(lm.fit)
pr.lm <- predict(lm.fit,test)
MSE.lm <- sum((pr.lm - test$price)^2)/nrow(test)


plot(test$price,pr.lm,col='blue',main='Real vs predicted lm',pch=18, cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='LM',pch=18,col='blue', bty='n', cex=.95)





