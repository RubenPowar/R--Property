#Implement neural network on 95-14 basic LR Data

#read data and remove unnecessary attributes
LRSales_95_14 <- read.csv("Saved Data/combined_95_14.csv")
LR.mod <- LRSales_95_14


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
for (i in 113000:771192) {
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





