#implement regression methods on borough data

#library for svm
install.packages('e1071')
library(e1071)

#library for neural network
install.packages("neuralnet")
library(neuralnet)




test.boroughs <- boroughs

test.boroughs$price <- as.integer(test.boroughs$price)
test.boroughs$borough <- as.numeric(as.factor(test.boroughs$price))
test.boroughs$year <- as.numeric(test.boroughs$year)
test.boroughs$month <- as.numeric(test.boroughs$month)
test.boroughs$population <- as.integer(test.boroughs$population)
test.boroughs$child.pov <- gsub("%", "",test.boroughs$child.pov)
test.boroughs$child.pov <- as.numeric(test.boroughs$child.pov)
test.boroughs$income <- as.numeric(test.boroughs$income)
test.boroughs$JSA <- as.numeric(test.boroughs$JSA)
test.boroughs$living.wage <- as.numeric(test.boroughs$living.wage)

test.boroughs$crime.s <- NULL
test.boroughs$crime.t <- NULL
test.boroughs$crime.o <- NULL

test.boroughs2 <- test.boroughs
test.boroughs2$X <- NULL
test.boroughs2$living.wage <- NULL
test.boroughs2$population <- NULL
test.boroughs2$child.pov <- NULL
test.boroughs2$income <- NULL
test.boroughs2$JSA <- NULL


test.boroughs <- na.omit(test.boroughs)
test.boroughs2 <- na.omit(test.boroughs2)

#Check data is complete
apply(test.boroughs,2,function(x) sum(is.na(x)))
apply(test.boroughs2,2,function(x) sum(is.na(x)))




#split data into train and test sets 
index <- sample(1:nrow(test.boroughs),round(0.9*nrow(test.boroughs)))
train <- test.boroughs[index,]
test <- test.boroughs[-index,]


index2 <- sample(1:nrow(test.boroughs2),round(0.9*nrow(test.boroughs2)))
train2 <- test.boroughs2[index,]
test2 <- test.boroughs2[-index,]

#fit generalised linear model
glm.fit <- glm(price~., data=train2)
glm.predict <- predict(lm.fit,test2)

#calculate errors
glm.RMSE <- sqrt(sum((lm.predict - test$price)^2)/nrow(test))
glm.MAPE <- 100/length(lm.predict) * sum(abs((test$price-lm.predict)/
                                               test$price))

#fit svm to same sector
svm.fit2 <- svm(price~., data=train2, kernel="radial")
svm.predict2 <- predict(svm.fit,test2)
svm.fit <- svm(price~., data=train, kernel="radial")
svm.predict <- predict(svm.fit,test)

#calculate errors
svm.RMSE <- sqrt(sum((svm.predict - test$price)^2)/nrow(test))
svm.MAPE <- 100/length(svm.predict) * sum(abs((test$price-svm.predict)/
                                                test$price))

#calculate errors
svm.RMSE2 <- sqrt(sum((svm.predict2 - test2$price)^2)/nrow(test2))
svm.MAPE2 <- 100/length(svm.predict2) * sum(abs((test2$price-svm.predict2)/
                                                test2$price))


maxs <- apply(test.boroughs, 2, max) 
mins <- apply(test.boroughs, 2, min)

#scale dataset 1-0 to facilitate easier training, then remove user data
scaled <- as.data.frame(scale(test.boroughs, center = mins, scale = maxs - mins))
data.to.predict <- scaled[nrow(scaled),]
scaled <- scaled[-nrow(scaled), ]



  #split data into train and test sets 
  index <- sample(1:nrow(test.boroughs),round(0.9*nrow(test.boroughs)))
  nn.train <- scaled[index,]
  nn.test <- scaled[-index,]
  
  
  
  #train neural network
  n <- names(nn.train)
  f <- as.formula(paste("price ~", paste(n[!n %in% "price"], collapse = " + "))
  )
  nn <- neuralnet(f, data=nn.train, hidden=c(7,3), learningrate = 1, algorithm = "rprop+", err.fct = "sse", act.fct = "logistic", linear.output=T)
  plot(nn)
  
  #test neural network
  pr.nn <- compute(nn,nn.test[,2:10])
  
  #descale values to get comprehensible result
  pr.nn <- pr.nn$net.result*(max(test.boroughs$price)-min(test.boroughs$price))+min(
    test.boroughs$price)
  test.r <- (nn.test$price)*(max(test.boroughs$price)-min(test.boroughs$price))+min(
    test.boroughs$price)
  
  
  # Calculate error measures
  nn.RMSE <- sqrt(sum((test.r - pr.nn)^2)/nrow(nn.test))
  nn.MAPE <- 100/length(test.r) * sum(abs((test.r - pr.nn)/test.r))
  
  #update averaged error and prediction
  nn.RMSE.avg <- ((i-1)*nn.RMSE.avg + nn.RMSE)/i
  nn.MAPE.avg <- ((i-1)*nn.MAPE.avg + nn.MAPE)/i
  nn.predict.avg <- ((i-1)*nn.predict.avg + user.nn)/i
  


