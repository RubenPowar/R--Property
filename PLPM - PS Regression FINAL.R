#Implement multiple linear and svm regression on 95-14 LR data at sector level


#library for svm
install.packages('e1071')
library(e1071)




#STATE YOUR CHOICES HERE:======================================================


outward.code.choice <- "NW1"      # as string
postcode.sector.choice <- "NW1.7" # as variable (outward.code.choice is substr)
postcode.sectors.exist <- TRUE    # have postcode sectors been found
perc.to.remove <- 2               # numeric chosing top and bottom n% to remove
perc.in.train.set <- 90           # state the percentage 0-100 (not 0-1)
num.to.avg <- 10                  # number of models to average
attempt.grid.search <- FALSE      # True if you wish to improve svm model  (see
                                  # notes by code for impact)

year.of.choice <- 2016            # year as numeric
month.of.choice <- 1              # 1=Jan, 2=Feb...
whether.new.choice <- 'N'         # Y/N
tenure.choice <- 'F'              # F/L
property.type.choice <- 'D'       # D/F/S/T
postcode.choice <- "NW1 7SA"      # string containing postcode selection

#==============================================================================
if(!exists("LR")) {
  #read data and remove unnecessary attributes
  LR1 <- read.csv("price data/LR1.csv")
  LR2 <- read.csv("price data/LR2.csv")
  LR3 <- read.csv("price data/LR3.csv")
  LR4 <- read.csv("price data/LR4.csv")
  LR <- rbind(LR1, LR2, LR3, LR4)
  LR.mod <- LR

  #Clean up variable names, and remove unwanted data
  LR.mod$X <- NULL
  LR.mod$X.1 <- NULL
  LR.mod$address1 <- NULL
  LR.mod$address2 <- NULL
  LR.mod$address3 <- NULL
  LR.mod$address4 <- NULL
  LR.mod$local_authority <- NULL
  LR.mod$borough_code <- NULL
  LR.mod$county <- NULL
  LR.mod$date_processed <- NULL
  LR.mod$inner_outer <- NULL
#  LR.mod$lsoa11 <- NULL
#  LR.mod$msoa11 <- NULL
#  LR.mod$month <- NULL
#  LR.mod$oa11 <- NULL
  LR.mod$record_status <- NULL
  LR.mod$transaction_id <- NULL
  LR.mod$town <- NULL
  LR.mod$ward_code <- NULL

  colnames(LR.mod)[which(names(LR.mod) == "V28")] <- "Postcode.sector"

  LR.mod2 <- LR.mod

  #Price in thousands (rounded)
  LR.mod2$price <- round(LR.mod2$price, -3)/1000
  
}


  
#Only to be done if postcode sector isnt present===============================

GetPostcodeSector <- function(postcode) {
  return(substr(postcode, 1, nchar(postcode)-2))
}

if (!postcode.sectors.exist) {
  
  #LR.mod$Postcode.sector <- "c0"
  for (i in 1:nrow(LR.mod)) {
    #col.num <- as.integer(which(colnames(LR.mod)=="post_code"))
    postcode <- toString(LR.mod[i, 16])
    LR.mod[i, 28] = toString(GetPostcodeSector(postcode))
  }
}

#==============================================================================

#Change variables to continuous
LR.mod2$tenure <- as.numeric(factor(LR.mod2$tenure))
#LR.mod2$record_status <- factor(as.numeric(LR.mod2$record_status))
LR.mod2$borough_name <- as.numeric(factor(LR.mod2$borough_name))
LR.mod2$whether_new <- as.numeric(factor(LR.mod2$whether_new))
#LR.mod2$town <- factor(as.numeric(LR.mod2$town))
LR.mod2$year_month <- as.numeric(factor(as.numeric(LR.mod2$year_month)))
LR.mod2$property_type <- as.numeric(factor(LR.mod2$property_type))
LR.mod2$year <- as.numeric(factor(LR.mod2$year))
LR.mod2$lsoa11 <- as.numeric(factor(LR.mod2$lsoa11))
LR.mod2$msoa11 <- as.numeric(factor(LR.mod2$msoa11))
LR.mod2$oa11 <- as.numeric(factor(LR.mod2$oa11))
LR.mod2$month <- as.numeric(factor(LR.mod2$month))
LR.mod2$quarter <- as.numeric(factor(LR.mod2$quarter))


#Check data is complete
apply(LR.mod2,2,function(x) sum(is.na(x)))



#Select outward code and get all EXISTING sectors
out.code <- toString(outward.code.choice)
outward.code <- data.frame()
for (i in 0:9) {
  temp.sector <- paste(out.code, i)
  temp.sector2 <- toString(gsub(" ", "", paste(out.code, ".", i), fixed = TRUE))
  print(temp.sector2)
  temp.df <- split(LR.mod2,LR.mod2$Postcode.sector)[[temp.sector]]
  if(!is.null(temp.df)) {
  assign(temp.sector2, temp.df)
    outward.code <- rbind(outward.code, temp.df)
  }
}

#E01002675 <- split(LR.mod2,LR.mod2$lsoa11)[["E01002675"]]
#E02000534 <- split(LR.mod2,LR.mod2$msoa11)[["E02000534"]]
#E00013288 <- split(LR.mod2,LR.mod2$oa11)[["E00013288"]]

#TE01002675 <- split(TW7,TW7$lsoa11)[["E01002675"]]

temp.result <- split(outward.code,outward.code$post_code)[[postcode.choice]]
lsoa11 <- temp.result[1,]$lsoa11
msoa11 <- temp.result[1,]$msoa11
oa11   <- temp.result[1,]$oa11


#reformat postcode sector value
outward.code$Postcode.sector <- as.numeric(as.factor(outward.code$Postcode.sector))
outward.code$post_code <- NULL

#Prepare data to predict-------------------------------------------------------

#create record with user input
property.type.choice <- match(property.type.choice, c('D', 'F', 'S', 'T'))
tenure.choice <- match(tenure.choice, c('F', 'L', 'U'))
whether.new.choice <- match(whether.new.choice, c('N', 'Y'))

data.to.predict <- test[1,]
data.to.predict$lsoa11 <- lsoa11
data.to.predict$month <- month.of.choice
data.to.predict$msoa11 <- msoa11
data.to.predict$oa11 <- oa11
data.to.predict$property_type <- property.type.choice
data.to.predict$quarter <- ceiling(month.of.choice/3)
data.to.predict$tenure <- tenure.choice
data.to.predict$whether_new <- whether.new.choice
data.to.predict$year <- year.of.choice-1994
data.to.predict$year_month <- (year.of.choice-1995)*12 + month.of.choice

#------------------------------------------------------------------------------

#fit linear regression to chosen sector


#Chose sector
assign("test.data", eval(parse(text = postcode.sector.choice)))
test.data <- test.data[sample(nrow(test.data)),]

#remove extremes n% (n determined by temp.int)
RemovePerecents <- function(temp.data, temp.int) {
  temp.data = temp.data[order(temp.data$price),]
  t.length <- nrow(temp.data)
  num.to.remove <- temp.int*t.length/100
  temp.data <- temp.data[round(num.to.remove):(t.length-num.to.remove),]
  return(temp.data)
}
test.data <- RemovePerecents(test.data, perc.to.remove)

#remove data made obsolete
test.data$borough_name <- NULL
test.data$Postcode.sector <- NULL
test.data$post_code <- NULL


train.perc <- perc.in.train.set/100

#instantiate average variables
glm.RMSE.avg <- 0
glm.MAPE.avg <- 0
svm.RMSE.avg <- 0
svm.MAPE.avg <- 0
glm.predict.avg <- 0
svm.predict.avg <- 0


for (i in 1:num.to.avg) {

  #split data into train and test sets 
  index <- sample(1:nrow(test.data),round(train.perc*nrow(test.data)))
  train <- test.data[index,]
  test <- test.data[-index,]

  #fit generalised linear model
  glm.fit <- glm(price~., data=train)
  glm.predict <- predict(lm.fit,test)

  #calculate errors
  glm.RMSE <- sqrt(sum((lm.predict - test$price)^2)/nrow(test))
  glm.MAPE <- 100/length(lm.predict) * sum(abs((test$price-lm.predict)/
                                                test$price))

  #fit svm to same sector
  svm.fit <- svm(price~., data=train, kernel="radial")
  svm.predict <- predict(svm.fit,test)

  #calculate errors
  svm.RMSE <- sqrt(sum((svm.predict - test$price)^2)/nrow(test))
  svm.MAPE <- 100/length(svm.predict) * sum(abs((test$price-svm.predict)/
                                                test$price))
  
  
  glm.RMSE.avg <- ((i-1)*glm.RMSE.avg + glm.RMSE)/i
  glm.MAPE.avg <- ((i-1)*glm.MAPE.avg + glm.MAPE)/i
  svm.RMSE.avg <- ((i-1)*svm.RMSE.avg + svm.RMSE)/i
  svm.MAPE.avg <- ((i-1)*svm.MAPE.avg + svm.MAPE)/i
  
  #predict user input
  user.predict.glm <- predict(lm.fit, data.to.predict)
  user.predict.svm <- predict(svm.fit, data.to.predict)
  
  glm.predict.avg <- ((i-1)*glm.predict.avg + user.predict.glm)/i
  svm.predict.avg <- ((i-1)*glm.predict.avg + user.predict.svm)/i
  
}

#compare errors
MAPE.improvement <- glm.MAPE.avg - svm.MAPE.avg
RMSE.improvement <- glm.RMSE.avg - svm.RMSE.avg

PrintResults <- function() {
  print(paste("when trained with", num.to.avg, "models"))
  print(paste("MAPE.improvement:", MAPE.improvement, "RMSE.improvement:",
            RMSE.improvement))
  print(paste("MAPE :    GLM:     ", lm.MAPE, "   SVM:     ", svm.MAPE))
  print(paste("RMSE :    GLM:     ", lm.RMSE, "   SVM:     ", svm.RMSE))
  print(paste("Our Generalised Linear Model predicts a value of £", 
              round(user.predict.glm*1000)))
  print(paste("Our Support Vector Regression Model predicts a value of £", 
              round(user.predict.svm*1000)))
}
PrintResults()

if (attempt.grid.search){

  #improve svm
  # perform a grid search
  tuneResult <- tune(svm, price ~ .,  data = test.data,
                   ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9)))
  print(tuneResult)
  plot(tuneResult)

  #finer grid search
  temp.seq <- seq(0.15,0.25,0.01)
  temp.seq2 <- seq(16,20,0.5)
  tuneResult2 <- tune(svm, price ~ .,  data = test.data,
                   ranges = list(epsilon = temp.seq, cost = temp.seq2))
  print(tuneResult2)
  plot(tuneResult2)

  tuned.svm.fit <- svm(price~., data=train, kernel="radial", cost=17.5, epsilon
                     =0.18, gamma=0.0357)
  tuned.svm.predict <- predict(svm.fit,test)
  tuned.svm.RMSE <- sqrt(sum((svm.predict - test$price)^2)/nrow(test))
  tuned.svm.MAPE <- 100/length(svm.predict) * sum(abs((test$price-svm.predict)/
                                                      test$price))

  #pick best model
  tunedModel <- tuneResult2$best.model
  tunedModelY <- predict(tunedModel, test.data)

  error <- test.data$price - tunedModelY  
  tunedModelRMSE <- sqrt(mean(error^2))

#  We see that our support vector machine regression model invariably converges 
#to a final prediction model, the MAPE and RMSE of which do not change with any
#additional tuning using our grid search


}

#==============================================================================

#data1 <- test_x
#data1$borough_name <- NULL
#data1$record_status <- NULL
#data1$town <- NULL
#index <- sample(1:nrow(data1),round(0.50*nrow(data1)))
#train <- data1[index,]
#test <- data1[-index,]
#lm.fit <- glm(price~., data=train)
#summary(lm.fit)
#pr.lm <- predict(lm.fit,test)
#MSE.lm <- sum((pr.lm - test$price)^2)/nrow(test)

#plot(test$price,pr.lm,col='blue',main='Real vs predicted lm',pch=18, cex=0.7)
#abline(0,1,lwd=2)
#legend('bottomright',legend='LM',pch=18,col='blue', bty='n', cex=.95)

#==============================================================================


#make prediction on user input-------------------------------------------------

#create record with user input
#property.type.choice <- match(property.type.choice, c('D', 'F', 'S', 'T'))
#tenure.choice <- match(tenure.choice, c('F', 'L', 'U'))
#whether.new.choice <- match(whether.new.choice, c('N', 'Y'))

#data.to.predict <- test[1,]
#data.to.predict$lsoa11 <- lsoa11
#data.to.predict$month <- month.of.choice
#data.to.predict$msoa11 <- msoa11
#data.to.predict$oa11 <- oa11
#data.to.predict$property_type <- property.type.choice
#data.to.predict$quarter <- ceiling(month.of.choice/3)
#data.to.predict$tenure <- tenure.choice
#data.to.predict$whether_new <- whether.new.choice
#data.to.predict$year <- year.of.choice-1994
#data.to.predict$year_month <- (year.of.choice-1995)*12 + month.of.choice

#make prediction

#user.predict.glm <- predict(lm.fit, data.to.predict)
#user.predict.svm <- predict(svm.fit, data.to.predict)



#print out important information
#RMSE.improvement <- lm.RMSE - svm.RMSE
#print(paste("MAPE.improvement:", MAPE.improvement, "RMSE.improvement:",
#            RMSE.improvement))
#print(paste("MAPE :    GLM:     ", lm.MAPE, "   SVM:     ", svm.MAPE))
#print(paste("RMSE :    GLM:     ", lm.RMSE, "   SVM:     ", svm.RMSE))

#print(paste("Our Generalised Linear Model predicts a value of £", 
#            round(user.predict.glm*1000)))
#print(paste("Our Support Vector Regression Model predicts a value of", 
#            round(user.predict.svm*1000)))



