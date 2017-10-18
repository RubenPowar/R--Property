#Implement multiple linear and svm regression on 95-14 LR data at OC level
#using grouped median method


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
LR.mod$record_status <- NULL
colnames(LR.mod2)[which(names(LR.mod2) == "V28")] <- "Postcode.sector"

LR.mod2 <- LR.mod

#Only to be done if postcode sector isnt present===============================

GetPostcodeSectorNumber <- function(postcode) {
  return(substr(postcode, nchar(postcode), nchar(postcode)))
}

#LR.mod$Postcode.sector <- "c0"
for (i in 1:nrow(medians.df)) {
  col.num <- as.integer(which(colnames(medians.df)=="Postcode.sector"))
  postcode.sec <- toString(medians.df[i, col.num])
  print(postcode.sec)
  medians.df[i, col.num] = toString(GetPostcodeSectorNumber(postcode.sec))
}

#==============================================================================

#Change variables to continuous
LR.mod2$tenure <- factor(as.numeric(LR.mod2$tenure))
LR.mod2$record_status <- factor(as.numeric(LR.mod2$record_status))
LR.mod2$borough_name <- factor(as.numeric(LR.mod2$borough_name))
LR.mod2$whether_new <- factor(as.numeric(LR.mod2$whether_new))
LR.mod2$town <- factor(as.numeric(LR.mod2$town))
LR.mod2$year_month <- as.numeric(factor(as.numeric(LR.mod2$year_month)))
LR.mod2$property_type <- factor(as.numeric(LR.mod2$property_type))
LR.mod2$year <- factor(as.numeric(LR.mod2$year))


#Price in thousands (rounded)
LR.mod2$price <- round(LR.mod2$price, -3)/1000

#Check data is complete
apply(LR.mod,2,function(x) sum(is.na(x)))


#get sales from NW1
colnames(LR.mod2)[which(names(LR.mod2) == "V28")] <- "Postcode.sector"


#Select outward code and get all EXISTING sectors

GetSectors <- function(DF) {
  out.code <- DF
  temp.dataframe <- data.frame()
  print(out.code)
  for (i in 0:9) {
    temp.sector <- paste(out.code, i)
    temp.sector2 <- toString(gsub(" ", "", paste(out.code, ".", i), fixed = TRUE))
    print(temp.sector2)
    temp.df <- split(LR.mod2,LR.mod2$Postcode.sector)[[temp.sector]]
    if(!is.null(temp.df)) {
      assign(temp.sector2, temp.df)
      temp.dataframe <- rbind(temp.dataframe, temp.df)
    }
  }
  return(temp.dataframe)
}

GetSectorMedians <- function(OC, util) {
  assign("medians.df", data.frame())
  temp.OC = eval(parse(text = deparse(substitute(OC))))
  for(i in 1:237) {
    num.of.rgroups = util
    temp.df = split(temp.OC, temp.OC$year_month)[[i]]
    temp.df = temp.df[sample(nrow(temp.df)),]
    temp.size = round(nrow(temp.df)/num.of.rgroups)
    for(j in 1:num.of.rgroups) {
      temp.df2 = temp.df[(1+(j-1)*temp.size):(j*temp.size),]
      temp.df2 = temp.df2[order(temp.df2$price),]
      temp.df2 = na.omit(temp.df2)
      df2.median = temp.df2[(ceiling(nrow(temp.df2)/2)),]
      medians.df = rbind(medians.df, df2.median)
    }
  }
  return(medians.df)
}

RoughDataUtilisation <- function(data.set, perc) {
  temp.int <- (perc*nrow(NW1))/(237*100)
  return(temp.int)
}
assign("NW1", data.frame())
NW1 <- GetSectors("NW1")
data.util <- RoughDataUtilisation(NW1, 20)
md.df <- GetSectorMedians(NW1, data.util)



medians.df <- data.frame()
for(i in 1:237) {
  num.of.rgroups <- 15
  temp.df <- split(NW1, NW1$year_month)[[i]]
  temp.df <- temp.df[sample(nrow(temp.df)),]
  temp.size <- round(nrow(temp.df)/5)
  for(j in 1:num.of.rgroups) {
    temp.df2 <- temp.df[(1+(j-1)*num.of.rgroups):(j*num.of.rgroups),]
    temp.df2 <- temp.df2[order(temp.df2$price),]
    temp.df2 <- na.omit(temp.df2)
    df2.median <- temp.df2[(ceiling(nrow(temp.df2)/2)),]
    medians.df <- rbind(medians.df, df2.median)
  }
}



E01002675 <- split(LR.mod2,LR.mod2$lsoa11)[["E01002675"]]
E02000534 <- split(LR.mod2,LR.mod2$msoa11)[["E02000534"]]
E00013288 <- split(LR.mod2,LR.mod2$oa11)[["E00013288"]]

TE01002675 <- split(TW7,TW7$lsoa11)[["E01002675"]]

#reformat data
NW1$town = NULL
NW1$Postcode.sector <- factor(as.numeric(as.factor(NW1$Postcode.sector)))


#fit linear regression to chose NW1 sector

#Chose sector
test.data <- md.df
test.data <- test.data[sample(nrow(test.data)),]

#remove data made obsolete
test.data$X.1 <- NULL
test.data$record_status <- NULL
test.data$town <- NULL
test.data$post_code <- NULL
test.data$Postcode.sector <- as.integer(as.factor(test.data$Postcode.sector))

#split data into train and test sets 90:10
index <- sample(1:nrow(test.data),round(0.9*nrow(test.data)))
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


