#Implement a Neural Network to predict at the Postcode Sector level


install.packages("neuralnet")
library(neuralnet)


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
LR.mod$transaction_id <- NULL
LR.mod$ward_code <- NULL
LR.mod$record_status <- NULL
colnames(LR.mod)[which(names(LR.mod) == "V28")] <- "Postcode.sector"

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

#remove postcode (after sector found)
LR.mod2$post_code <- NULL

#Change variables to continuous
LR.mod2$tenure <- as.numeric(factor(LR.mod2$tenure))
LR.mod2$borough_name <- as.numeric(factor(LR.mod2$borough_name))
LR.mod2$whether_new <- as.numeric(LR.mod2$whether_new)
LR.mod2$town <- as.numeric(factor(LR.mod2$town))
LR.mod2$year_month <- as.numeric(factor(LR.mod2$year_month))
LR.mod2$property_type <- factor(as.numeric(LR.mod2$property_type))
LR.mod2$year <- as.numeric(LR.mod2$year)
LR.mod2$month <- as.numeric(LR.mod2$month)
LR.mod2$lsoa11 <- as.numeric(as.factor(LR.mod2$lsoa11))
LR.mod2$msoa11 <- as.numeric(as.factor(LR.mod2$msoa11))
LR.mod2$oa11 <- as.numeric(as.factor(LR.mod2$oa11))
LR.mod2$property_type <- as.numeric(as.factor(LR.mod2$property_type))
LR.mod2$price <- as.numeric(LR.mod2$price)
LR.mod2$quarter <- as.numeric(LR.mod2$quarter)


#Price in thousands (rounded)
LR.mod2$price <- round(LR.mod2$price, -3)/1000

#Check data is complete
apply(LR.mod2,2,function(x) sum(is.na(x)))
sapply(LR.mod2, class)


#get sales from NW1

#Select outward code and get all EXISTING sectors
out.code <- "TW7"
Outward.Code <- data.frame()
for (i in 0:9) {
  temp.sector <- paste(out.code, i)
  temp.sector2 <- toString(gsub(" ", "", paste(out.code, ".", i), fixed = TRUE))
  print(temp.sector2)
  temp.df <- split(LR.mod2,LR.mod2$Postcode.sector)[[temp.sector]]
  if(!is.null(temp.df)) {
    assign(temp.sector2, temp.df)
    Outward.Code <- rbind(Outward.Code, temp.df)
  }
}
Outward.Code$Postcode.sector <- as.numeric(as.factor(Outward.Code$Postcode.sector))


E01002675 <- split(LR.mod2,LR.mod2$lsoa11)[["E01002675"]]
E02000534 <- split(LR.mod2,LR.mod2$msoa11)[["E02000534"]]
E00013288 <- split(LR.mod2,LR.mod2$oa11)[["E00013288"]]

TE01002675 <- split(TW7,TW7$lsoa11)[["E01002675"]]


#reformat data
Outward.Code$town = NULL
Outward.Code$Postcode.sector <- factor(as.numeric(as.factor(Outward.Code$Postcode.sector)))


#fit linear regression to chose NW1 sector

#Chose sector
test.data <- TW7.5
test.data <- test.data[sample(nrow(test.data)),]


#Price in thousands (rounded)
test.data$price <- round(test.data$price, -3)/1000

#remove data made obsolete
test.data$borough_name <- NULL
test.data$town <- NULL
test.data$Postcode.sector <- NULL
test.data <- subset(test.data, select=c(5,1:4,6:11))


sapply(test.data, class)

maxs <- apply(test.data, 2, max) 
mins <- apply(test.data, 2, min)

scaled <- as.data.frame(scale(test.data, center = mins, scale = maxs - mins))

train_ <- scaled[index,]
test_ <- scaled[-index,]

n <- names(train_)
f <- as.formula(paste("price ~", paste(n[!n %in% "price"], collapse = " + ")))
nn <- neuralnet(f,data=train_,hidden=c(7,3),err.fct = "sse", act.fct = "logistic",linear.output=T)
plot(nn)

# Predict
pr.nn <- compute(nn,test_[,2:11])

# Results from NN are normalized (scaled)
# Descaling for comparison
pr.nn_ <- pr.nn$net.result*(max(test.data$price)-min(test.data$price))+min(test.data$price)
test.r <- (test_$price)*(max(test.data$price)-min(test.data$price))+min(test.data$price)

# Calculating RMSE
nn.RMSE <- sqrt(sum((test.r - pr.nn_)^2)/nrow(test_))

# Calculating MAPE
nn.MAPE <- 100/length(test.r) * sum(abs((test.r - pr.nn_)/test.r))

print(paste("MAPE :    GLM:     ", lm.MAPE, "   SVM:     ", svm.MAPE, "   NN:     ", nn.MAPE))
print(paste("RMSE :    GLM:     ", lm.RMSE, "   SVM:     ", svm.RMSE, "   NN:     ", nn.RMSE))





