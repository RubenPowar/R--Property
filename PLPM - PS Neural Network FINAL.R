#Implement Neural Network on 95-14 LR data at sector level


#library for neural network
install.packages("neuralnet")
library(neuralnet)




#STATE YOUR CHOICES HERE:======================================================


outward.code.choice <- "NW1"      # as string
postcode.sector.choice <- "NW1.7" # as variable (outward.code.choice is substr)
postcode.sectors.exist <- TRUE    # have postcode sectors been found
perc.to.remove <- 2               # numeric chosing top and bottom n% to remove
perc.in.train.set <- 90           # state the percentage 0-100 (not 0-1)
num.to.avg <- 1                   # number of models to average (keep low)
hidden.layer <- c(7,3)            # vector containing hidden layer(s) structure

year.of.choice <- 2015            # year as numeric
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
property.type.choice2 <- match(property.type.choice, c('D', 'F', 'S', 'T'))
tenure.choice2 <- match(tenure.choice, c('F', 'L', 'U'))
whether.new.choice2 <- match(whether.new.choice, c('N', 'Y'))

data.to.predict <- test[1,]
data.to.predict$lsoa11 <- lsoa11
data.to.predict$month <- month.of.choice
data.to.predict$msoa11 <- msoa11
data.to.predict$oa11 <- oa11
data.to.predict$property_type <- property.type.choice2
data.to.predict$quarter <- ceiling(month.of.choice/3)
data.to.predict$tenure <- tenure.choice2
data.to.predict$whether_new <- whether.new.choice2
data.to.predict$year <- year.of.choice-1994
data.to.predict$year_month <- (year.of.choice-1995)*12 + month.of.choice

#------------------------------------------------------------------------------

#train Neural Network on chosen sector

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

#remove data made obsolete and rearrange columns
test.data$borough_name <- NULL
test.data$Postcode.sector <- NULL
test.data$post_code <- NULL
test.data <- subset(test.data, select=c(5,1:4,6:11))


#add user input to test data so scaling takes it into account
test.data <- rbind(test.data, data.to.predict)

train.perc <- perc.in.train.set/100

#instantiate average variables
nn.RMSE.avg <- 0
nn.MAPE.avg <- 0
nn.predict.avg <- 0

sapply(test.data, class)

maxs <- apply(test.data, 2, max) 
mins <- apply(test.data, 2, min)

#scale dataset 1-0 to facilitate easier training, then remove user data
scaled <- as.data.frame(scale(test.data, center = mins, scale = maxs - mins))
data.to.predict <- scaled[nrow(scaled),]
scaled <- scaled[-nrow(scaled), ]



for (i in 1:num.to.avg) {
  
  
  
  #split data into train and test sets 
  index <- sample(1:nrow(test.data),round(train.perc*nrow(test.data)))
  nn.train <- scaled[index,]
  nn.test <- scaled[-index,]
  
  
  
  #train neural network
  n <- names(nn.train)
  f <- as.formula(paste("price ~", paste(n[!n %in% "price"], collapse = " + "))
                  )
  nn <- neuralnet(f, data=nn.train, hidden=hidden.layer, learningrate = 1, algorithm = "rprop+", err.fct = "sse", act.fct = "logistic", linear.output=T)
  plot(nn)
  
  #test neural network
  pr.nn <- compute(nn,nn.test[,2:11])
  
  #descale values to get comprehensible result
  pr.nn <- pr.nn$net.result*(max(test.data$price)-min(test.data$price))+min(
    test.data$price)
  test.r <- (nn.test$price)*(max(test.data$price)-min(test.data$price))+min(
    test.data$price)
  
 
  
  #predict user input
  user.nn <- compute(nn,data.to.predict[,2:11])
  user.nn <- user.nn$net.result*(max(test.data$price)-min(test.data$price))+min(
    test.data$price)

  # Calculate error measures
  nn.RMSE <- sqrt(sum((test.r - pr.nn)^2)/nrow(nn.test))
  nn.MAPE <- 100/length(test.r) * sum(abs((test.r - pr.nn)/test.r))
  
  #update averaged error and prediction
  nn.RMSE.avg <- ((i-1)*nn.RMSE.avg + nn.RMSE)/i
  nn.MAPE.avg <- ((i-1)*nn.MAPE.avg + nn.MAPE)/i
  nn.predict.avg <- ((i-1)*nn.predict.avg + user.nn)/i

}

PrintNNResults <- function() {
  print(paste("when trained with", num.to.avg, "models"))
  print(paste("Neural Network MAPE :     ", nn.MAPE.avg))
  print(paste("Neural Network RMSE :     ", nn.RMSE.avg))
  print(paste("Our Neural Network predicts a value of £", 
              round(user.nn*1000)))
}
PrintNNResults()
