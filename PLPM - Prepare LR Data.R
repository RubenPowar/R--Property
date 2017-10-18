#Prepare Land Registry Data for input into regression algorithms===============


#read data and remove unnecessary attributes
LR1 <- read.csv("price data/LR1.csv")    # 1994 - 2000
LR2 <- read.csv("price data/LR2.csv")    # 2001 - 2006
LR3 <- read.csv("price data/LR3.csv")    # 2007 - 2012
LR4 <- read.csv("price data/LR4.csv")    # 2013 - 2014
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
colnames(LR.mod2)[which(names(LR.mod2) == "V28")] <- "Postcode.sector"


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

#Change variables to continuous or factors
LR.mod$tenure <- factor(as.numeric(LR.mod$tenure))
LR.mod$record_status <- factor(as.numeric(LR.mod$record_status))
LR.mod$borough_name <- factor(as.numeric(LR.mod$borough_name))
LR.mod$whether_new <- factor(as.numeric(LR.mod$whether_new))
LR.mod$town <- factor(as.numeric(LR.mod$town))
LR.mod$year_month <- as.numeric(LR.mod$year_month)
LR.mod$property_type <- factor(as.numeric(LR.mod$property_type))
LR.mod$year <- as.numeric(LR.mod$year)

#Price rounded to nearest thousand, then divided by 1000
LR.mod$price <- round(LR.mod$price, -3)/1000

LR.mod$price <- as.numeric(LR.mod$price)

#remove further unwanted attributes and change names
LR.mod$X.1 <- NULL
LR.mod$post_code <- NULL
colnames(LR.mod2)[which(names(LR.mod2) == "V28")] <- "Postcode.sector"




#Final validation steps========================================================

#Check data is complete
completeness <- apply(LR.mod,2,function(x) sum(is.na(x)))
zero.vec <- rep(0, length(completeness))
if (all(completeness==zero.vec)) {
  print(paste("Is Data Complete?     YES"))
} else {
  print(paste("Is Data Complete?     NO!"))
  print(completeness)
}

#Now we know data is complete

#Normalise data between 0 and 1
LR.mod$price <- ((LR.mod$price - min(LR.mod$price)) / 
                   (max(LR.mod$price) - min(LR.mod$price)))







