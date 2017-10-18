#Copyright ...

#Data.Police ...
#Data produced by Land Registry © Crown copyright 2016.
#London Datastore ...

#Author : Ruben Powar
#Title : Predicting the London Property Market

install.packages('httr')
library(httr)
install.packages('plyr')
library(plyr)
install.packages('fitdistrplus')
library(fitdistrplus)
install.packages('logspline')
library(logspline)

#House Price Records - Land Registry ==========================================

#Load raw house price data
raw1 <- read.csv(
  "London-price-paid-house-price-data-since-1995-CSV/London\ Year_1995-2000.csv"
  ,header = TRUE)
raw2 <- read.csv(
  "London-price-paid-house-price-data-since-1995-CSV/London\ Year_2001-2006.csv"
  ,header = TRUE)
raw3 <- read.csv(
  "London-price-paid-house-price-data-since-1995-CSV/London\ Year_2007-2012.csv"
  ,header = TRUE)
raw4 <- read.csv(
  "London-price-paid-house-price-data-since-1995-CSV/London\ Year_2013-2014.csv"
  ,header = TRUE)
pp.complete <- read.csv("pp-complete.csv", header = TRUE)


#Reorder columns alphabetically
ordered1 <- raw1[ , order(names(raw1))]
ordered2 <- raw2[ , order(names(raw2))]
ordered3 <- raw3[ , order(names(raw3))]
ordered4 <- raw4[ , order(names(raw4))]

#save ordered locally
write.csv(ordered1, file = "Saved Data/ordered1.csv")
write.csv(ordered2, file = "Saved Data/ordered2.csv")
write.csv(ordered3, file = "Saved Data/ordered3.csv")
write.csv(ordered4, file = "Saved Data/ordered4.csv")

#Make ordered1-3 and ordered4 compatible

#Remove fields from ordered1-3

ordered1$id <- NULL
ordered2$id <- NULL      #Not relevant to model
ordered3$id <- NULL

ordered1$post_code_clean <- NULL
ordered2$post_code_clean <- NULL 
ordered3$post_code_clean <- NULL

#Code to show post_code_clean == post_code
#n <- 0
#temp1 <- ordered1
#for(i in seq_along(length(temp1))) 
#  if (temp1[[i, (which(colnames(temp1)=="post_code"))]] == temp1[[which(
#    colnames(temp1)=="post_code_clean"),i]]) {
#    n <- n+1
#    #x <-ordered1[[i,16]]
#  }
#n <- temp1[[(which(colnames(temp1)=="post_code")),1]]
#x

ordered1$ward_name <- NULL
ordered2$ward_name <- NULL      #Can be derived from ward_code
ordered3$ward_name <- NULL

#Remove fields from ordered4
ordered4$postcode <- NULL
ordered4$Postcode_district <- NULL
ordered4$Postcode_sector <- NULL
ordered4$Ward14 <- NULL      #questionable value
ordered4$statsward <- NULL      #No extra info on ward needed
ordered4$house_flat <- NULL      #Can be derived from Property_type

#Add fields to ordered4
ordered4$address1 <- "c0"
ordered4$address3 <- "c0"

#Rename fields in ordered4
names(ordered4)[which(colnames(ordered4)=="whether_newbuild")] <- "whether_new"

#Reorder columns alphabetically for merge
ordered1 <- ordered1[ , order(names(ordered1))]
ordered2 <- ordered2[ , order(names(ordered2))]
ordered3 <- ordered3[ , order(names(ordered3))]
ordered4 <- ordered4[ , order(names(ordered4))]

#Combine Data
LRSales_95_14 <- rbind(ordered1, ordered2, ordered3, ordered4)




#Save to this point
write.csv(ordered1, file = "Saved Data/ordered1.csv")
write.csv(ordered2, file = "Saved Data/ordered2.csv")
write.csv(ordered3, file = "Saved Data/ordered3.csv")
write.csv(ordered4, file = "Saved Data/ordered4.csv")
write.csv(LRSales_95_14, file = "Saved Data/combined_95_14.csv")

#Load saved data ^^
ordered1 <- read.csv("Saved Data/ordered1.csv",header = TRUE)
ordered2 <- read.csv("Saved Data/ordered2.csv",header = TRUE)
ordered3 <- read.csv("Saved Data/ordered3.csv",header = TRUE)
ordered4 <- read.csv("Saved Data/ordered4.csv",header = TRUE)

LRSales_95_14 <- read.csv("Saved Data/combined_95_14.csv")
LRSales.95.16 <- read.csv("pp-complete.csv", header = FALSE)
LRSales.Fixes <- rename(LRSales.95.16, 
                        c("V1"="transaction_id", "V2"="price","V3"="date", 
                          "V4"="post_code","V5"="property_type",
                          "V6"="whether_new","V7"="tenure", "V8"="address1",
                          "V9"="address2","V10"="address3",
                          "V13"="local_authority"))


#Output of this sction of code saved to LR1-LR4