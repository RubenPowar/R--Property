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
raw1 <- read.csv("London-price-paid-house-price-data-since-1995-CSV/London\ Yea
                 r_1995-2000.csv",header = TRUE)
raw2 <- read.csv("London-price-paid-house-price-data-since-1995-CSV/London\ Yea
                 r_2001-2006.csv",header = TRUE)
raw3 <- read.csv("London-price-paid-house-price-data-since-1995-CSV/London\ Yea
                 r_2007-2012.csv ",header = TRUE)
raw4 <- read.csv("London-price-paid-house-price-data-since-1995-CSV/London\ Yea
                 r_2013-2014.csv",header = TRUE)
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
#  if (temp1[[i, (which(colnames(temp1)=="post_code"))]] == temp1[[which(colnam
#                                          es(temp1)=="post_code_clean"),i]]) {
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
LRSales.Fixes <- rename(LRSales.95.16, c("V1"="transaction_id", "V2"="price", "V3"
             ="date", "V4"="post_code", "V5"="property_type", "V6"="whether_new"
                        , "V7"="tenure", "V8"="address1", "V9"="address2", "V10"
                        ="address3", "V13"="local_authority"))



#Crime Records - Data.Police ==================================================


#Ingest Crime Records ---------------------------------------------------------

GetNameFromPath <- function(path) {
  
  pos <- regexpr("20", path)
  name <- substr(x, as.integer(p[1]),as.integer(p[1])+6)
}

GetFileNames <- function(main.dir) {

  files <- (list.files(path=main.dir, full.names = TRUE))
  temp.matrix <- matrix(nrow = length(files), ncol = 2)
  for (i in 1:length(files)) {
    temp.string <- as.list(list.files(path=files[i], full.names = TRUE))
    #print(temp.string[1])
    #print(temp.string[2])
    #x1 <- array(temp.string[1], temp.string[2])
    #print(temp.list[[1]][1])
    temp.matrix[[i]][1] <- temp.string[1]
    temp.matrix[[i]][2] <- temp.string[2]
  }
  
  #print(as.list(inner.list[1][1]))
  #print(files)
  return(temp.matrix)
}

MergePair <- function(file.list, which.folder) {

  list.of.dataframes <- list()
  temp1 <- read.csv(toString(file.list[[which.folder]][1]), header=TRUE)
  temp2 <- read.csv(toString(file.list[[which.folder]][2]), header=TRUE)
  temp3 <- rbind(temp1, temp2)
  return(temp3)
}

Compile.months.list <- function(file.list) {
  list.of.months <- list()
  for (n in 1:2) {
    city.met.combined <- MergePair(file.list, n)
    list.of.months <- list(list.of.months, city.met.combined)
  }
  return(city.met.combined)
}

MergeAllFiles <- function(file.list) {
  
  temp1 <- read.csv(toString(file.list[[1]][1]), header=TRUE)
  temp2 <- read.csv(toString(file.list[[1]][2]), header=TRUE)
  total.file <- rbind(temp1, temp2)
  
  for (i in 2:length(police.reports)/2){
    temp1 <- read.csv(toString(file.list[[i]][1]), header=TRUE)
    temp2 <- read.csv(toString(file.list[[i]][2]), header=TRUE)
    total.file <- rbind(total.file, temp1, temp2)
  }
  return(total.file)
}

police.reports <- GetFileNames("Police reports")
file1 <- Compile.months.list(police.reports)
#month.pair <- MergePair(police.reports, 1)
#list.of.months <- list(month.pair)
#list.of.months <- list(list.of.months, month.pair)

#big.file.list <- MergeAllFiles(police.reports)

#Clean Up Records -------------------------------------------------------------

RemoveUnwanted <- function(file) {
  file$Crime.ID <- NULL
  file$Reported.by <- NULL
  file$Location <- NULL
  file$LSOA.code <- NULL
  file$LSOA.name <- NULL
  file$Context <- NULL
  return(file)
  
}

file1 <- RemoveUnwanted(file1)

ReplaceLatLong <- function(file) {
  file$Postcode.sector <- "c0"
  for (i in 1:nrow(file)) {
    long <- file[i, which(colnames(file)=="Longitude")]
    lat <- file[i, which(colnames(file)=="Latitude")]
    col.of.post <- which(colnames(file)=="Postcode.sector")
    temp.string <- paste("api.postcodes.io/postcodes?lon=", toString(long), "&l
                         at=", toString(lat))
    temp.string <- toString(gsub(" ", "", temp.string, fixed = TRUE))
    r <- GET(temp.string)
    postcode <- (content(r)$result[[1]][1])
    #print(temp.string)
    #print(postcode)
    if (!is.null(postcode)) {
      file[i,col.of.post] = postcode
    }
    #If no valid postcode use previous row (file sorted by long, number missing
    #<1% so not disastrous assumption)
    else {
      file[i,col.of.post] = file[i-1,col.of.post]
      } 
  }
    return(file)
}

#r <- GET("api.postcodes.io/postcodes?lon=-0.113767&lat=51.517372")
file2 <- ReplaceLatLong(file1)


#Prepare crimes.data ----------------------------------------------------------


PopulateVector <- function(vec) {
  vec[1] <- "2010-12"
  for (i in 11:16) {
    for (j in 1:12) {
      position.var <- 1 + j + (i-1)*12
      if (j<10) {
        x <- (gsub(" ", "", paste("0", toString(j)), fixed = TRUE))
      }
      if (j>9) {
        x <- toString(j)
      }
      vec[1 + j + (i-11)*12] <- (gsub(" ", "", (paste("20", toString(i), "-", x
      )),  fixed = TRUE))
    }
  }
  return(vec)
}


months.vec <- PopulateVector(months.vec)
m <- matrix(0, ncol = 73, nrow = 1)
crimes.data <- data.frame(m)


for(i in 1:length(crimes.data)) {
  names(crimes.data)[i] <- toString(months.vec[i])
}



#Get Stats From Records--------------------------------------------------------

AreLevelsEqual <- function(data, levels) {
  
}

#Remove last two characters of postcode to return postcode sector
GetPostcodeSector <- function(postcode) {
  return(substr(postcode, 1, nchar(postcode)-2))
}



#Update file containing stats for crimes. month against postcode sector
AddToCrimes <- function(post, mont, type, crimes) {
  #print("BEGINNING OF ADD TO CRIMES ::::::::::::")
  #print(crimes[, 1:3])
  #print(crimes.data[, 1:3])

  #types : s = 1, t = 2, o = 0
  #months : starting with 2010-12 to 2016-12 (6 x 12 + 1 = 73)
  #postcodes : 
  
  #Find cell to put data in
  col.of.month <- which(colnames(crimes) == mont)
  tryCatch({row.of.post <- which(row.names(crimes) == post)}, error=function(
    e) {})
  
  #Get values from crimes.data cell as a vector
  if (length(row.of.post) == 0) {
    row.of.post <- 0
  }
  #temp.vector <- unlist(strsplit(crimes.data[[row.of.post]][col.of.month], split=" "))
  
  #If postcode doesnt exist in crimes.data add new row named after postcode
  if (row.of.post == 0) {
    crimes <- rbind(crimes, vector(mode="character", length=73))
    row.names(crimes)[nrow(crimes)] <- post
    row.of.post <- nrow(crimes)
  }
  #Get values from crimes.data cell as a vector
  temp.string <- toString(crimes[row.of.post, col.of.month])
  temp.vector <- unlist(strsplit(temp.string, split=" "))

  #If vector is not (x, y, z), i.e. (x, y), correct to (x, y, 0)
  if (length(temp.vector) < 3) {
    for (i in 1:3)
      if (is.na(temp.vector[i])) {
        temp.vector[i] <- 0
      }
  }
  #print(crimes[, 1:3])
  
  #Increment category and update crimes.data
  temp.vector[type] <- strtoi(temp.vector[type]) + 1
  crimes[row.of.post, col.of.month] <- paste(temp.vector, collapse=" ")
  #print(row.of.post)
  #print(col.of.month)
  #print(crimes[, 1:3])
  
  return(crimes)
}


#Get relevant info from each crime and pass to be added to file
CalculateStats <- function(data) {
  social <- c("Anti-social behaviour", "public order")
  theft <- c("Theft from the person", "Burglary", "Other theft", "Bicycle theft
             ", "Shoplifting", "Vehicle crime", "Robbery")
  other <- c("Criminal damage and arson", "  Violence and sexual offences", "Dr
             ugs", "Possession of weapons", "Other crime", "Violent crime")
  notinc <- vector(mode="character", length=10)
  temp.int <- 1
  all.levels <- c(social, theft, other)
  data.levels <- (levels(data$Crime.type))
  #AreLevelsEqual(data.levels, all.levels)
  
  mon <- toString(data[1, which(colnames(data)=="Month")])
  s <- 0
  t <- 0
  o <- 0
  for (i in 1:nrow(data)) {
    
    post <- data[i, which(colnames(data)=="Post")]
    postcode.sector <- GetPostcodeSector(post)
    if (data[i, which(colnames(data)=="Crime.type")] %in% social) {
      s <- s+1
      typ <- 1
    }
    if (data[i, which(colnames(data)=="Crime.type")] %in% theft) {
      t <- t+1
      typ <- 2
    }
    if (data[i, which(colnames(data)=="Crime.type")] %in% other) {
      o <- o+1
      typ <- 3
    }
    x <- x+1
    #print(crimes.data[, 1:3])
    
    #print("BEFORE ADDTOCRIMES IS CALLED :::::")
    #print(crimes.data[, 1:3])
    
    crimes.data <- AddToCrimes(postcode.sector, mon, typ, crimes.data)
    
    
#    else {
#      if (!(data[i, which(colnames(data)=="Crime.type")] %in% notinc)) {
#        #notinc[temp.int] <- data[i, which(colnames(data)=="Crime.type")]
#        temp.int <- temp.int + 1;
#      }
#    }
  }
  sto <- c(mon, s, t, o)
  #print(notinc)
  return(crimes.data)
}

crimes.data <- CalculateStats(file2)





