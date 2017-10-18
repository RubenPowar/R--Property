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

AddWanted <- function(file) {
file$Postcode.sector <- "c0"
}

ReplaceLatLong <- function(file) {
for (i in as.numeric(1:nrow(file))) {
  long <- file[i, as.numeric(which(colnames(file)=="Longitude"))]
  lat <- file[i, as.numeric(which(colnames(file)=="Latitude"))]
  col.of.post <- as.numeric(which(colnames(file)=="Postcode.sector"))
  temp.string <- paste("api.postcodes.io/postcodes?lon=", toString(long), 
                       "&lat=", toString(lat))
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

CleanRecords <- function(file) {
file <- RemoveUnwanted(file)
file <- AddWanted(file)
file <- ReplaceLatLong(file)
}

temp.months <- CleanRecords(temp.months)

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
theft <- c("Theft from the person", "Burglary", "Other theft", "Bicycle theft",
           "Shoplifting", "Vehicle crime", "Robbery")
other <- c("Criminal damage and arson", "  Violence and sexual offences",
           "Drugs", "Possession of weapons", "Other crime", "Violent crime")
notinc <- vector(mode="character", length=10)
temp.int <- 1
all.levels <- c(social, theft, other)
data.levels <- (levels(data$Crime.type))
#AreLevelsEqual(data.levels, all.levels)

mon <- toString(data[1, which(colnames(data)=="Month")])
s <- 0
t <- 0
o <- 0
x <- 0
for (i in 1:nrow(data)) {
  
  post <- data[i, which(colnames(data)=="Postcode.sector")]
  postcode.sector <- GetPostcodeSector(post)
  mon <- toString(data[i, which(colnames(data)=="Month")])
  
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
  print(postcode.sector)
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


#Replace temp.months with crimes month you want to process
crimes.data <- CalculateStats(temp.months)

#==============================================================================



