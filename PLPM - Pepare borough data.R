#This script prepares all the data for 


m <- matrix(0, ncol = 12, nrow = 2)
boroughs <- data.frame(m)
colnames(boroughs) <- c("price","borough", "year", "month", "population", 
                        "child.pov", "income", "JSA", "living.wage", "crime.s",
                        "crime.t", "crime.o")

JSA <- read.csv("London Data/to use/jobseekers.allowance.csv",header = TRUE)
POP <- read.csv("London Data/to use/Borough MYE 1981-2015.csv",header = TRUE)
INC <- read.csv("London Data/to use/income-of-tax-payers.csv",header = FALSE)
POV <- read.csv("London Data/to use/child.poverty2.csv",header = FALSE)
LLW <- read.csv("London Data/to use/london.living.wage2.csv",header = FALSE)
CRS <- read.csv("London Data/to use/crimess.csv",header = FALSE)
CRT <- read.csv("London Data/to use/crimest.csv",header = FALSE)
CRO <- read.csv("London Data/to use/crimeso.csv",header = FALSE)




#populate JSA and borough

for (i in 1:33) {
  for (j in 1:64) {
    #boroughs[72*(i-1)+j,(which(colnames(boroughs) == "JSA"))] <- toString(JSA[i,2+j])
    #boroughs[72*(i-1)+j,(which(colnames(boroughs) == "borough"))] <- toString(JSA[i,2])
    #boroughs[72*(i-1)+j,(which(colnames(boroughs) == "population"))] <- toString(POP[i,20+ceiling(j/4)])
    #boroughs[72*(i-1)+j,(which(colnames(boroughs) == "income"))] <- toString(INC[1+i,1+2*(ceiling(j/4))])
    #boroughs[72*(i-1)+j,(which(colnames(boroughs) == "child.pov"))] <- toString(POV[3+i,1+2*(ceiling(j/4))])
    #boroughs[72*(i-1)+j,(which(colnames(boroughs) == "living.wage"))] <- toString(LLW[2+i,2+(ceiling(j/4))])
    #boroughs[72*(i-1)+j,(which(colnames(boroughs) == "price"))] <- avg.prices[i,j]
    #boroughs[72*(i-1)+j,(which(colnames(boroughs) == "crime.s"))] <- CRS[i,j]
    #boroughs[72*(i-1)+j,(which(colnames(boroughs) == "crime.t"))] <- CRT[i,j]
    #boroughs[72*(i-1)+j,(which(colnames(boroughs) == "crime.o"))] <- CRO[i,j]
  }
}

#populate year
for (k in 1:33)
for (i in 1:18) {
  for (j in 1:4) {
    if (!(i==18 && j==4)) {
      boroughs[72*(k-1) +4*(i-1) + j, 3] <- 1999 + i-1
    }
  }
}
#populate month
for (i in 1:nrow(boroughs)) {
  boroughs[i,4] <- (i)%%4
  if (i%%4 == 0) {
    boroughs[i,4] <- 4
  }
    
}

temp.word <- toString(POP[3,])
MYE2 <- rbind(temp.word,temp.word,temp.word,temp.word)






write.csv(boroughs, file = "London Data/to use/boroughs-price.csv")

mat <- matrix(0, ncol = 64, nrow = 33)
avg.prices <- data.frame(mat)

#loop through data to gather means
for (i in 9:9) {
  temp.df <- split(LR.mod,LR.mod$borough_name)[[8]]
  for (j in 5:20) {
    temp.ddf <- split(temp.df, temp.df$year)[[20]]
    
    mean1.temp <- mean(split(temp.ddf, temp.ddf$quarter)[[1]]$price)
    avg.prices[i,(j-5)*4+1] <- mean1.temp

    mean2.temp <- mean(split(temp.ddf, temp.ddf$quarter)[[2]]$price)
    avg.prices[i,(j-5)*4+2] <- mean2.temp

    mean3.temp <- mean(split(temp.ddf, temp.ddf$quarter)[[3]]$price)
    avg.prices[i,(j-5)*4+3] <- mean3.temp

    mean4.temp <- mean(split(temp.ddf, temp.ddf$quarter)[[4]]$price)
    avg.prices[i,(j-5)*4+4] <- mean4.temp

  }
}

write.csv(boroughs, file = "London Data/to use/boroughs+price.csv")
write.csv(avg.prices, file = "London Data/to use/avg.price.csv")



boroughs <- read.csv("London Data/to use/boroughs+price.csv",header = TRUE)











