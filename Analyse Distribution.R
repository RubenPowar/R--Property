#Investigate distributions

install.packages('httr')
library(httr)
install.packages('plyr')
library(plyr)
install.packages('fitdistrplus')
library(fitdistrplus)
install.packages('logspline')
library(logspline)

#Load saved data ^^
ordered1 <- read.csv("Saved Data/ordered1.csv",header = TRUE)
ordered2 <- read.csv("Saved Data/ordered2.csv",header = TRUE)
ordered3 <- read.csv("Saved Data/ordered3.csv",header = TRUE)
ordered4 <- read.csv("Saved Data/ordered4.csv",header = TRUE)

#getprices from ordered 1 and 4 to nearest £1000i

prices3 <- round(ordered3$price, -3)/1000
prices4 <- round(ordered4$price, -3)/1000

#plot histograms of prices

hist(prices1,xlim=c(0,500), breaks=10000)
hist(prices3,xlim=c(0,1000), breaks=10000)

descdist(prices4, discrete = FALSE)

#analyse distribution - Cullen and Frey
descdist(prices3, discrete = FALSE)

#remove bottom and top 1% of values in prices3
prices3 <- sort(prices3)

prices3.trim <- prices3[(length(prices3)/100):(length(prices3)-(length(prices3)/100))]

#compare range of prices3 and prices3.trim
print(paste(min(prices3), "-", max(prices3), "and", min(prices3.trim), "-", max(prices3.trim)))
#compare averages
print(paste(mean(prices3), "and", mean(prices3.trim)))

#normalise between 1 and 0
prices3.trim.n = (prices3.trim-min(prices3.trim))/(max(prices3.trim)-min(prices3.trim))

#plot histrogram of reduced normalised data
hist(prices3.trim.n,xlim=c(0,1), breaks=10000)

#analyse new Cullen and Frey (we see kurtosis and skewness massively drop)
descdist(prices3.trim.n, discrete = FALSE)

#could be beta or gamma distribution
fit.weibull <- fitdist(prices3.trim, "weibull") #close to gamma
fit.norm <- fitdist(prices3.trim.n, "norm")
fit.beta <- fitdist(prices3.trim.n, "beta")

#plot distribution graphs
plot(fit.norm)
plot(fit.weibull)




