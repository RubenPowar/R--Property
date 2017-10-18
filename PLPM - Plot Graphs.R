install.packages("ggplot2") 
library(ggplot2)

#Get Statistics


#get whole london mean prices per month
ave.prices.lon <- vector()
for (i in 1:72) {
  ave.prices.lon[i] <- mean(split(LR.mod, LR.mod$year_month)[[i]]$price)
}







#Plot Graphs

#Jobseekers allowance, raw Barking and Dagenham vs Kingston upon Thames
JSA <- read.csv("London Data/to use/jobseekers.allowance.csv")
x1 <- vector()
x2 <- vector()
for(i in 3:71) {
  x1 <- c(x1,as.integer(gsub(",", "", toString(JSA[2,i]))))
  x2 <- c(x2,as.integer(gsub(",", "", toString(JSA[21,i]))))
}
plot.new()
plot.window(xlim=range(1:69),ylim=range(1:8000))
lines(x2, type="l",col="blue")
lines(x1, type="l",col="red")
axis(1); axis(2); box();
title(main="Jobseekers Allowance", xlab="quarter (1999-2016)", 
      ylab="number claiming allowance")
legend('topleft', legend = c("Barking and Dagenham", "Kingston upon Thames"), 
       col=c("red", "blue"), lty=1, bty='n', cex=.75)



months <- 1:72
geom_smooth(ints, ave.prices.lon)
plot(months, ave.prices.lon)
ggplot(data = LR.mod, aes(year_month, price))
