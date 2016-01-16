brent.csv<-read.csv("~/Desktop/R/Oil.csv",header=TRUE)
brentindex<-brent.csv[,2:3]
plot(brentindex)
abline(lm(brentindex$Price ~ brentindex$Index))
reg1 <- lm(Price ~ Index, data = brentindex)
predict(reg1)
summary(reg1)
# The regression is based on 5 years of data.
#Slope: 0.12929, Intercept: 73.8931
# y=kx+d, y=0.12929x+73.8931, where y=price and x=index
# Index max is 318, for 318 week between 1st of Jan 2009 until today; 1 week=1 index point
# we add 47 index points to get the estimate for the end of 2015 and 99 for the end of 2016
coefficients(reg1)
f<-function(x){0.1292922*x+73.8930962}
f(365)
# So, the end of year 2015 estimate is at USD 121.0847, based on 5 years of data.
f(417)
# The estimate of the end of year 2016 is at USD 127.8079, based on 5 years of data.

brent1.csv<-read.csv("~/Desktop/R/Oil1.csv",header=TRUE)
brentindex1<-brent1.csv[,2:3]
plot(brentindex1)
abline(lm(brentindex1$Price ~ brentindex1$Index))
reg2 <- lm(Price ~ Index, data = brentindex1)
predict(reg2)
summary(reg2)
coefficients(reg2)
f1<-function(x){-1.010182*x+124.347387}
f1(104)
# we add 47 index points to get the estimate for the end of 2015 and 99 for the end of 2016
# So, the end of year 2015 estimate is at USD 19.228846, based on 1 year of data.
f1(156)
# The estimate of the end of year 2016 is at USD -33.24101, based on 1 year of data, which is not sensible.
# Therefore we try with 2 years of historical data instead of one.

brent2.csv<-read.csv("~/Desktop/R/Oil2.csv",header=TRUE)
brentindex2<-brent2.csv[,2:3]
plot(brentindex2,xlab = "Number of Weeks", main = "Oil Price with 2 years Data")
abline(lm(brentindex2$Price ~ brentindex2$Index))
reg3 <- lm(Price ~ Index, data = brentindex2)
predict(reg3)
summary(reg3)
coefficients(reg3)
f2<-function(x){-0.3318072*x+119.7749949}
f2(156)
# So, the end of year 2015 estimate is at USD 68.01307, based on 2 years of data.
f2(208)
# So, the end of year 2016 estimate is at USD 50.7591, based on 2 years of data.

brent5.csv<-read.csv("~/Desktop/R/SolarCity.csv",header=TRUE)
brentindex5<-brent5.csv[,3:4]
cor(brentindex5)
plot(brentindex5)
brentindex6<-brent5.csv[,2:3]
plot(brentindex6)

brent0.csv<-read.csv("~/Desktop/R/GasCrude.csv",header=TRUE)
brentindex11<-brent0.csv[,3:4]
cor(brentindex11)
plot(brentindex01)
electricity.csv<-read.csv("~/Desktop/R/MainModel.csv",header=TRUE)
revenue<-electricity.csv[,1:2]
qplot(Index,Revenue,data=revenue,geom=c("point", "smooth"), method=lm)
rev1 <- lm(Revenue ~ Index, data = revenue)
predict(rev1)
summary(rev1)
coefficients(rev1)
f5<-function(x){3555.944*x-1348.136}
f5(17)
# So, the end of year 2015 estimate is at USD 59102.91, based on 12 quarters of revenues in leasing.
f5(21)
# So, the end of year 2016 estimate is at USD 73326.69, based on 12 quarters of revenues in leasing until 3rd Q 2014.

hybrid.csv<-read.csv("~/Desktop/R/HybridSales.csv",header=TRUE)
qplot(Year,US.Hybrid.Vehicles.Sales,data=hybrid.csv,geom=c("point", "smooth"), method=lm, ylab="Sales", main="US Hybrid Vehicles Sales from 1999-2012")
US1 <- lm(US.Hybrid.Vehicles.Sales ~ Year, data = hybrid.csv)
coefficients(US1)
f01<-function(x){32405.3*x-64805270.2}
f01(2015)
f01(2016)
predict(US1)

correlation.csv<-read.csv("~/Desktop/R/Correlation.csv",header=TRUE)
correlation<-correlation.csv[,3:4]
qplot(Electricity,Revenue,data=correlation,geom=c("point", "smooth"), method=lm)
rev1 <- lm(Revenue ~ Index, data = revenue)
cor(correlation)
