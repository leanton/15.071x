IBM = read.csv("IBMStock.csv")
GE = read.csv("GEStock.csv")
ProcterGamble = read.csv("ProcterGambleStock.csv")
CocaCola = read.csv("CocaColaStock.csv")
Boeing = read.csv("BoeingStock.csv")

# Replacing date to R format

IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
GE$Date = as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")

# Commands prompted to answer PS questions

str(CocaCola)
summary(IBM)
summary(GE)
summary(CocaCola)
summary(Boeing)
sd(ProcterGamble$StockPrice)
plot(CocaCola$Date, CocaCola$StockPrice, type="l")
lines(ProcterGamble$Date, ProcterGamble$StockPrice)

plot(CocaCola$Date, CocaCola$StockPrice, type="l", col="red")
lines(ProcterGamble$Date, ProcterGamble$StockPrice, col="blue")
abline(v=as.Date(c("2000-03-01")), lwd=2)


# PS2, 3.1
plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
lines(GE$Date[301:432], GE$StockPrice[301:432], type="l", col="blue")
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], type="l", col="black")
lines(IBM$Date[301:432], IBM$StockPrice[301:432], type="l", col="orange")
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], type="l", col="green")
abline(v=as.Date(c("2000-03-01")), lwd=2)

# PS2, 4
tapply(IBM$StockPrice, months(IBM$Date), mean)
mean(IBM$StockPrice)

tapply(GE$StockPrice, months(GE$Date), mean)
tapply(CocaCola$StockPrice, months(CocaCola$Date), mean)