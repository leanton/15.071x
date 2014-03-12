mvt = read.csv("mvtWeek1.csv")
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)
mvt$Date = DateConvert

hist(mvt$Date, breaks=100)
boxplot(mvt$Date ~ mvt$Arrest)
sort(table(mvt$LocationDescription))
Top5 = subset(mvt, mvt$LocationDescription == "DRIVEWAY - RESIDENTIAL" | mvt$LocationDescription == "GAS STATION" | mvt$LocationDescription == "ALLEY" | mvt$LocationDescription == "PARKING LOT/GARAGE(NON.RESID.)" | mvt$LocationDescription == "STREET")
table(Top5$LocationDescription, Top5$Weekday)