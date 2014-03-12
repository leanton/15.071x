poll = read.csv("AnonymityPoll.csv")

str(poll)
table(poll$Smartphone)

table(poll$Sex, poll$Region)

# PS3, #2

nonetsmart <- subset(ap,ap$Internet.Use=="0"&ap$Smartphone=="0")
nrow(nonetsmart)
nonetsmart <- subset(ap,ap$Internet.Use=="1"&ap$Smartphone=="1")
nrow(nonetsmart)
nonetsmart <- subset(ap,ap$Internet.Use=="1"&ap$Smartphone=="0")
nrow(nonetsmart)
nonetsmart <- subset(ap,ap$Internet.Use=="0"&ap$Smartphone=="1")
nrow(nonetsmart)

summary(ap)

limited <- subset(ap,ap$Internet.Use=="1"|ap$Smartphone=="1")
nrow(limited)

summary(limited)

mean(limited$Info.On.Internet)

table(limited$Info.On.Internet)

table(limited$Worry.About.Info)