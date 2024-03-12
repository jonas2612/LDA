rm(list = ls())

library(readxl)
toenail <- read_excel("toenail_with_missings.xls", na = "NA")
save(toenail, file = "toenail.RDATA")


load("toenail.RDATA")

summary(toenail)
head(toenail)
tail(toenail)
colSums(is.na(toenail))  # age is missing for one person


toenail$gender <- factor(toenail$gender, labels = c("female", "male"))
toenail$health_club <- factor(toenail$health_club,
                              labels = c("Once or less", "Twice or more"))
toenail$nl <- toenail$Unafflicted_nail_length_with_NA



###############
# Gender
###############


colSums(with(toenail, table(id,gender)) !=0)
# 105 of both genders
firstvisits <- toenail[toenail$month ==0,]

# relationship with nl  - no difference
with(toenail, boxplot(nl ~ gender))
with(firstvisits, boxplot(nl ~ gender))
with(firstvisits, tapply(nl,gender,mean,na.rm = TRUE))
with(toenail, tapply(nl,gender,mean,na.rm = TRUE))



prop.table(with(toenail, table(health_club,gender )))
prop.table(with(toenail, table(treatment,gender )))
with(firstvisits, boxplot(age ~ gender))

###############
# age
###############

hist(firstvisits$age)
summary(firstvisits$age)

cor(toenail$age,toenail$month,use = "complete.obs" )

sum(rowSums(table(toenail$id,toenail$age) != 0)!=1)
#one person has no age, so no change in age
which(rowSums(table(toenail$id,toenail$age) != 0) == 0)
toenail[565:570,]

#nl 
with(firstvisits, plot(nl~age))
with(toenail, plot(nl~age))
cor(toenail$age,toenail$nl,use = "complete.obs" )


###############
# health_club
###############
table(firstvisits$health_club)


sum(rowSums(table(toenail$id,toenail$health_club) != 0)!=1) # no change


#nl
with(firstvisits, plot(nl~health_club))
with(toenail, plot(nl~health_club))


with(firstvisits, boxplot(age ~ health_club))


###############
# month
###############

with(toenail, plot(nl~month))
lines(loess(toenail$nl~toenail$month), col=2) #:(


###############
# trt
###############

# relationship with nl  - no difference
with(toenail, boxplot(nl ~ treatment))
with(firstvisits, boxplot(nl ~ treatment))
with(firstvisits, tapply(nl,treatment,mean,na.rm = TRUE))
with(toenail, tapply(nl,treatment,mean,na.rm = TRUE))







