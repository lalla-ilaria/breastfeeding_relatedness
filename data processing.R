library(truncnorm)
#data processing

Feeding<-read.csv("modelwithr.csv",header=TRUE)
sex<-as.factor(Feeding$Csex)
# SES<-factor(Feeding$SES)
# SES2<- relevel(SES, ref = "Low")
agestop<-as.numeric(Feeding$time1)

ITID<-as.factor(Feeding$ITID)
Village<-as.factor(Feeding$Village)
District<-as.factor(Feeding$District)
PMR<-as.factor(Feeding$PMR)
PMR1<- relevel(PMR, ref = "Neo")
age <- ifelse(Feeding$BirthPeriod == ">=1990", Feeding$agecohort, ifelse(Feeding$BirthPeriod == "1980-1989", "25-30", ">30" ))
age<-as.factor(age)
#age <- as.factor(Feeding$agecohort)
sex<-as.factor(Feeding$Csex)
kid<- as.numeric(Feeding$kids)
mom<- as.numeric(Feeding$mother)
father<- as.numeric(Feeding$father)
order<- as.factor(Feeding$order)
BirthPeriod<-as.factor(Feeding$BirthPeriod)
status<-as.factor(Feeding$status)
size<-as.factor(Feeding$HSCAT)


#only child model

#number of children in the sample with real data

N <- 558

household <- sample(1:4, N, replace = TRUE)

#1 duolocal

#2 matrilocal

#3 neolocal

#4 patrilocal

#simulate relatedness

mom_rel <- ifelse(household %in% c(1), rtruncnorm(n = N, a = 0.07, b = 0.50, mean = 0.40, sd = 0.09),
            ifelse(household %in% c(2),rtruncnorm(n = N, a = 0.08, b = 0.55, mean = 0.36, sd = 0.09),
              ifelse(household %in% c(3),rtruncnorm(n = N, a = 0.09, b = 0.50, mean = 0.30, sd = 0.10),
                rtruncnorm(n = N, a = 0.05, b = 0.50,mean = 0.22, sd = 0.09))))



dad_rel <- ifelse(household %in% c(1), rtruncnorm(n = N, a = 0.00, b = 0.43, mean = 0.06, sd = 0.12),
             ifelse(household %in% c(2),rtruncnorm(n = N, a = 0.00, b = 0.44, mean = 0.22, sd = 0.11),
               ifelse(household %in% c(3),rtruncnorm(n = N, a = 0.00, b = 0.50, mean = 0.30, sd = 0.13),
                 rtruncnorm(n = N, a = 0.25, b = 0.65, mean = 0.38, sd = 0.08))))



child_rel <- ifelse(household %in% c(1), rtruncnorm(n = N, a = 0.16, b = 0.50, mean = 0.31, sd = 0.08),
                 ifelse(household %in% c(2),rtruncnorm(n = N, a = 0.16, b = 0.55, mean = 0.38, sd = 0.09),
                     ifelse(household %in% c(3),rtruncnorm(n = N, a = 0.17, b = 0.50, mean = 0.40, sd = 0.10),
                        rtruncnorm(n = N, a = 0.13, b = 0.50, mean = 0.38, sd = 0.08))))
hist(child_rel)
hist((mom_rel + dad_rel)/2 + 0.1, add = TRUE, col = col.alpha("blue", 0.8))
