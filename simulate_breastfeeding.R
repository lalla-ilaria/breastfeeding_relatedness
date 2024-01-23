
#simulate breastfeeding data
sim_breastfeeding <- function (N, relatedness_avg = relatedness_averages, poiss_interc, poiss_child, poiss_mom, poiss_dad, censor = FALSE ){
  #generate household types and assign color
  household <- sample(1:4, N, replace = TRUE)
  col_hh <- ifelse(household == 1, "lightgreen", ifelse(household == 2, "coral", ifelse(household == 3, "goldenrod", "lightblue")))
  
  #generate relatednesses 
  mom_rel <- ifelse(household == 1, rtruncnorm(n = N, a = 0.07, b = 0.50, mean = relatedness_avg$mom[1], sd = 0.09),
                    ifelse(household == 2,rtruncnorm(n = N, a = 0.08, b = 0.55, mean = relatedness_avg$mom[2], sd = 0.09),
                           ifelse(household == 3,rtruncnorm(n = N, a = 0.09, b = 0.50, mean = relatedness_avg$mom[3], sd = 0.10),
                                  rtruncnorm(n = N, a = 0.05, b = 0.50,mean = relatedness_avg$mom[4], sd = 0.09))))
  
  
  dad_rel <- ifelse(household == 1, rtruncnorm(n = N, a = 0.00, b = 0.43, mean = relatedness_avg$dad[1], sd = 0.12),
              ifelse(household == 2,rtruncnorm(n = N, a = 0.00, b = 0.44, mean = relatedness_avg$dad[2], sd = 0.11),
               ifelse(household == 3,rtruncnorm(n = N, a = 0.00, b = 0.50, mean = relatedness_avg$dad[3], sd = 0.13),
                                  rtruncnorm(n = N, a = 0.25, b = 0.65, mean = relatedness_avg$dad[4], sd = 0.08))))
  
  child_rel <- (mom_rel + dad_rel)/2 + 0.1
  
  #generate duration of breastfeeding from poisson distribution
  poiss_rate <- exp( rnorm(N, poiss_interc, 0.2) + child_rel * poiss_child + mom_rel * poiss_mom + dad_rel * poiss_dad)

  duration <- rpois(N, poiss_rate) 

  #censor to twelve months
  if(censor == TRUE){
    duration <- ifelse(duration >= 12, 12, duration)
  }#if censor

  #generate status (i.e. whether data is censored)
  if(censor == FALSE) {
    status <-  rep(1, N)} else {
      ifelse(duration > 12, 0, 1) }

  #prepare data
  breastfeeding_data <- list(N = N,
                             household = household,
                             col_hh = col_hh,
                             mom_rel = mom_rel,
                             dad_rel = dad_rel,
                             child_rel = child_rel,
                             poiss_rate = poiss_rate,
                             duration = duration,
                             status = status)
  return(breastfeeding_data)
}

relatedness_averages <- data.frame(mom = c(0.4, 0.36, 0.3, 0.22 ),
                              dad = c(0.06, 0.22, 0.3, 0.38))

bf <- sim_breastfeeding(N = 558, poiss_interc = -2.5, poiss_child = 10, poiss_mom = 5, poiss_dad = -5 )


child_sim <- coxph(Surv(bf$duration, bf$status) ~ bf$child_rel) 
mother_sim <- coxph(Surv(bf$duration, bf$status) ~ bf$mom_rel)
father_sim <- coxph(Surv(bf$duration, bf$status) ~ bf$dad_rel)
parents_sim <- coxph(Surv(bf$duration, bf$status) ~ bf$mom_rel + bf$dad_rel)
all_sim <- coxph(Surv(bf$duration, bf$status) ~ bf$child_rel + bf$mom_rel + bf$dad_rel)
#residence_sim <- coxph(Surv(bf$duration, bf$status) ~ strata(as.factor(bf$household)))

#####real data models
Feeding <-read.csv("modelwithr.csv",header=TRUE)

#TO DO: check the data processing 
sex<-as.factor(Feeding$Csex)
ITID<-as.factor(Feeding$ITID)
Village<-as.factor(Feeding$Village)
District<-as.factor(Feeding$District)
PMR<-as.factor(Feeding$PMR)
PMR1<- relevel(PMR, ref = "Neo")
age<-as.factor(Feeding$agecohort)
kid<- as.numeric(Feeding$kids)
mom<- as.numeric(Feeding$mother)
father<- as.numeric(Feeding$father)
order<- as.factor(Feeding$order)
BirthPeriod<-as.factor(Feeding$BirthPeriod)
time<-as.numeric(Feeding$time1)
status<-as.factor(Feeding$status)
size<-as.factor(Feeding$HSCAT)
status <- as.factor(Feeding$status)
residence_real<-coxme(Surv(time,status)~sex+order+size+ age+BirthPeriod+PMR1+(1|Village/ITID)
                      ,data = Feeding) 
mother_real<-coxme(Surv(time,status)~mom+sex+order+size+age+BirthPeriod+(1|Village/ITID)
                   ,data = Feeding) 

child_real<-coxme(Surv(time,status)~ kid + sex+order+ age+size+BirthPeriod+ (1|Village/ITID)
                  ,data = Feeding)
father_real <-coxme(Surv(time,status)~father+sex+order+ age+size+BirthPeriod+(1|Village/ITID)
                    ,data = Feeding)
parents_real <- coxme(Surv(time,status)~mom+father+sex+size+order+age+BirthPeriod+(1|Village/ITID)
                      ,data = Feeding) 
all_real <- coxme( Surv( time, status) ~ sex + size + order + age + BirthPeriod + kid + mom + father + (1|Village/ITID), data = Feeding)


#compare coefficients
child_sim$coefficients 
child_real$coefficients

mother_sim$coefficients
mother_real$coefficients

father_sim$coefficients
father_real$coefficients

parents_sim$coefficients
parents_real$coefficients

all_sim$coefficients
all_real$coefficients
