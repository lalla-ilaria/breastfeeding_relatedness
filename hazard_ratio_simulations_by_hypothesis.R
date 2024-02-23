library(rethinking)
library(dagitty)
library(truncnorm)
library(survival)
library(coxme)
library(tidyverse)
library(knitr)

#load and process real data
Feeding <-read.csv("../modelwithr full.csv",header=TRUE)
col_realhh <- ifelse(Feeding$PMR == "Duo", "lightgreen", ifelse(Feeding$PMR == "Matri", "indianred", ifelse(Feeding$PMR == "Neo", "goldenrod", "lightblue")))
#1 neolocal
#2 duolocal
#3 matrilocal
#4 patrilocal

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



#function to simulate breastfeeding data
#note that the function needs a set of average relatednesses for the parents in each residence type (relatedness_averages), and a set of parameters for the poisson simulation (one value per each of four parameters for each considered hypothesis, within the list poiss_parameters)
#note that the function has the option to return the full set of data and model results, or only the coefficients estimated by the cox models
# note that there (WILL BE) the option to test with censoring
sim_breastfeeding <- function (N, relatedness_avg = relatedness_averages, poiss_pars = poiss_parameters, n_hyp = ncol(poiss_pars), censor = FALSE, censor_at = 12, coefficients = FALSE ){
  #generate household types and assign color
  #1 neolocal
  #2 duolocal
  #3 matrilocal
  #4 patrilocal
  household <- sample(1:4, N, replace = TRUE)
  col_hh <- ifelse(household == 1, "goldenrod", ifelse(household == 2, "lightgreen", ifelse(household == 3, "indianred", "lightblue")))
  
  #generate relatednesses 
  mom_rel <- ifelse(household == 1,rtruncnorm(n = N, a = 0.09, b = 0.50, mean = relatedness_avg$mom[3], sd = 0.10),
                    ifelse(household == 2, rtruncnorm(n = N, a = 0.07, b = 0.50, mean = relatedness_avg$mom[1], sd = 0.09),
                           ifelse(household == 3,rtruncnorm(n = N, a = 0.08, b = 0.55, mean = relatedness_avg$mom[2], sd = 0.09),
                                  rtruncnorm(n = N, a = 0.05, b = 0.50,mean = relatedness_avg$mom[4], sd = 0.09))))
  
  
  dad_rel <- ifelse(household == 1,rtruncnorm(n = N, a = 0.00, b = 0.50, mean = relatedness_avg$dad[3], sd = 0.13),
                    ifelse(household == 2, rtruncnorm(n = N, a = 0.00, b = 0.43, mean = relatedness_avg$dad[1], sd = 0.12),
                           ifelse(household == 3,rtruncnorm(n = N, a = 0.00, b = 0.44, mean = relatedness_avg$dad[2], sd = 0.11),
                                  rtruncnorm(n = N, a = 0.25, b = 0.65, mean = relatedness_avg$dad[4], sd = 0.08))))
  
  child_rel <- (mom_rel + dad_rel)/2 + rnorm(N, 0.1, 0.05)
  
  #generate duration of breastfeeding from poisson distribution
  poiss_rate <- matrix(nrow = N, ncol = n_hyp)
  duration <- matrix(nrow = N, ncol = n_hyp)
  status <- matrix(nrow = N, ncol = n_hyp)
  for (i in 1:n_hyp){
    poiss_rate[,i] <- exp( rnorm(N, poiss_pars[1,i], 0.2) + child_rel * poiss_pars[2,i] + mom_rel * poiss_pars[3,i] + dad_rel * poiss_pars[4,i])
    
    duration[,i] <- rpois(N, poiss_rate[,i]) 
    
    #generate status (i.e. whether data is censored)
    if(censor == FALSE) {
      status[,i] <-  rep(1, N)} else {
        status[,i] <- ifelse(duration[,i] > censor_at, 0, 1) }
    
    #censor to twelve months
    if(censor == TRUE){
      duration[,i] <- ifelse(duration[,i] >= censor_at, censor_at, duration[,i])
    }#if censor
  }#generate durations
  
  #run cox models
  child_sim <- list()
  mother_sim <- list()
  father_sim <- list()
  parents_sim <- list()
  all_sim <- list()
  residence_sim <- list()
  
  for( i in 1:n_hyp){
    child_sim[[i]] <- coxph(Surv(duration[,i], status[,i]) ~ child_rel)
    mother_sim[[i]] <- coxph(Surv(duration[,i], status[,i]) ~ mom_rel)
    father_sim[[i]] <- coxph(Surv(duration[,i], status[,i]) ~ dad_rel)
    parents_sim[[i]] <- coxph(Surv(duration[,i], status[,i]) ~ mom_rel + dad_rel)
    all_sim[[i]] <- coxph(Surv(duration[,i], status[,i]) ~ child_rel + mom_rel + dad_rel)
    residence_sim[[i]] <- coxph(Surv(duration[,i], status[,i])~as.factor(household))
    
  }#run cox model
  
  #prepare data
  #if we want all data output (coefficients is FALSE)
  breastfeeding_data <- list(N = N,
                             household = household,
                             col_hh = col_hh,
                             mom_rel = mom_rel,
                             dad_rel = dad_rel,
                             child_rel = child_rel,
                             poiss_rate = poiss_rate,
                             duration = duration,
                             status = status,
                             child_sim = child_sim,
                             mother_sim = mother_sim,
                             father_sim = father_sim,
                             parents_sim = parents_sim,
                             all_sim = all_sim,
                             residence_sim = residence_sim
  )
  #if we want only the coefficients (for plotting, coefficients is TRUE)
  breastfeeding_coefficients <- list(child  = child_sim |> map("coefficients"), 
                                     mother = mother_sim |> map("coefficients"),
                                     father = father_sim |> map("coefficients"),
                                     parents = parents_sim |> map("coefficients"),
                                     all = all_sim |> map("coefficients"),
                                     residence = residence_sim |> map("coefficients"))
  #decide output (data or coefficients)
  if(coefficients == TRUE){
    return( breastfeeding_coefficients )
  } else {
    return( breastfeeding_data ) 
  }
  
}


#define average relatedness for parents
#the followinf is based on real data
relatedness_averages <- data.frame(mom = c(0.4, 0.36, 0.3, 0.22 ),
                                   dad = c(0.06, 0.22, 0.3, 0.38))
#define coefficients for each hypothesis (see below for explanation on the coefficients)
n_hypotheses <- 4
poiss_parameters <- matrix(c( 3,  2,-2,-2, #H1
                              2.5, 0, 2,-2, #H2
                              1,4,2,-2, #H3
                              2, 2, 0, 0),#H4
                           nrow = 4, ncol = n_hypotheses, byrow = FALSE,
                           dimnames = list(c("intercept", "child", "mom", "dad"),
                                           c(paste(rep("H", n_hypotheses), 1:n_hypotheses, sep = ""))))
#simulates breastfeeding data and model results for four different hypotheses
bf <- sim_breastfeeding(N = 558)


all_real <- coxme( Surv( time, status) ~ kid + mom + father + sex + size + order + age + BirthPeriod +  (1|Village/ITID), data = Feeding)


N_attempted_pars <- 50 #n of different parameters to simulate with 
par_value <- rtruncnorm(N_attempted_pars, 0, mean = 2) #set of paramter values to simulate with 

#all family members
png("hazard ratio simulations by hypothesis.png", width = 9, height = 7, units = "cm", res = 300, pointsize = 7)
par(mfrow = c(1,1),mgp = c(1.5, 0.5, 0), mar = c(2.5, 2.5, 2, 1) + 0.1)
plot(c(bf$all_sim[[1]]$coefficients, bf$all_sim[[2]]$coefficients, bf$all_sim[[3]]$coefficients, bf$all_sim[[4]]$coefficients),rep(1:4, each = 3) + c(-0.15, 0, 0.15),
     xlim = c(min(bf$all_sim[[3]]$coefficients) - 2.5, max (bf$all_sim[[2]]$coefficients) + 3.5),
     pch = 16, cex = 2, col = c("darkgreen", "darkred", "darkblue"), xlab = "hazard ratio by family member", ylab = "", yaxt = "n" , ylim = rev(c(0.5, 4.5)))
abline( v = 0 , col = "grey80")
axis(2, at = 1:4, las=1,
     labels =  c("H1", "H2", "H3", "H4"))
for (i in 1:length(par_value)) {
  many_poiss_par <- matrix(c( 3,  par_value[i],-par_value[i],-par_value[i], #H1
                              0.7, 0, par_value[i],-par_value[i], #H2
                              -2.5, 2*par_value[i],par_value[i],-par_value[i], #H3
                              0.2, par_value[i], 0, 0),#H4
                           nrow = 4, ncol = n_hypotheses, byrow = FALSE)
  coeffs <- sim_breastfeeding(100, coefficients = TRUE, poiss_pars = many_poiss_par)
  points(unlist(c(coeffs$all[1], coeffs$all[2], coeffs$all[3], coeffs$all[4])), rep(1:4, each = 3) + c(-0.15, 0, 0.15), pch = 16, col = adjustcolor(c("darkgreen","darkred", "darkblue"), alpha.f=0.4))
}

points(rep(all_real$coefficients[1:3], 4),rep(1:4, each = 3) + c(-0.15, 0, 0.15), pch = 16, cex = 2, col = c("lightgreen","indianred", "cornflowerblue"))
legend("topleft", inset = 0.02, c("Child", "Mother", "Father"), fill = c("lightgreen", "indianred", "cornflowerblue"), border = c("lightgreen", "indianred", "cornflowerblue"), box.col = "white")
dev.off()