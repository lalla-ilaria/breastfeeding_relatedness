###############################
######## LOAD PACKAGES#########
###############################
#create function to install the package if not installed already
usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

usePackage("survival")
usePackage("survminer")
usePackage("coxme")
usePackage("Rmisc")
usePackage("writexl")
usePackage("arsenal")
usePackage("rmarkdown")
usePackage("raster")
usePackage("truncnorm")
usePackage("tidyverse")
###############################
#### LOAD & process DATA ######
###############################

Feeding<-read.csv("breastfeeding_data.csv",header=TRUE)
Feeding<- Feeding[complete.cases(Feeding),]
for (i in c(1:8,10) ) {
  Feeding[,i] <- as.factor(Feeding[,i])
}
Feeding$PMR <- relevel(Feeding$PMR, ref = "Patri")
head(Feeding)


###############################
###### RUN COX MODELS #########
###############################
#Model 2 with residence pattern
res<-coxme(Surv(time,status)~Csex+order+HSCAT+ BirthPeriod+PMR+(1|Village/ITID)
           ,data = Feeding) 
        
#Model 3 with predictors for all family members
all<- coxme(Surv(time,status)~Csex+order+HSCAT+BirthPeriod+kids+mother+father+(1|Village/ITID)
            ,data = Feeding)  

#Model 1 with only controls
control<- coxme(Surv(time,status)~Csex+order+HSCAT+BirthPeriod+(1|Village/ITID)
                ,data = Feeding) 

###############################
######## PLOTS ################
###############################

#Figure 2
######density plot
duo <- subset(Feeding, PMR=="Duo")
patri<-subset(Feeding, PMR=="Patri")
matri<-subset(Feeding, PMR=="Matri")
neo<-subset(Feeding, PMR=="Neo")

density_duo <- density(duo$AgeStopFeed)
density_patri <- density(patri$AgeStopFeed)
density_matri <- density(matri$AgeStopFeed)
density_neo <- density(neo$AgeStopFeed)

mean_duo <- mean(duo$AgeStopFeed)
mean_patri <- mean(patri$AgeStopFeed)
mean_matri <- mean(matri$AgeStopFeed)
mean_neo <- mean(neo$AgeStopFeed)

# Plot the density for duo$AgeStopFeed
png("Fig2.png", width = 9, height = 8, units = "cm", res = 300, pointsize = 8)
par(mfrow = c(1,1),mgp = c(1.5, 0.5, 0), mar = c(2.5, 2.5, 2, 1) + 0.1)
plot(density_duo, main="Density Plot of Breastfeeding Duration", xlab="Time (months)", ylab="Density", xlim=c(0, 50), ylim=range(c(0,0.06)),  type="n")

# Adding density plot for patri$AgeStopFeed with transparency
polygon(density_patri, col=adjustcolor("#E7B800", alpha.f=0.4), border=adjustcolor("#E7B800", alpha.f=0.4))

# Adding density plot for duo$AgeStopFeed again so it's on top
polygon(density_duo, col=adjustcolor("#F76D5E", alpha.f=0.4), border=adjustcolor("#F76D5E", alpha=0.4))

polygon(density_matri, col=adjustcolor("#72D8FF", alpha.f=0.4), border=adjustcolor("#72D8FF", alpha=0.4))
polygon(density_neo, col=adjustcolor("#62AA67", alpha.f=0.4), border=adjustcolor("#62AA67", alpha=0.4))

abline(v=mean_duo, col="#F76D5E", lwd=2, lty=2)
abline(v=mean_patri, col="#E7B800", lwd=2, lty=2)
abline(v=mean_matri, col="#72D8FF", lwd=2, lty=2)
abline(v=mean_neo, col="#62AA69", lwd=2, lty=2)

# Adding a legend
legend("topright", legend=c("Patrilocal", "Duolocal","Matrilocal","Neolocal"), fill=c(adjustcolor("#E7B800", alpha.f=0.4),
                                                                                      adjustcolor("#F76D5E", alpha.f=0.4),
                                                                                      adjustcolor("#72D8FF", alpha.f=0.4),
                                                                                      adjustcolor("#62AA69", alpha.f=0.4)))
dev.off()


#Figure 4
###############
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
bf <- sim_breastfeeding(N = 500)




N_attempted_pars <- 50 #n of different parameters to simulate with
par_value <- rtruncnorm(N_attempted_pars, 0, mean = 2) #set of paramter values to simulate with

#all family members
png("Fig4.png", width = 9, height = 7, units = "cm", res = 300, pointsize = 9)
par(mfrow = c(1,1),mgp = c(1.5, 0.5, 0), mar = c(2.5, 2.5, 2, 1) + 0.1)
plot(c(bf$all_sim[[1]]$coefficients, bf$all_sim[[2]]$coefficients, bf$all_sim[[3]]$coefficients),rep(1:3, each = 3) + c(-0.15, 0, 0.15),
     xlim = c(min(bf$all_sim[[3]]$coefficients) - 2.5, max (bf$all_sim[[2]]$coefficients) + 3.5),
     pch = 16, cex = 2, col = c("darkgreen", "darkred", "darkblue"), xlab = "cox parameter estimate by family member", ylab = "", yaxt = "n" , ylim = rev(c(0.5, 3.5)))
abline( v = 0 , col = "grey80")
axis(2, at = 1:3, las=1,
     labels =  c("H1", "H2", "H3"))
for (i in 1:length(par_value)) {
  many_poiss_par <- matrix(c( 3,  par_value[i],-par_value[i],-par_value[i], #H1
                              0.7, 0, par_value[i],-par_value[i], #H2
                              -2.5, 2*par_value[i],par_value[i],-par_value[i], #H3
                              0.2, par_value[i], 0, 0),#H4
                           nrow = 4, ncol = n_hypotheses, byrow = FALSE)
  coeffs <- sim_breastfeeding(100, coefficients = TRUE, poiss_pars = many_poiss_par)
  points(unlist(c(coeffs$all[1], coeffs$all[2], coeffs$all[3])), rep(1:3, each = 3) + c(-0.15, 0, 0.15), pch = 16, col = adjustcolor(c("darkgreen","darkred", "darkblue"), alpha.f=0.4))
}

points(rep(all$coefficients[7:9], 3),rep(1:3, each = 3) + c(-0.15, 0, 0.15), pch = 16, cex = 2, col = c("lightgreen","indianred", "cornflowerblue"))
legend("topleft", inset = 0.02, c("Child", "Mother", "Father"), fill = c("lightgreen", "indianred", "cornflowerblue"), border = c("lightgreen", "indianred", "cornflowerblue"), box.col = "white")
dev.off()
##################

hazard_ratios <- matrix(nrow = 12, ncol = 3, dimnames = list( c("Son (vs daughter)", "2nd (vs 1st) child", "Medium size (vs. big)", "Small size (vs. big)", "Mother cohort >=1990 (vs. <1980)", "Mother cohort 1980-89 (vs. <1980)", "Duolocal (vs. Patrilocal)", "Matrilocal (vs. Patrilocal)", "Neolocal (vs. Patrilocal)", "Child relatedness", "Mother relatedness", "Fahter relatedness") , c("Model 1 - HR (p value)", "Model 2 - HR (p value)", "Model 3 - HR (p value)")))
#####Results from mixed effects models, HR & 95% CI
days_se<- sqrt(diag(vcov(control)))
days_coef<- fixef(control)
upper.CI<- days_coef+ 1.96*days_se
lower.CI<- days_coef-1.96*days_se
HR<- exp(days_coef)
p_val <- signif(1 - pchisq((days_coef/days_se)^2, 1), 2)
model <- rep("control", length(days_se))
forest_data <- cbind(days_coef,upper.CI,lower.CI,HR, p_val, model)
hazard_ratios[1:length(days_se),1] <- paste(round(HR, 2), " (", round(p_val, 2), ")", sep = "") 

days_se<- sqrt(diag(vcov(res)))
days_coef<- fixef(res)
upper.CI<- days_coef+ 1.96*days_se
lower.CI<- days_coef-1.96*days_se
HR<- exp(days_coef)
p_val <- signif(1 - pchisq((days_coef/days_se)^2, 1), 2)
model <- rep("residence", length(days_se))
forest_data <- rbind(forest_data,
                     cbind(days_coef,upper.CI,lower.CI,HR, p_val, model)[7:9,])
hazard_ratios[1:length(days_se),2] <- paste(round(HR, 2), " (", round(p_val, 2), ")", sep = "") 


days_se<- sqrt(diag(vcov(all)))
days_coef<- fixef(all)
upper.CI<- days_coef+ 1.96*days_se
lower.CI<- days_coef-1.96*days_se
HR<- exp(days_coef)
p_val <- signif(1 - pchisq((days_coef/days_se)^2, 1), 2)
model <- rep("all", length(days_se))
forest_data <- rbind(forest_data,
                     cbind(days_coef,upper.CI,lower.CI,HR, p_val, model)[7:9,])
hazard_ratios[1:6,3] <- paste(round(HR[1:6], 2), " (", round(p_val[1:6], 2), ")", sep = "") 
hazard_ratios[10:12,3] <- paste(round(HR[7:9], 2), " (", round(p_val[7:9], 2), ")", sep = "") 


forest_data <- as.data.frame(forest_data)
forest_data$color <- ifelse(forest_data$model == "control", "#281713", ifelse(forest_data$model == "residence", "cornflowerblue", "indianred"))

png("Fig3.png", width = 16, height = 7, units = "cm", res = 300, pointsize = 9)
par(mfrow = c(1,1),mgp = c(2.5, 0.5, 0), mar = c(2.5, 14, 2, 1) + 0.1)
plot(forest_data$days_coef, nrow(forest_data):1, xlim = c(-7, 4), 
     pch = 16, col = forest_data$color, cex = 2,
     xlab = "coefficient estimate", ylab = "", yaxt = "n")
abline( v = 0 , col = "grey80")
for (i in nrow(forest_data):1) {
  lines(c(forest_data$lower.CI[i], forest_data$upper.CI[i]), rep(c(12:1)[i], each =2), col = forest_data$color[i])
  }
axis(2, at = nrow(forest_data):1, las=1,
     labels =  c("Son (vs daughter)", "2nd (vs 1st) child", "Medium size (vs. big)", "Small size (vs. big)", "Mother cohort >=1989 (vs. <1980)", "Mother cohort 1980-89 (vs. <1980)", "Duolocal (vs. Patrilocal)", "Matrilocal (vs. Patrilocal)", "Neolocal (vs. Patrilocal)", "Child relatedness", "Mother relatedness", "Fahter relatedness"))
dev.off()



###############################
#######DESCRIPTIVE STATS#######
###############################


#####descriptive stastics
table3 <- summary(tableby(District ~ PMR+ BirthPeriod +order
                          + HSCAT +AgeStopFeed+ mother+kids+father+time + status, data=Feeding, test=FALSE))

write2word(table3, "describe.doc",
           keep.md = TRUE,
           quiet = TRUE, # passed to rmarkdown::render
           title = "model describe")
#####describe relatedness
table4 <- summary(tableby(PMR ~  mother+kids+father, data=Feeding, test=FALSE))
write2word(table4, "relatedness.doc",
           keep.md = TRUE,
           quiet = TRUE, # passed to rmarkdown::render
           title = "relatedness")

summary(Feeding$Csex)
