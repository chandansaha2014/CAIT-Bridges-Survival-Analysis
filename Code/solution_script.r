###########################################################################
#  Title: CAIT Bridge Survival Analysis
#   Date: Sep 2016
# Author: Chandan Saha
#
#
# Description:
#
# This script runs Weibull accelerate failure time (AFT) model on NBI Dataset
# and predcits dock elements survival probability for condition State 1
#
###########################################################################

#Set working Directory 

setwd("~/Interview/CAIT/CAIT_Bridges/Data")

DataClean <- read.table("DataClean.csv", header= TRUE, sep= ",")

SurvivalData <- read.csv('Survival.csv')

##################   Data Cleaning       #################################


##########################################################################
####                      Filter Deck Structures                      ####
##########################################################################

# Filtering Deck Structure From DataClean table
Data_DeckStructure <- DataClean[ which(DataClean$ElementNumber== 12), ]

# Data Exploration
names(Data_DeckStructure)
summary(Data_DeckStructure)
sum(is.na(Data_DeckStructure))

mice_plot <- aggr(Data_DeckStructure, col=c('blue','green'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(Data_DeckStructure), cex.axis=.7,
                  gap=1, ylab=c("Missing data","Pattern"))

setwd("~/Interview/CAIT/CAIT_Bridges/Output")
dev.copy(jpeg,'mice_plot.jpeg')
dev.off()

sum(is.na(Data_DeckStructure$NumberOfSnowfalls))
##########################################################################
####                         Remove Null Rows                         ####
##########################################################################

str(Data_DeckStructure)
dim(Data_DeckStructure)



missmap(DataClean, main = "Missingness Map Train")


setwd("~/Interview/CAIT/CAIT_Bridges/Output")
dev.copy(jpeg,'missingness.jpeg')
dev.off()

# Check N/A for Condition State Coloumns
sum(is.na(Data_DeckStructure$ConditionState1))
sum(is.na(Data_DeckStructure$ConditionState2))
sum(is.na(Data_DeckStructure$ConditionState3))
sum(is.na(Data_DeckStructure$ConditionState4))



Data_DeckStructure$TotalElement = Data_DeckStructure$ConditionState1 + Data_DeckStructure$ConditionState2 + Data_DeckStructure$ConditionState3+ Data_DeckStructure$ConditionState4
head(Data_DeckStructure$TotalElement)


####                            Feature Extraction                   #####

SelectVariable <- psm(Surv(TIS, Survival) ~ Age + ADTT + ADT + Skew +  NumberOfSnowfalls+ NumberOfFreezeThawCycles , 
                            weights = Quantity, data = SurvivalData,  dist = "weibull") 
anova(SelectVariable)

plot(anova(SelectVariable) , margin = c("chisq" ,"d.f." , "P"))

fastbw(SelectVariable , rule = "aic")


##########################################################################
####              Check Dock Condition's Consistency                  ####
##########################################################################



UniqueStructure <- as.data.frame(unique(Data_DeckStructure[c("StructureNumber", "TotalElement")]))
head(UniqueStructure)


results <- as.data.frame(table(UniqueStructure$StructureNumber))
head(results)
ConstCondState <- results[ which(results$Freq=='1'),] 


newdata <- Data_DeckStructure[Data_DeckStructure$StructureNumber %in% ConstCondState$Var1, ]
head(newdata)


# Checking again for duplicate 
UniqueStructure <- as.data.frame(unique(newdata[c("StructureNumber", "TotalElement")]))
length(unique(UniqueStructure$StructureNumber))
nrow(UniqueStructure)

# Write Csv file named "output.csv" to Output Folder 

setwd("~/Interview/CAIT/CAIT_Bridges/Output")
cat("Saving Output file..............")
write.csv(newdata, file = "output.csv")


##########################################################################
####                    Survival Modeling                             ####
##########################################################################

#Data Exploration - structures and classes

head(SurvivalData)
summary(SurvivalData)
class(SurvivalData$TIS)


##########################################################################
####    Parametric Weibull model- Accelerated Failure Time            ####
##########################################################################

#Weibull Model Fit - With Weight
weibull <- survreg(Surv(TIS, Survival)~ Age + ADTT+ ADT+  Skew+ NumberOfSnowfalls+ NumberOfFreezeThawCycles, weights = Quantity, data = SurvivalData[SurvivalData$TIS >0,],  dist = "weibull") 

summary(weibull)

# 0-50 years survival probability 
Deficient_Age = seq(1,50, by=1)
weibull_fit <- data.frame(Deficient_Age)



##########################################################################
####                          Weibull FIt With Weight                 ####
##########################################################################

# Age =26.68, ADT=5267, ADTT=763.37, NumberOfSnowfalls=14.88 , NumberOfFreezeThawCycles=56.34, Skew = 13.39
beta <- (3.74e+00*1) + (1.07e-02*26.68) + (8.04e-05 *763.37) + (-2.24e-05*5267)+ (4.78e-03*13.39)+ (1.05e-02*14.88)+ (-7.61e-03 *56.34)



# The Weibill Parameterisation is done using Maximum Liklihood estimation-survreg function
scale_param <- weibull$scale
intercept <- weibull$coefficients[1]
gamma <- 1/scale_param
alpha <- exp(-beta*gamma)
weibull_fit$survival_probability <- exp(-alpha*Deficient_Age^gamma)

# Survival Probability for given value of covaraites

ggplot(data= weibull_fit, aes(x=weibull_fit$Deficient_Age, y=weibull_fit$survival_probability, group=1)) +
  geom_line(colour="skyblue" , size=1)+
  geom_point(size=1.5) +
  xlab("Deffieciency Age in Years") + ylab("Survival Probability") +
  ggtitle("Weibull Survival Graph")

# Save Plot 
setwd("~/Interview/CAIT/CAIT_Bridges/Output")
dev.copy(jpeg,'Survival_weights.jpeg')
dev.off()

# Survival Probability corresponding to t= 25
PredCondStage1 <- weibull_fit$survival_probability[weibull_fit$Deficient_Age==25]
PredCondStage1 
# 54% of the elements would be in condition state 1 for t= 25 years



##########################################################################
####                  Fitting without Weight                          ####
##########################################################################
#Weibull Model Fit - Without Weight
weibull_nonweight <- survreg(Surv(TIS, Survival)~ Age + ADTT+ ADT+  Skew+ NumberOfSnowfalls+ NumberOfFreezeThawCycles, data = SurvivalData[SurvivalData$TIS >0,],  dist = "weibull") 

summary(weibull_nonweight)

weibull_fit_nonwight <- data.frame(Deficient_Age)


# Age =26.68, ADT=5267, ADTT=763.37, NumberOfSnowfalls=14.88 , NumberOfFreezeThawCycles=56.34, Skew = 13.39
beta_nonweight <- (2.75e+00+00*1) + (7.62e-03*26.68) + (1.25e-05 *763.37) + (-1.86e-05*5267)+ (1.13e-03*13.39)+ (3.40e-03*14.88)+ (-2.69e-03 *56.34)



scale_param_nonweight <- weibull_nonweight$scale
intercept_nonweight <- weibull_nonweight$coefficients[1]
gamma_nonweight <- 1/scale_param_nonweight
alpha_nonweight <- exp(-beta_nonweight*gamma_nonweight)
weibull_fit_nonwight$survival_probability <- exp(-alpha_nonweight*Deficient_Age^gamma_nonweight)

# Survival Probability for given value of covaraites

ggplot(data= weibull_fit_nonwight, aes(x=weibull_fit_nonwight$Deficient_Age, y=weibull_fit_nonwight$survival_probability, group=1)) +
  geom_line(colour="red" , size=1)+
  geom_point(size=1.5) +
  xlab("Deffieciency Age in Years") + ylab("Survival Probability") +
  ggtitle("Weibull Non Weight")


# Save Plot 
setwd("~/Interview/CAIT/CAIT_Bridges/Output")
dev.copy(jpeg,'Survival_nonweights.jpeg')
dev.off()

# Survival Probability corresponding to t= 25
PredCondStage1_nonweight <- weibull_fit_nonwight$survival_probability[weibull_fit_nonwight$Deficient_Age==25]
PredCondStage1_nonweight 
# 20% of the Dock elements would be in condition state 1 till t= 25 years



##########################################################################
####                  Comparision with Kaplan Meir                    ####
##########################################################################



#Kaplan Meir 


SurvivalFitKaplan <- survfit(Surv(SurvivalData$TIS, SurvivalData$Survival)~1, conf.type = 'none')
summary(SurvivalFitKaplan)$table

# Kaplan Meir Plot
plot(SurvivalFitKaplan, xlab="Years in Condition State 1", ylab="Survival Probability")






# Predict_function 

## Weibull distribution using Predict function
cov_data <- list(Age = 26.28, ADT=5267, ADTT=763.37, NumberOfSnowfalls=14.88 , NumberOfFreezeThawCycles=56.34,Skew = 13.39 )
lines(predict(weibull, newdata=cov_data,type="quantile",p=seq(.01,.99,by=.01)),seq(.99,.01,by=-.01),col="blue", xlim= c(0,50))



# Using flexsurv function

flex_surv <- flexsurvreg(Surv(TIS, Survival) ~ Age+ ADTT+ ADT+ NumberOfSnowfalls+ NumberOfFreezeThawCycles+ Skew, weights=Quantity, data = SurvivalData, dist = "weibull")
#covariate_data <- list(Age = 26.28, ADT=5267, ADTT=763.37, NumberOfSnowfalls=14.88 , NumberOfFreezeThawCycles=56.34,Skew = 13.39 )
plot(flex_surv, newdata=cov_data, X=NULL, type="survival", t=NULL,est=TRUE, ci=NULL, B=1000, cl=0.95,col="green", lty=1, lwd=2, col.ci=NULL, lty.ci=2, lwd.ci=1, main="Weibull using flexsurv")

# Save Plot to Output Folder
setwd("~/Interview/CAIT/CAIT_Bridges/Output")
dev.copy(jpeg,'Survival_weibull_Flexsurv.jpeg')
dev.off()


#### CLeaning Up 
cat("Execution Complete... ")
cat("Cleaning Up Workspace... ")
#rm(list = ls())



