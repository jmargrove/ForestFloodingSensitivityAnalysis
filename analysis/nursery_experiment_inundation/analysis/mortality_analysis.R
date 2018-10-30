##### Paper: water inundation sensitivity in dipterocarps
##### author: james margrove 
##### date: 26.01.2017
################################################################################

##### remove clutter 
rm(list=ls())

##### imported packaged 
require(ggplot2)
require(lme4)
#install.packages("lme4")
require(merTools)
#install.packages("merTools")
require(lmerTest)
#install.packages("lmerTest", dep = TRUE)
source(system.file("utils", "allFit.R", package = "lme4"))
source("./functions/newbinplot.R"); require(arm)
source("./functions/ranNorm.R")
source("./functions/booter.R")
survival_data <- read.table("./data/Experiment, mort, leafAB, dden, wden,sla.txt", header = TRUE);
str(survival_data)

##### data exploration with ggplot. 
ggplot(survival_data, aes(x=treat, y=surv)) + geom_point() + stat_smooth()


##### first model if treatmnean and diameter had an addative effect 
survival_model1 <- glmer(surv ~ treat + dia + 
                           (1|sp/mother) + 
                           (1|block), 
                         data = survival_data, 
                         family = "binomial")
summary(survival_model1)

##### effect
# the effect of flooding overall on dipterocarps is negative. 
##### model validation.

##### residuals & fitted values 
surv_model1_residuals <- resid(survival_model1, type =  "pearson");
surv_model1_fitted <- fitted(survival_model1)

# Some deviations from normallity but otherwise OK 
newbinplot(surv_model1_residuals, surv_model1_fitted);

# Random effects reasonabbly normally distributed
ranNorm("mother:sp", slope = 1, model = survival_model1)
ranNorm("sp", slope = 1, model = survival_model1)
ranNorm("block", slope = 1, model = surivival_model1)

##### Graphing the analysis
surv_preds <- expand.grid(dia = mean(survival_data$dia),
                          treat = seq(from = 0, to = 21, length = 100),
                          mother = 0,
                          sp = 0,
                          block = 0)

##### predicting the values 
surv_preds$p <- predict(survival_model1, 
                       newdata = surv_preds,
                       type = "response", 
                       re.form = NA)

predict(survival_model1, 
        newdata = surv_preds,
        type = "response", 
        re.form = NA)

summary(survival_model1)

# bootstrap confidence intervals for the analysis 
CI <- booter(survival_model1, surv_preds, survival_data, 1000)
surv_preds$CI025 <- CI[1,]
surv_preds$CI025 <- CI[2,]
write.table(surv_preds, file = "./graphs/graph_data/surv_pred_dipter.csv")

# Confidence intervals 
ggplot(surv_preds, aes(x=treat, y=p)) + geom_line() + 
  geom_ribbon(aes(ymin = CI[1,], ymax = CI[2,]))
################################################################################
##### extremes of survival curve...
min(surv_preds$p)
max(surv_preds$p)

##### seedling survival analysis 
surv_preds_dia <- expand.grid(dia = seq(from = min(survival_data$dia, na.rm = TRUE), 
                                        to = max(survival_data$dia, na.rm = TRUE), 
                                        length = 100), 
                              treat = 9)

surv_preds_dia$p <- predict(survival_model1, 
                            newdata = surv_preds_dia, 
                            type = "response",
                            re.form = NA)

##### plotting the data 
ggplot(surv_preds_dia, aes(x = dia, y = p)) + geom_line()


################################################################################
################################################################################
################################################################################
##### species interaction - survival analysis... 
##### are the species different 
##### date: 30 01 2017

# first model 
surv_model2 <- glmer(surv ~ sp + treat + dia + sp:treat + 
                       (1 | mother) + (1| block), 
                     data = survival_data, 
                     family = "binomial")

# Model does not converge.
surv_model3 <- glmer(surv ~ sp + treat + dia + sp:treat + 
                       (1 | mother) + (1 | block), 
                     data = survival_data, 
                     family = "binomial", 
                     control=glmerControl(optimizer="nlminbw"))

# optimizer nlminbw allows convergence.
summary(surv_model3)

# residuals & fitted values.
surv_model1_residuals <- resid(surv_model3, type =  "pearson");
surv_model1_fitted <- fitted(surv_model3)

# Some deviations from normallity but otherwise OK (for ecology data).
newbinplot(surv_model1_residuals, surv_model1_fitted);

# Random effects reasonably normally distributed
ranNorm("mother", slope = 1, model = surv_model3)
ranNorm("block", slope = 1, model = surv_model3)

################################################################################
#Graphing the data 
preds_sp_inter_surv <- expand.grid(sp = levels(survival_data$sp), 
                                   treat = seq(from = 0, 
                                               to = 21, 
                                               length = 100),
                                   dia = mean(survival_data$dia, na.rm = TRUE))

preds_sp_inter_surv$p <- predict(surv_model3, 
                                 newdata = preds_sp_inter_surv,
                                 type = "response",
                                 re.form = NA)

##### boot strap the model to get confidence intervals 
CI <- booter(surv_model3, preds_sp_inter_surv, survival_data, nsamples = 10)
CI025 <- CI[1,]
CI975 <- CI[2,]

ggplot(preds_sp_inter_surv, aes(x = treat, y = p, color = sp)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = CI025, ymax = CI975))
  

coef <- fixef(surv_model3)

slope_coef <- data.frame(sp = levels(survival_data$sp), 
                         p = c(coef[11], coef[11] + coef[13:length(coef)]))

rownames(slope_coef) <- c()
#slope_coef <- slope_coef[order(-slope_coef$p), ]

# Testing the model with a type II anova 
car::Anova(surv_model3)
# Summarise the model 
summary(surv_model3)
# Calculate the differance in AIC
diff(AIC(surv_model3, survival_model1)[,2])
# write the coef results 
write.table(slope_coef, file = "./psSlopeCoef.txt")

