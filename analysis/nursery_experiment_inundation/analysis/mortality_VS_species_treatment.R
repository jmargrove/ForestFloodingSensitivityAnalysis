# nursery mortality analysis - species interaction for slopes 

require(lme4)
source(system.file("utils", "allFit.R", package = "lme4"))
source('./analysis/nursery_experiment_inundation/functions/index.R')
survival_data <- read.table("./analysis/nursery_experiment_inundation/data/nursery_experiment_data.txt", header = TRUE);
# Model does not converge.
surv_model <- glmer(surv ~ sp + treat + dia + sp:treat + 
                       (1 | mother) + (1 | block), 
                     data = survival_data, 
                     family = "binomial", 
                     control=glmerControl(optimizer="nlminbw"))

surv_model2 <- update(surv_model, . ~ . - sp:treat)
anova(surv_model, surv_model2)
# model with the interaction term explains signifincatly more variation 

##### residuals & fitted values 
surv_model1_residuals <- resid(surv_model, type =  "pearson");
surv_model1_fitted <- fitted(surv_model)

# check the binned plot 
newbinplot(surv_model1_fitted, surv_model1_residuals);

# Random effects 
ranNorm("mother", slope = 1, model = surv_model)
ranNorm("block", slope = 1, model = surv_model)

# coefs for slopes 
coef <- fixef(surv_model)

slope_coef <- data.frame(sp = levels(survival_data$sp), 
                         p = c(coef[11], coef[11] + coef[13:length(coef)]))

write.table(slope_coef, file = 'analysis/nursery_experiment_inundation/data/survival_slope_coef.txt')

