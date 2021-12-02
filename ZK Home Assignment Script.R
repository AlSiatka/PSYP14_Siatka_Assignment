# ZK Home Assignment Script #

# PART 1
# packages used #
library(psych) # for describe
library(car) # for residualPlots, vif, pairs.panels, ncvTest
library(lmtest) # bptest
library(sandwich) # for coeftest vcovHC estimator
library(boot) # for bootstrapping
library(lmboot) # for wild bootsrapping
library(tidyverse) # for tidy code

data_sample_1 = read.csv("https://tinyurl.com/ha-dataset1")
# read data set

# cleaning data
#participant ID_88 pain is coded wrong / 55 is not possible on the pain scale
#participant ID_34 is coded wrong / 4.2 is not possible for STAI_trait

# REMOVED ID_34/ID_88 DUE TO BEING CODED WRONG #

new_editedds <- data_sample_1[-c(88, 34), ]

# original models 1 and 2#

#model 1
mod_ends1 <- lm(pain ~ age + sex, data = new_editedds)
summary(mod_ends1)$adj.r.squared # 0.073555033
#model 2
mod_ends2 <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + cortisol_saliva + mindfulness, data = new_editedds)
summary(mod_ends2)$adj.r.squared # 0.5180092

# checking my models #

new_editedds %>%
  mutate(rownum = row.names(new_editedds)) %>%
  ggplot() + aes(x = age, y = pain, label = rownum) +
  geom_label() 

# threshold of Cook's distance > 4/158 = 0.025 #
#Cook's distance model 1
mod_ends1 %>%
  plot(which = 4)

mod_ends1 %>%
  plot(which = 5)

#Cook's distance model 2
mod_ends2 %>%
  plot(which = 4)

mod_ends2 %>%
  plot(which = 5)

# assumptions of linear regression
# Normality: The residuals of the model must be normally distributed
# Linearity: Ther relationship between the outcome variable and the predictor(s) must be linear
# Homoscedasticity: The variance of the residuals are similar at all values of the predictor(s)
# No Multicollinearity:None of the predictors can be linearly determined by the other predictor(s)

# normality # okay //
# QQ plot
mod_ends1 %>%
  plot(which = 2)

mod_ends2 %>%
  plot(which = 2)

# histogram
residuals_mod_ends1 = enframe(residuals(mod_ends1))
residuals_mod_ends1 %>%
  ggplot() + aes(x = value) + geom_histogram()

residuals_mod_ends2 = enframe(residuals(mod_ends2))
residuals_mod_ends2 %>%
  ggplot() + aes(x = value) + geom_histogram()

# skew and kurtosis
describe(residuals(mod_ends1))
describe(residuals(mod_ends2))
##comparing the models on data with and without the outliers #
summary(mod_ds1)
mod_ds1 <- lm(pain ~ age + sex, data = data_sample_1)
summary(mod_ds2)
mod_ds2 <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + cortisol_saliva + mindfulness, data = data_sample_1)

#linearity # okay //
windows()
mod_ends1 %>%
  residualPlots()

windows()
mod_ends2 %>%
  residualPlots()

# homoscedasticity # okay//
mod_ends1 %>%
  plot(which = 3)

mod_ends2 %>%
  plot(which = 3)

mod_ends1 %>%
  bptest() # Breush-Pagan test

mod_ends2 %>%
  bptest() # Breush-Pagan test

# multicollinearity #
mod_ends1 %>%
  vif()

mod_ends2 %>%
  vif()
# cortisol_serum and cortisol_saliva over 3 #
#check the correlation between the two variables #
windows()
new_editedds %>%
  select(pain, cortisol_serum, mindfulness, cortisol_saliva) %>%
  pairs.panels(col = "red", lm = T)

# REMOVED CORTISOL_SALIVA #
# FINAL MODELS - models 1 and new model 2#

#model 1
mod_ends1 <- lm(pain ~ age + sex, data = new_editedds)
summary(mod_ends1)$adj.r.squared # 0.073555033
#model 2
mod_cds2 <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + mindfulness, data = new_editedds)
summary(mod_cds2)$adj.r.squared # 0.504055

#comparison of AIC
AIC(mod_ends1) # 574.1267
AIC(mod_cds2) # 479.2624 - better fitted model
# anova
anova(mod_ends1, mod_cds2)

# checking my models #

new_editedds %>%
  mutate(rownum = row.names(new_editedds)) %>%
  ggplot() + aes(x = age, y = pain, label = rownum) +
  geom_label() 

# threshold of Cook's distance > 4/158 = 0.025 #
#Cook's distance model 2 # okay //
mod_cds2 %>%
  plot(which = 4)

mod_cds2 %>%
  plot(which = 5)

# assumptions of linear regression

# normality # okay //
# QQ plot
mod_cds2 %>%
  plot(which = 2)

# histogram
residuals_mod_cds2 = enframe(residuals(mod_cds2))
residuals_mod_cds2 %>%
  ggplot() + aes(x = value) + geom_histogram()

# skew and kurtosis
describe(residuals(mod_cds2))

#linearity # okay //
windows()
mod_cds2 %>%
  residualPlots()

# homoscedasticity # okay //
mod_cds2 %>%
  plot(which = 3)

mod_cds2 %>%
  bptest() # Breush-Pagan test

# multicollinearity # okay //
mod_cds2 %>%
  vif()

# PART 2
# packages used #
library(psych) # for describe
library(car) # for residualPlots, vif, pairs.panels, ncvTest
library(lmtest) # bptest
library(sandwich) # for coeftest vcovHC estimator
library(boot) # for bootstrapping
library(lmboot) # for wild bootsrapping
library(tidyverse) # for tidy code

data_sample_1 = read.csv("https://tinyurl.com/ha-dataset1")
# read data set

# cleaning data set by removing incorrectly coded ID_88 and ID_34
new_datasamp1 <- data_sample_1[-c(88, 34), ]

# building the regression models // pre-checking diagnostics

# initial model
initial_model <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + weight + IQ + household_income, data = new_datasamp1)
summary(initial_model)$adj.r.squared # 0.4980117

# checking my models #

new_datasamp1 %>%
  mutate(rownum = row.names(new_datasamp1)) %>%
  ggplot() + aes(x = age, y = pain, label = rownum) +
  geom_label() 

#Cook's distance # okay //
initial_model %>%
  plot(which = 4)

initial_model %>%
  plot(which = 5)

# assumptions of linear regression
# Normality: The residuals of the model must be normally distributed
# Linearity: Ther relationship between the outcome variable and the predictor(s) must be linear
# Homoscedasticity: The variance of the residuals are similar at all values of the predictor(s)
# No Multicollinearity:None of the predictors can be linearly determined by the other predictor(s)

# NEW MODEL WITH ID_88 and ID_34 removed

# normality # okay //
# QQ plot
initial_model %>%
  plot(which = 2)

# histogram
residuals_initial_model = enframe(residuals(initial_model))
residuals_initial_model %>%
  ggplot() + aes(x = value) + geom_histogram()

# skew and kurtosis
describe(residuals(initial_model))

#linearity # okay //
windows()
initial_model %>%
  residualPlots()

# homoscedasticity # okay//
initial_model %>%
  plot(which = 3)

initial_model %>%
  bptest() # Breush-Pagan test

# multicollinearity # okay //
initial_model %>%
  vif()

# BACKWARDS REGRESSION #

back_initial_model = step(initial_model, direction = "backward")
# predictors retained: age, mindfulness, cortisol_serum, pain_cat #

#comparison of AIC
AIC(my_model) # XX
AIC(initial_model) # 484.0054 

# new regression model using retained predictors #
backward_model <- lm(pain ~ age + pain_cat + mindfulness + cortisol_serum, data = new_datasamp1)
theory_based_model <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + mindfulness, data = new_datasamp1)

# comparing the two models via AIC #
AIC(backward_model) # 476.3015
AIC(theory_based_model) # 479.2624

# comparing the two models via ANOVA #
anova(backward_model, theory_based_model)

# comparing the two models #
summary(backward_model)$adj.r.squared # 0.5073085
summary(theory_based_model)$adj.r.squared # 0.504055

# backward_model is better #

# PREDICT PAIN BASED ON DATA 2 #
data_sample_2 = read.csv("https://tinyurl.com/87v6emky")

# calculate predicted values
pred_test <- predict(backward_model, data_sample_2)
pred_test_back <- predict(theory_based_model, data_sample_2)

# now we calculate the sum of squared residuals
RSS_test = sum((data_sample_2[, "pain"] - pred_test)^2)
RSS_test_back = sum((data_sample_2[, "pain"] - pred_test_back)^2)


# PART 3
# packages used #
library(psych) # for describe
library(car) # for residualPlots, vif, pairs.panels, ncvTest
library(lmtest) # bptest
library(sandwich) # for coeftest vcovHC estimator
library(boot) # for bootstrapping
library(lmboot) # for wild bootsrapping
library(tidyverse) # for tidy code
library(lme4) # random fixed effects model
library(cAIC4) # comparison
library(r2glmm) # for r2beta\t
library(lmerTest) # to get singificance test in lmer
library(MuMIn) # for r.squaredGLMM

# custom function from notes - extracts beta coefficients from LMM #
stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object, "y"))
  sdx <- apply(getME(object, "X"), 2, sd)
  sc <- fixef(object) * sdx/sdy
  se.fixef <- coef(summary(object))[, "Std. Error"]
  se <- se.fixef * sdx/sdy
  return(data.frame(stdcoef = sc, stdse = se))
}

# read data set
data_sample_3 = read.csv("https://tinyurl.com/b385chpu")
sum(is.na(data_sample_3))

# assigning hospital as a grouping factor #
data_sample_3 = data_sample_3 %>%
  mutate(hospital = factor(hospital))

# random-intercept model #
mod_lmm1 = lmer(pain ~ sex + age + STAI_trait + pain_cat + cortisol_serum + cortisol_saliva + mindfulness + (1 | hospital), data = data_sample_3)
summary(mod_lmm1)

# compute variance for model #
r.squaredGLMM(mod_lmm1)
# marginal - 0.3869574 // 39% of variance is explained by the fixed effects
# conditional - 0.4654805 // 47% of the variance is explained by both fixed and random effects

# predict pain on data_sample_4 by the data_sample_3 regression equation #
# regression equation - y=(intercept) + (intercept1)X1 + (intercept2)X2 + (intercept3)X3 + (intercept4)X4 + (intercept5)X5 + (intercept6)X6 + (intercept7)X7 + e
## y = (3.45051) + (1.55871)(sex) + (-0.05882)(age) + (-0.02342)(STAI_trait) + (0.08550)(pain_cat) + (0.40586)(cortisol_serum) + (0.12908)(cortisol_saliva) + (-0.21248)(mindfulness) + e

# dataset 4
data_sample_4 = read.csv("https://tinyurl.com/4f8thztv")
sum(is.na(data_sample_4))

# build a linear mixed effects model to predict pain on data_sample_3
#random intercept
mod_lmm2 = lmer(pain ~ age + pain_cat + mindfulness + (1 | hospital), data = data_sample_3)
summary(mod_lmm2)

#random slope
mod_lmm3 = lmer(pain ~ age + pain_cat + mindfulness + (1 | hospital), data = data_sample_3)
summary(mod_lmm3)

# use the regression equation obtained on data_sample_3 to predict pain in data_sample_4 #
# Y = 3.45051 + -0.05882(age) + 0.24141(sex) + -0.02342(STAI) + 0.08550(pain catastrophizing) + -0.21248(mindfulness) + 0.40586(cortisol serum)

# compute the variance in model 4 #
RSS = sum((data_sample_4$pain - predict(mod_lmm1))^2)
#TSS
mod_mean <- lm(pain ~ 1, data = data_sample_4)
TSS = sum((data_sample_4$pain - predict(mod_mean))^2)
R2 = 1 - (RSS/TSS)
R2 # -0.3576841

# build a new linear mixed effects model based on data_sample_3 predicting pain with most significant predictor#
# random intercept
mod_lmm2 = lmer(pain ~ pain_cat (1 | hospital), data = data_sample_3)
summary(mod_lmm2)

# random slope
mod_lmm3 = lmer(pain ~ pain_cat (pain_cat | hospital),
                data = data_sample_3)
summary(mod_lmm3)

# visual the fitted regression lines for each hopsital seperately
data_sample_3 = data_sample_3 %>%
  mutate(pred_int = predict(mod_lmm2), pred_slope = predict(mod_lmm3))

# random intercept
windows()
data_sample_3 %>%
  ggplot() + aes(y = pain, x = pain_cat, group = hospital) +
  geom_point(aes(color = hospital), size = 10) + geom_line(color = "red",
                                                           aes(y = pred_int, x = pain_cat)) + facet_wrap(~hospital, ncol = 2)
# random slope
windows()
data_sample_3 %>%
  ggplot() + aes(y = pain, x = pain_cat, group = hospital) +
  geom_point(aes(color = hospital), size = 10) + geom_line(color = "red",
                                                           aes(y = pred_slope, x = pain_cat)) + facet_wrap(~hospital, ncol = 2)

# visualization from most influential predictor which is pain_cat

#comparing intercept and slope models
cAIC(mod_lmm2)$caic # 667.468

cAIC(mod_lmm3)$caic # 622.1036

anova(mod_lmm2, mod_lmm3)

# R^2 for model 4
r.squaredGLMM(mod_lmm3)
# MR^2 - 0.2219242
# CR^2 - 0.2954185
