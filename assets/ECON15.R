library(tidyverse)
library(wooldridge)
library(broom)
library(magrittr)
library(clubSandwich)
library(modelsummary)
library(estimatr)
library(plm)

df <- as_tibble(wagepan)
df %<>% mutate(year=as.factor(year))
df %<>% rename(id = nr)

pdim(df)

df.within <- df %>% select(id,year,educ,married,union,rur) %>%
  group_by(id) %>%
  summarize(
    mean.edu = mean(educ),
    var.edu = var(educ),
    mean.marr = mean(married),
    var.marr = var(married),
    mean.union = mean(union),
    var.union = var(union),
    mean.rural = mean(rur),
    var.rural = var(rur)
  )
df.within %>% datasummary_skim()

# 1. no within-person variance in the educ variable
# there is within-person variance in the 
# married, union, and rural variables
# 2. a positive within-person variance,
# it means that the values of that variable
# vary within individuals over time
# i.e. a positive within-person variance for the married
# variable indicates that individuals in the sample
# are not consistently married or unmarried
# 3. it is important b/c positive within-person variance 
# can affect how the variable is analyzed and interpreted
# if there is within-person variance in a variable,
# it is subject to change or fluctuation over time,
# which can inform descisions aimed at modifying the variable

est.pols <- lm_robust(lwage ~ educ + black + hisp + exper + I(exper**2) + married + union + rur + year,
                      data = df, clusters=id)
summary(est.pols)

# 4. being married is associated with a 0.126008
# increase in the natural logarithm of wage

est.re <- plm(lwage ~ educ + black + hisp + exper + I(exper**2) + married + union + rur + year,
              data = df, index = c("id","year"), model = "random")
summary(est.re)

# 5. θ in the RE model is 0.6367. 
# the random effects estimates are closer 
# to the fixed effects estimates than if θ was closer to 1.

est.fe <- lm_robust(lwage ~ I(exper**2) + married + union + rur + year,
                    data = df, fixed_effects = ~id)
summary(est.fe)

# 6. the variables are being absorbed by the dummy variables

clust.re <- coef_test(est.re, vcov = "CR1", cluster = "individual")
clust.re.SE <- clust.re$SE
names(clust.re.SE) <- names(est.re$coefficients)

modelsummary(list("POLS"=est.pols,"RE"=est.re,"FE"=est.fe),
             statistic_override=list(sqrt(diag(est.pols$vcov)),clust.re.SE,sqrt(diag(est.fe$vcov))),
             output="markdown")
# 7. coefficient on union indicates the effect of union membership
# on ln(wage) it is 0.182, so union members have 18.2% 
# higher wages than non-union members
# For the fixed effects model, the coefficient is 0.079,
# so within-person changes in union membership are associated
# with a 7.9% increase in wages
# For the random,  coefficient is 0.107, 
# so union members have 10.7% higher wages than non-union members
# 8. the pooled OLS and RE estimators should be comparable when addressed
# the fixed effects estimator, which only includes
# within-person variation, yields substantially different estimates