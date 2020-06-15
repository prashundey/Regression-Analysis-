library(mice)
library(MASS)
library(leaps)
library(knitr)

# CSV file containing 4 Environmental variables and 20 Generic variables
data <- read.csv('data.csv', header = TRUE)
NROW(data)
md.pattern(data)

# Modelling Evniromental Variables Vs. DV
# r2 = 0.4869035
ME <- lm(Y ~ E1+E2+E3+E4, data = data)
summary(ME)
summary(ME)$adj.r.squared
# Call:
#   lm(formula = Y ~ E1 + E2 + E3 + E4, data = data)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.89371 -0.48969  0.02537  0.51125  2.41039 
# 
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  7.94240    0.22126  35.897   <2e-16 ***
# E1           0.01050    0.01549   0.678    0.498    
# E2           0.28247    0.01531  18.447   <2e-16 ***
# E3           0.27166    0.01533  17.719   <2e-16 ***
# E4           0.28780    0.01505  19.120   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.7361 on 1102 degrees of freedom
# Multiple R-squared:  0.4888,	Adjusted R-squared:  0.4869 
# F-statistic: 263.4 on 4 and 1102 DF,  p-value: < 2.2e-16



# Modeling Effects of the Generic Variables
# two way interaction r2 = 0.508062
M_raw <- lm( Y ~ (E1+E2+E3+E4+G1+G2+G3+G4+G5+G6+G7+G8+G9+G10+G11+G12+G13+G14+G15+G16+G17+G18+G19+G20)^2, data = data)
plot(resid(M_raw) ~ fitted(M_raw), main='Residual Plot: Y ~ (E1...G20)^2')
summary(M_raw)
summary(M_raw)$adj.r.squared

# BOX COX lambda value
bc <- boxcox(M_raw, lambda = seq(-4,4))
best_lambda <- bc$x[which(bc$y==max(bc$y))]
best_lambda
# lambda ~ 2


# r2 = 0.5119079  -> BOX COX suggested trasnformation
M_trans <- lm(I(Y)^2 ~ (.)^2, data=data)
plot(resid(M_trans) ~ fitted(M_trans), main='Residual Plot: Y^2 ~ (E1...G20)^2')
summary(M_trans)
summary(M_trans)$adj.r.square

# r2 = 0.5024716
M_trans2 <- lm(log(Y) ~ (.)^2, data=data)



# Finding Most Significant Variables 
M <- regsubsets( model.matrix(M_trans)[,-1], I((data$Y)^2),
                 nbest = 1 , nvmax=5, 
                 method = 'forward', intercept = TRUE )
temp <- summary(M)

Var <- colnames(model.matrix(M_trans))
M_select <- apply(temp$which, 1, 
                  function(x) paste0(Var[x], collapse='+'))
kable(data.frame(cbind( model = M_select, adjR2 = temp$adjr2, BIC = temp$bic)),
      caption='Model Summary')
# |model                                     |adjR2             |BIC               |
# |:-----------------------------------------|:-----------------|:-----------------|
# |(Intercept)+E2:E4                         |0.33554579014252  |-439.512307850504 |
# |(Intercept)+E3+E2:E4                      |0.484425149221503 |-714.332767449744 |
# |(Intercept)+E3+E2:E4+G4:G16               |0.513221031637409 |-771.948407303816 |
# |(Intercept)+E3+E2:E4+G4:G16+G9:G14        |0.517804595530301 |-776.416098178087 |
# |(Intercept)+E3+E2:E4+E4:G11+G4:G16+G9:G14 |0.518889197932966 |-772.904462805179 |


# 1st order interaction
M_main <- lm( I(Y^2) ~ ., data = data)
temp <- summary(M_main)
kable(temp$coefficients[ abs(temp$coefficients[,4]) <= 0.001, ], caption='Sig Coefficients')
# |    | Estimate| Std. Error|   t value| Pr(>&#124;t&#124;)|
# |:---|--------:|----------:|---------:|------------------:|
# |E2  | 7.993953|  0.4257092| 18.777967|              0e+00|
# |E3  | 7.740230|  0.4239436| 18.257688|              0e+00|
# |E4  | 8.180551|  0.4171700| 19.609633|              0e+00|
# |G4  | 9.273894|  1.5395255|  6.023866|              0e+00|
# |G16 | 8.283791|  1.5300505|  5.414064|              1e-07|


# 2nd Order Interaction 
M_2nd <- lm( I(Y^2) ~ (.)^2.626263, data=data)
temp  <- summary(M_2nd)
kable(temp$coefficients[ abs(temp$coefficients[,4]) <= 0.01, ], caption='2nd Interaction')
# |        |   Estimate| Std. Error|   t value| Pr(>&#124;t&#124;)|
# |:-------|----------:|----------:|---------:|------------------:|
# |G19     |  69.423377|  23.951414|  2.898508|          0.0038512|
# |E2:G19  |  -3.177487|   1.225022| -2.593820|          0.0096639|
# |G5:G6   | -13.855659|   4.530682| -3.058184|          0.0023005|
# |G6:G16  |  12.840582|   4.594911|  2.794523|          0.0053211|
# |G6:G17  | -13.483692|   4.974232| -2.710708|          0.0068567|
# |G18:G19 | -13.679952|   5.147351| -2.657668|          0.0080242|


# r2 = 0.5165084
M_2stage <- lm( (Y^2) ~ (E3+E2+E4+G4+G16)^2, data=data)
summary(M_2stage)
temp$adj.r.squared
temp$coefficients[ abs(temp$coefficients[,3]) >= 1,]
confint(M_2stage, level = 0.99)
#       Estimate Std. Error  t value    Pr(>|t|)
# E3    7.083849   3.254346 2.176735 0.029714214
# E2    7.366875   3.372112 2.184647 0.029126955
# E4    8.744013   3.271745 2.672584 0.007639199
# E3:G4 1.442151   1.091042 1.321811 0.186508205


anova(M_2stage)
kable(anova(M_2stage), caption='ANOVA Table')
plot(resid(M_2stage) ~ fitted(M_trans), main='FINAL MODEL')

