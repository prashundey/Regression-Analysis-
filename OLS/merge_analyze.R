# Merging of files
# Each has 679 elements
IV <- read.csv("IV.csv")
DV <- read.csv("DV.csv")
IV <- IV[order(IV$ID),]
DV <- DV[order(DV$ID),]

all_data <- cbind(IV, DV)
all_data <- all_data[,c(1,2,4)]
NROW(all_data)

# Analysis of Count (Pre-Imputation) --- MANUAL --- Can use Mice md.pattern instead
# IV non-missing: 627
# DV non-missing: 524
# IV and DV non-missing: 483

# IV Missing Total: 52 (OnlyIV + Both Missing)
# DV Missing Total: 155 (OnlyDV + Both Missing)

# Eithe missing: 196
# Only IV Missing: 41
# Only DV Missing: 144
# Both Missing: 11
if (any(is.na(all_data$IV) == TRUE) | any(is.na(all_data$DV) == TRUE)) {
  nonMissingIV <- sum(is.na(all_data$IV) == FALSE)
  nonMissingIV
  
  nonMissingDV <- sum(is.na(all_data$DV) == FALSE)
  nonMissingDV
  
  nonMissing_IV_and_DV <- sum(is.na(all_data$IV) == FALSE & is.na(all_data$DV) == FALSE)
  nonMissing_IV_and_DV
  
  missingIV <- sum(is.na(all_data$IV))
  missingIV
  
  missingDV <- sum(is.na(all_data$DV))
  missingDV
  
  either_missing <- sum(is.na(all_data$IV) | is.na(all_data$DV))
  either_missing
  
  onlyIV <- sum(is.na(all_data$IV) & is.na(all_data$DV) == FALSE)
  onlyIV
  
  onlyDV <- sum(is.na(all_data$IV) == FALSE & is.na(all_data$DV))
  onlyDV
  
  both_missing <- sum(is.na(all_data$IV) & is.na(all_data$DV))
  both_missing
}

summary(all_data)
library(mice)
md.pattern(all_data)
#      ID IV  DV    
# 483  1  1   1   0
# 144  1  1   0   1
# 41   1  0   1   1
# 11   1  0   0   2
#      0 52 155 207

# Drop ID entries that have both IV and DV missing
# Count: 668 (Total Count - Both IV & DV missing)
data_incomplete <- all_data[is.na(all_data$IV) == FALSE | is.na(all_data$DV)== FALSE,]
NROW(data_incomplete)

# Running imputation via linear regression using bootstrap
imp <- mice(data_incomplete, method = "norm.boot", printFlag = FALSE)
data_complete <- complete(imp)

any(is.na(data_complete$IV)) == TRUE
any(is.na(data_complete$IV)) == TRUE
md.pattern(data_complete)

# Regression Analysis on Imputed Dated
reg_analysis <- lm(DV ~ IV, data = data_complete)
cor(data_complete$IV, data_complete$DV)
# R-value: 0.7239673
summary(reg_analysis)
# Call:
# lm(formula = DV ~ IV, data = data_complete)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -25.5970  -5.1976   0.1281   5.3694  25.6209 
# 
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  12.1495     0.9601   12.65   <2e-16 ***
# IV            4.0506     0.1496   27.08   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 7.961 on 666 degrees of freedom
# Multiple R-squared:  0.5241,	Adjusted R-squared:  0.5234 
# F-statistic: 733.5 on 1 and 666 DF,  p-value: < 2.2e-16


library(knitr)
kable(anova(reg_analysis), caption='ANOVA Table')
# |          |  Df|   Sum Sq|     Mean Sq|  F value| Pr(>F)|
# |:---------|---:|--------:|-----------:|--------:|------:|
# |IV        |   1| 46486.83| 46486.83274| 733.5379|      0|
# |Residuals | 666| 42206.72|    63.37346|       NA|     NA|

plot(data_complete$DV ~ data_complete$IV, main = 'Scatter : DV ~ IV', xlab = 'IV', ylab = 'DV', pch = 20)
abline(reg_analysis, col = 'red', lty = 3, lwd = 2)
legend('topleft', legend='Estimated Regression Line', lty = 1, lwd = 2, col = 'red')


# Confidence Intervals for Slope and Intercept
confint(reg_analysis, level = 0.95)
#                2.5 %    97.5 %
# (Intercept) 10.264220 14.034750
# IV           3.756984  4.344312
confint(reg_analysis, level = 0.99)
#               0.5 %    99.5 %
# (Intercept) 9.669222 14.629748
# IV          3.664302  4.436994


