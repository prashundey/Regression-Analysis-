install.packages('alr3')
library(alr3)

# Count: 401
# -> All data observed
data <- read.csv('data.csv', header = TRUE)
md.pattern(data) 
plot(data$y ~ data$x, main = '', xlab = 'x', ylab = 'y', pch = 20)
reg_analysis <- lm(y ~ x, data = data)
abline(reg_analysis, col = 'red', lty = 3, lwd = 2)
cor(data$x, data$y)
# r = -0.747836
summary(reg_analysis)
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  3.51576    0.02847   123.5   <2e-16 ***
# x           -0.39764    0.01767   -22.5   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.0816 on 399 degrees of freedom
# Multiple R-squared:  0.5593,	Adjusted R-squared:  0.5582 
# F-statistic: 506.3 on 1 and 399 DF,  p-value: < 2.2e-16

sqrt(.5593)
confint(reg_analysis, level = 0.99)
pureErrorAnova(reg_analysis)

# Transformations
x <- data$x
y <- data$y

transform <- function(a,b) {
  data_trans <- data.frame(xtrans=a, ytrans= b)
  plot(data_trans$y ~ data_trans$x, main = 'Scatter : DV ~ IV', xlab = 'IV', ylab = 'DV', pch = 20)
  reg <- lm(ytrans ~ xtrans, data = data_trans)
  summary(reg)
}

#r2 = .5593
transform(x,y) 
#r2 = .5545
transform(x, log(y))
#r2 = .5563
transform(1, y)
#r2 = .5847
transform(1/x, y)
#r2 = .578
transform(1/x, log(y))
#r2 = .6027
transform(1/x, exp(2*y))
#r2 = .5904
transform(1/x, y^2)
#r2 = .614
transform(x^-5, y^9)
#r2 = .578
transform(1/x, log(sqrt(y)))
#r2 = .5676
transform(sqrt(x), y)
#r2 = .578



# TOP 2 Transformations
data_trans1 <- data.frame(xtrans=1/x, ytrans= exp(2*y))
reg1 <- lm(ytrans ~ xtrans, data = data_trans1)
summary(reg1)
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -128.701  -32.575   -0.135   30.462  156.709 
# 
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -100.58      17.62  -5.708 2.24e-08 ***
# xtrans        669.18      27.20  24.602  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 51.72 on 399 degrees of freedom
# Multiple R-squared:  0.6027,	Adjusted R-squared:  0.6017 
# F-statistic: 605.3 on 1 and 399 DF,  p-value: < 2.2e-16

library(knitr)
pureErrorAnova(reg1)
kable(anova(reg1), caption='ANOVA Table')
# |          |  Df|  Sum Sq|     Mean Sq|  F value| Pr(>F)|
# |:---------|---:|-------:|-----------:|--------:|------:|
# |xtrans    |   1| 1619173| 1619173.288| 605.2641|      0|
# |Residuals | 399| 1067386|    2675.152|       NA|     NA|

confint(reg1, level = 0.99)
#                 0.5 %    99.5 %
# (Intercept) -146.1904 -54.97209
# xtrans       598.7779 739.57630

plot(data_trans1$y ~ data_trans1$x, main = '', xlab = '1/x', ylab = 'exp(2y)', pch = 20)
abline(reg1, col = 'red', lty = 3, lwd = 2)
legend('topleft', legend='Estimated Regression Line', lty = 1, lwd = 2, col = 'red')



data_trans2 <- data.frame(xtrans=x^-5, ytrans= y^9)
reg2 <- lm(ytrans ~ xtrans, data = data_trans2)
summary(reg2)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -9386.9 -2141.2  -189.1  2254.3 11591.1 
# 
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   8601.0      297.1   28.95   <2e-16 ***
# xtrans       45094.1     1789.9   25.19   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 3532 on 399 degrees of freedom
# Multiple R-squared:  0.614,	Adjusted R-squared:  0.613 
# F-statistic: 634.7 on 1 and 399 DF,  p-value: < 2.2e-16

library(knitr)
kable(anova(reg2), caption='ANOVA Table')
# |          |  Df|     Sum Sq|    Mean Sq|  F value| Pr(>F)|
# |:---------|---:|----------:|----------:|--------:|------:|
# |xtrans    |   1| 7916264095| 7916264095| 634.6975|      0|
# |Residuals | 399| 4976526991|   12472499|       NA|     NA|
pureErrorAnova(reg2)
confint(reg2, level = 0.99)


plot(data_trans2$y ~ data_trans2$x, main = '', xlab = 'x^-5', ylab = 'y^9', pch = 20)
abline(reg2, col = 'red', lty = 3, lwd = 2)
legend('topleft', legend='Estimated Regression Line', lty = 1, lwd = 2, col = 'red')


data_trans <- data.frame(xtrans=x, ytrans= y)
data_trans <- data_trans2
hist(data_trans$xtrans)

maxx = max(data_trans$xtrans) -.02
maxx
minn = min(data_trans$xtrans) +.02
minn
groups <- cut(data_trans$xtrans,breaks=c(-Inf,seq(minn, maxx,by = 0.02),Inf))
table(groups)


a <- ave(data_trans$xtrans, groups)
data_bin <- data.frame(x=a, y=data_trans$ytrans)
plot(data_bin$y ~ data_bin$x, main = 'Scatter : DV ~ IV', xlab = 'IV', ylab = 'DV', pch = 20)


fit_b <- lm(y ~ x, data = data_bin)
pureErrorAnova(fit_b)
# Analysis of Variance Table
# 
# Response: y
#               Df     Sum Sq    Mean Sq  F value    Pr(>F)    
# x              1 7926757813 7926757813 699.1070 < 2.2e-16 ***
# Residuals    399 4966033273   12446199                       
#  Lack of fit  16  623424694   38964043   3.4365 9.787e-06 ***
#  Pure Error  383 4342608579   11338404                       
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1






