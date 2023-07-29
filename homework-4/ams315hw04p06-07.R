# Henry Shi
# AMS 315
# Homework 4
# 2022-03-21


# Problem 6 ---------------------------------------------------------------


# Part a ------------------------------------------------------------------


# x1 = upstream dissolved oxygen readings
# x2 = downstream dissolved oxygen readings
x1 = c(5.2, 4.8, 5.1, 5.0, 4.9, 4.8, 5.0, 4.7, 4.7, 5.0, 4.6, 5.2, 5.0, 4.9, 4.7)
x2 = c(3.2, 3.4, 3.7, 3.9, 3.6, 3.8, 3.9, 3.6, 4.1, 3.3, 4.5, 3.7, 3.9, 3.8, 3.7)

# mean dissolved oxygen readings
x1bar = mean(x1)
x2bar = mean(x2)

# d = difference (x1-x2) in mean dissolved oxygen readings
d = x1bar - x2bar

# Answer: dbar = 1.17


# Part b ------------------------------------------------------------------


# H0: mu1 - mu2 = 0
# Ha: mu1 - mu2 != 0
alpha = 0.05

# n1 and n2
n1 = length(x1)
n2 = length(x2)

# Variances of oxygen readings 
s1 = sd(x1)
s2 = sd(x2)

# Degrees of freedom
c = (s1^2/n1) / (s1^2/n1 + s2^2/n2)
df = ( (n1-1)*(n2-1) ) / ( (1-c)^2 * (n1-1) + c^2*(n2-1) )

# Test statistic
t = (x1bar - x2bar - 0) / sqrt(s1^2/n1 + s2^2/n2)

# Rejection region
t_reject = qt(1-alpha/2,df)

# t = 12.18
# t_reject = 2.07
# |t| > t_reject

# We have |t| > t_alpha/2 at the given df, so we reject H0.
# Therefore, there is a significant difference in mean oxygen content
# for locations above and below the town.


# Part c ------------------------------------------------------------------


# 99% confidence interval for mu1 - mu2

# (x1bar - x2bar) +/- t_alpha/2 * sqrt(s1^2/n1 + s2^2/n2)

alpha = 0.01
t_alpha2 = qt(1-alpha/2,df)

# Lower bound of confidence interval
CI_99_LB = (x1bar - x2bar) - t_alpha2 * sqrt(s1^2/n1 + s2^2/n2)

# Upper bound of confidence interval
CI_99_UB = (x1bar - x2bar) + t_alpha2 * sqrt(s1^2/n1 + s2^2/n2)

# Answer:
# 99% confidence interval for difference in mean dissolved oxygen readings:
# (0.897, 1.436)


# Problem 7 ---------------------------------------------------------------


# Part a ------------------------------------------------------------------

# Portfolios 1 and 2
p1 = c(130, 135, 135, 131, 129, 135, 126, 136, 127, 132)
p2 = c(154, 144, 147, 150, 155, 153, 149, 139, 140, 141)

# Variances of portfolios 1 and 2
var_p1 = var(p1)
var_p2 = var(p2)

# var_p1 = 12.93, var_p2 = 35.51
# Therefore, portfolio 2 appears to have a higher risk than portfolio 1

# H0: var_p2 <= var_p1
# Ha: var_p2 > var_p1
alpha = 0.05

# test statistic F
F = var_p2 / var_p1

# df1=n1-1, df2=n2-1
n1 = length(p1)
n2 = length(p2)
df1 = n1-1
df2 = n2-1

# rejection region
F_reject = qf(1-alpha,df2,df1)

# F = 2.75, F_reject = 3.18.
# F < F_reject, so we fail to reject H0.
# There is not sufficient evidence that portfolio 2 appears to have a
# higher risk than portfolio 1.


# Part b ------------------------------------------------------------------


# p-value of test

p = 1-pf(F,df2,df1)

# Answer: p = 0.074

# 95% confidence interval on the ratio of standard deviations s_p2/s_p1

# Lower and upper F-scores
F_L = qf(alpha,df2,df1)
F_U = qf(1-alpha,df2,df1)

CI_lower = sqrt(var_p2/var_p1 * F_L)
CI_upper = sqrt(var_p2/var_p1 * F_U)

# Answer: 95% confidence interval is (0.93, 2.95)