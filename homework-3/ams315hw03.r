# Henry Shi
# AMS 315
# Homework 03
# 2022-02-24

# Problem 7 ---------------------------------------------------------------

# mean yield per acre
# H0: mu <= 500
# H1: mu > 500
mu = 500

# significance level
alpha = 0.025

# sample average
ybar = 553
# sample standard deviation (= population s.d.)
s = 124
sigma = s

# sample size (large enough to assume population is normal)
n = 36

# We can use the z-test with s = sigma = 124.

# Test statistic, zTest = 2.56
zTest = (ybar-mu)/(s/sqrt(n))

# Threshold z-value, zAlpha = 1.96
zAlpha = qnorm(1-alpha)

# zTest > zAlpha, i.e. Test statistic is within the rejection region.
# Therefore, we reject H0.