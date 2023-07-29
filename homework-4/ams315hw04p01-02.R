# Problem 1 ---------------------------------------------------------------

x=c(4,9,10,17,23,31)
y=c(13,27,42,61,78,86)
#plot(x,y)
xbar = mean(x)
ybar = mean(y)
s_xy = sum((x-xbar)*(y-ybar))
s_xx = sum((x-xbar)^2)
b1 = s_xy/s_xx
b0 = ybar - b1*xbar


# 1c ----------------------------------------------------------------------

y_pred = b0 + b1*12


# Problem 2 ---------------------------------------------------------------

n = length(y)
yhat = b0 + b1*x
s_E2 = sum((y-yhat)^2)/(n-2)
s_E = sqrt(s_E2)
alpha = 0.05

CI_95 = c(b1 - qt(1-alpha/2,4)*s_E*sqrt(1/s_xx) , b1 + qt(1-alpha/2,4)*s_E*sqrt(1/s_xx))

# Problem 3 ---------------------------------------------------------------

ss_reg = sum((yhat - ybar)^2)
F= ss_reg/s_E2

