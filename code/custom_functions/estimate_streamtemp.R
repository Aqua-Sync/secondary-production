# function to estimate stream temperatures from air temperature
# from eq. 2 in Mohseni, O., Erickson, T. R., & Stefan, H. G. (1997). A Non-Linear Regression Model for Weekly Stream Temperatures at 585 Gaging Stations in the US.
estimate_streamtemp = function(x){
  numerator = 26.2                           # values are median parameter values from Mohseni et al. Tbl X.
  denominator = 1 + (exp(0.18*(13.3 - x)))
  0.8 + (numerator/denominator)
}