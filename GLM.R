#making data fram
mydata = data.frame(
  "Year" = c(seq(1, 13)),
  "Cases" = c(12, 14, 33, 50, 67, 74, 123, 141, 165, 204, 253, 246, 240)
)


#Building Models with various link functions
mod_gaus1 = glm(Cases ~ Year, family = gaussian(link = "identity"), mydata)
summary(mod_gaus1)
mod_gaus2 = glm(Cases ~ Year, family = gaussian(link = "log"), mydata)
summary(mod_gaus2)
mod_gaus3 = glm(Cases ~ Year, family = gaussian(link = "inverse"), mydata)
summary(mod_gaus3)

mod_gam1 = glm(Cases ~ Year, family = Gamma(link = "identity"), mydata)
summary(mod_gam1)
mod_gam2 = glm(Cases ~ Year, family = Gamma(link = "log"), mydata)
summary(mod_gam2)
mod_gam3 = glm(Cases ~ Year, family = Gamma(link = "inverse"), mydata)
summary(mod_gam3)

mod_poiss1 = glm(Cases ~ Year, family = poisson(link = "log"), mydata)
summary(mod_poiss1)
mod_poiss2 = glm(Cases ~ Year, family = poisson(link = "sqrt"), mydata)
summary(mod_poiss2)


mod_invgaus1 = glm(Cases ~ Year, family = inverse.gaussian(link = "identity"), mydata)
summary(mod_invgaus1)
mod_invgaus2 = glm(Cases ~ Year, family = inverse.gaussian(link = "log"), mydat5a)
summary(mod_invgaus2)

mod_quas1 = glm(Cases ~ Year, family = quasi(link = "identity"), mydata)
summary(mod_quas1)
mod_quas2 = glm(Cases ~ Year, family = quasi(link = "inverse"), mydata)
summary(mod_quas2)
mod_quas3 = glm(Cases ~ Year, family = quasi(link = "log"), mydata)
summary(mod_quas3)
mod_quas4 = glm(Cases ~ Year, family = quasi(link = "1/mu^2"), mydata)
summary(mod_quas4)
mod_quas5 = glm(Cases ~ Year, family = quasi(link = "sqrt"), mydata)
summary(mod_quas5)

mod_quaspoiss = glm(Cases ~ Year, family = quasipoisson(), mydata)
summary(mod_quaspoiss)

library(fitdistrplus)
library(logspline)

#checking distribution of data
descdist(mydata$Cases, discrete = FALSE)

#fitting different distributions for inspection
fe = fitdist(mydata$Cases, 'exp')
fg = fitdist(mydata$Cases, 'gamma')
fln = fitdist(mydata$Cases, 'lnorm')
par(mfrow=c(2, 2))
denscomp(list(fln,fe,fg), legendtext=c("lnorm","exponential", "gamma"))
qqcomp(list(fln,fe,fg), legendtext=c("lnorm","exponential", "gamma"))
cdfcomp(list(fln,fe,fg), legendtext=c("lormom","exponential","gamma"))
ppcomp(list(fln,fe,fg), legendtext=c("lnorm","exponential", "gamma"))
#Of the above, the gamma seems like the best fit

fw = fitdist(mydata$Cases, 'weibull')
par(mfrow=c(2, 2))
denscomp(list(fw,fg), legendtext=c("weibull", "gamma"))
qqcomp(list(fw,fg), legendtext=c("weibull","gamma"))
cdfcomp(list(fw,fg), legendtext=c("weibull","gamma"))
ppcomp(list(fw,fg), legendtext=c("weibull", "gamma"))

#Further ananlysis of gamma distribution
1-pchisq(deviance(mod_invgaus1),df.residual(mod_invgaus1)) 
1-pchisq(deviance(mod_invgaus2),df.residual(mod_invgaus2))
1-pchisq(deviance(mod_invgaus3),df.residual(mod_invgaus3))
#Given the extremely high p-values for each of the models, there is no evidence of lack of fit

anova(mod_invgaus1,test="Chi")
#Here, we see that the variable 'Year' is clearly significant
anova(mod_invgaus2,test="Chi")
#Here, we see that the variable 'Year' is clearly significant
anova(mod_gam3,test="Chi")
#Here, we see that the variable 'Year' is clearly significant

#Testing quadratic term for completeness
mod11 = glm(Cases ~ Year+I(Year^2), family = inv.gaussian(link = "identity"), mydata) 
anova(mod_invgaus1,mod11,test="Chi")
#we can see there is no need for a quadratic term in the model

mod21 = glm(Cases ~ Year+I(Year^2), family = inv.gaussian(link = "log"), mydata) 
anova(mod_invgaus2,mod21,test="Chi")
#we can see there is no need for a quadratic term in the model


#predicting the 13th year with each model to see which comes closest to the actual value

#first, retraining the models on only 12 data points
testData=mydata[1:12,]

test_mod1 = glm(Cases ~ Year, family = inverse.gaussian(link = "identity"), testData)
summary(test_mod1)
test_mod2 = glm(Cases ~ Year, family = inverse.gaussian(link = "log"), testData )
summary(test_mod2)
#Both P-values indicate high model significance

#Predicting 13th year (1994)
new_data = data.frame(Year=13)
predict(test_mod1, new_data )
predict(test_mod2, new_data )
#Both predictions for the cases in the 13th year are badly off, particularly using the log link

