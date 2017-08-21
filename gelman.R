coeff = 4.7
se =4.3

magnitude =3

cauchy.p = 0.95

cauchy.scale= magnitude/tan(pi*(cauchy.p-0.5))

integrand  =function(x){dcauchy(x,scale = cauchy.scale)}


check =integrate(integrand,lower=-magnitude,upper=magnitude)$value/integrate(integrand,lower=-Inf,upper=Inf)$value


cauchy.scale=0.3
# lower limit of sex ratio change =0, upper =50
# http://www.casact.org/education/annual/2011/handouts/C9-Schmid.pdf

integrand1  =function(x){dcauchy(x,location=0,scale = cauchy.scale)*dnorm(x,mean=coeff,sd=se)}

posterior.positive =integrate(integrand1,lower=0,upper=50)$value/integrate(integrand1,lower=-50,upper=50)$value

posterior.positive.andless1 = integrate(integrand1,lower=0,upper=1)$value/integrate(integrand1,lower=0,upper=50)$value

integrand2  =function(x){dnorm(x,mean=0,sd = cauchy.scale)*dnorm(x,mean=coeff,sd=se)}

posterior.positivenorm =integrate(integrand2,lower=0,upper=50)$value/integrate(integrand1,lower=-50,upper=50)$value

posterior.positive.andless1norm = integrate(integrand2,lower=0,upper=1)$value/integrate(integrand1,lower=0,upper=50)$value


qcauchy(0.975,0,0.3)/0.3


qcauchy(0.05,0,0.5)/0.5
pnorm(0,4.7,4.3)
