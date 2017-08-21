retrodesign <- function(A, s, alpha=.05, df=Inf, n.sims=10000){
 
  
   z <- qt(1-alpha/2, df) 
  p.hi <- 1 - pt(z-A/s, df) 
  p.lo <- pt(-z-A/s, df)  
  power <- p.hi + p.lo
  typeS <- p.lo/power
  estimate <- A + s*rt(n.sims,df)
  significant <- abs(estimate) > s*z
  exaggeration <- mean(abs(estimate)[significant])/A
  return(list(power=power, typeS=typeS, exaggeration=exaggeration))
}

retrodesign(0.1,3.3,df=3)

A =0.1
s =3.3

pnorm(z-A/s)

mean(estimate)
significant
estimate

 kk = abs(estimate)[significant]
 str(estim)
 [significant]
 
 mean(abs(estimate)[significant])
 summary(significant)
 
 
 
 
 
 
 Bf(sd=32.2,obtained=5.47,dfdata=119,meanoftheory=13.3,sdtheory=4.93,dftheory=72)
 
 
 Bf<-function(sd , obtained, dfdata , meanoftheory=0, sdtheory=1, dftheory = 1,tail=2)
 {           
   area <- 0
   normarea <- 0
   
   theta <- meanoftheory - 10 * sdtheory
   incr <- sdtheory / 200
   for (A in -2000:2000){
     theta <- theta + incr
     dist_theta <- dnorm(theta, meanoftheory, sdtheory)
     dist_theta <- dt((theta-meanoftheory)/sdtheory, df=dftheory)
     if(identical(tail, 1)){
       if (theta <= 0){
         dist_theta <- 0
       } else {
         dist_theta <- dist_theta * 2
       }
     }
     height <- dist_theta * dt((obtained-theta)/sd, df = dfdata)
     area <- area + height * incr
     normarea <- normarea + dist_theta*incr
   }
   LikelihoodTheory <- area/normarea
   Likelihoodnull <- dt(obtained/sd, df = dfdata)
   BayesFactor <- LikelihoodTheory / Likelihoodnull 
   BayesFactor
 }
 
 
 str(significant)
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 tau = function(alpha,powr){
   # A Gaussian test at level alpha
   # has power powr for Y ~ N( 1, tau^2 ).
   # Solve for tau using noncentral chisquare.
   aux = function(tau){
     1-powr - pchisq(qchisq(1-alpha,1),1,ncp=1/tau^2)
   }
   ur = uniroot( aux, lower=10^-9,upper=10^6,
                 tol = .Machine$double.eps^0.9)
   ur$root
 }


 
 tau(0.05,0.06)