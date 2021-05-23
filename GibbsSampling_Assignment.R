#import and view data
bodtmp = bodytemp <- read.csv("~/M440B_HW/bodytemp.csv")
bodtmp

#separates temperature based on sex.
bodtmpus <- unstack(bodtmp,temperature~gender)
bodtmpus

#here is how to calculate the mean of the male and female temperatures.
avg.male = mean(bodtmpus[[1]])
avg.female = mean(bodtmpus[[2]])

#here theta and ksi are initialized.
theta <- 0
ksi <- 0

#first value of theta (MALES)
theta[1] = mean(bodtmpus[[1]])

#loop provided in instructions
k = 1
while(k<1001){
  ksi[k] <-rgamma(1,65/2+2,rate=(32.5*(theta[k]-avg.male)^2)+32*var(bodtmpus[[1]])+2)
  theta[k+1] <- rnorm(1, (ksi[k]*65*avg.male+394.4)/(ksi[k]*65+4), sqrt(1/(ksi[k]*65+4)))
  k <- k+1
}

#plots for males.
par(mfcol=c(2,2))
dthetaMales<-density(theta[501:1000])
dksiMales <-density(ksi[501:1000])
plot(dthetaMales, xlab = 'Temperature', ylab = 'Density', main = 'Male Temperature Average')
plot(dksiMales, xlab = 'Temperature', ylab = 'Density', main = 'Male Temperature Precision')

#summary statistics for male theta
summary(theta[501:1000])

#second value of theta (FEMALES)
theta[1] = mean(bodtmpus[[2]])

#loop provided in instructions copy-and-pasted
k = 1
while(k<1001){
  ksi[k] <-rgamma(1,65/2+2,rate=(32.5*(theta[k]-avg.female)^2)+32*var(bodtmpus[[2]])+2)
  theta[k+1] <- rnorm(1, (ksi[k]*65*avg.female+394.4)/(ksi[k]*65+4), sqrt(1/(ksi[k]*65+4)))
  k <- k+1
}

dthetaFemales <-density(theta[501:1000])
dksiFemales <- density(ksi[501:1000])
plot(dthetaFemales, xlab = 'Temperature', ylab = 'Density', main = 'Female Temperature Average')
plot(dksiFemales, xlab = 'Temperature', ylab = 'Density', main = 'Female Temperature Precision')

#summary statistics for fem theta
summary(theta[501:1000])

#reset values
theta <- 0
ksi <- 0

#EXTRA CREDIT:
#MALES:
ksi[1] = 1/var(bodtmpus[[1]])


k = 1
while(k<1001){
  theta[k] <-rnorm(1, (ksi[k]*65*avg.male+394.4)/(ksi[k]*65+4), sqrt(1/(ksi[k]*65+4)))
  ksi[k+1] <-rgamma(1,65/2+2,rate=(32.5*(theta[k]-avg.male)^2)+32*var(bodtmpus[[1]])+2)
  k <- k+1
}

par(mfcol=c(2,2))
dtksiMales<-density(ksi[501:1000])
dthetaMales <-density(theta[501:1000])
plot(dksiMales, xlab = 'Temperature', ylab = 'Density', main = 'Male Temperature Precision')
plot(dthetaMales, xlab = 'Temperature', ylab = 'Density', main = 'Male Temperature Average')

#FEMALES:
ksi[1] = 1/var(bodtmpus[[2]])

k = 1
while(k<1001){
  theta[k] <-rnorm(1, (ksi[k]*65*avg.male+394.4)/(ksi[k]*65+4), sqrt(1/(ksi[k]*65+4)))
  ksi[k+1] <-rgamma(1,65/2+2,rate=(32.5*(theta[k]-avg.male)^2)+32*var(bodtmpus[[1]])+2)
  k <- k+1
}

dtksiFemales<-density(ksi[501:1000])
dthetaFemales <-density(theta[501:1000])
plot(dksiFemales, xlab = 'Temperature', ylab = 'Density', main = 'Female Temperature Precision')
plot(dthetaFemales, xlab = 'Temperature', ylab = 'Density', main = 'Female Temperature Average')

