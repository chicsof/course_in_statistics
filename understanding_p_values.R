# the p value if used to decide whether an Ho should be accepted or rejected. It is releted to the propability of the event happening,
#assuming it is random (h0 is true) BUT it does not always equal that.

#p-value is : propability of event happening at random + propability of all other events of an equal chance + propability of all events that are less 
#likly to happen

#lets take the example of tossing a coin and landing 5 heads on a row, what is the p-value
posibleOutcomes<- 2^5
posibleOutcomes
P4heads <- 1/posibleOutcomes
P4heads
#the propability of landing 5 tails in a row is equally likly
P4tails <- 1/posibleOutcomes
#there is no other outcome less likly so we can get the p value from
pValue <- P4heads + P4tails
pValue 
#the propability was 0.031, however the p-value is 0.03, which is less than the usuall threshold of 0.05 and so we chould accept the H0
#this chould occure at random

#what whould the p value be if we goten 4 heads and 1 tail
WaysForheads1tail <- factorial(5)/factorial(4)*factorial(5-4)
P4heads1tail <- WaysForheads1tail/posibleOutcomes
P4heads1tail
#the propability of something equally likly having (4 tails and 1 head)
P4tails1head <- P4heads1tail
#propability of something less likly happening (5 heads or 5 tails), is already calculated so p whould be:
pValue <- P4heads1tail + P4tails1head + P4heads + P4tails
pValue#also quite hight so event chould easily occure at random

#what if we were dealing with continious distributions, e.g hight measurments?
#in this case we use a density function where the aeria under the graph represents the propability for that x1 to x2 occuring.

#if we had the following height sample, what is the propability of a person having a height from 1.60 to 1.68
heights <- c(1.50, 1.45, 1.54, 1.60,1.61,1.62,1.66,1.64,1.66, 1.66, 1.66, 1.66, 1.69, 1.70, 1.71, 1.72, 1.73, 1.74, 1.75, 1.80, 1.85, 1.90)

#this how the graph looks like, where the blue line represents the pdf (propability density function)
#assuming the heights of the population follows a normal distribution
h<-hist(heights, breaks=10, col="red") 
xfit<-seq(min(heights),max(heights),length=40) 
yfit<-dnorm(xfit,mean=mean(heights),sd=sd(heights)) 
yfit <- yfit*diff(h$mids[1:2])*length(heights) 
lines(xfit, yfit, col="blue", lwd=2)

# generally we can get the aeria under the graph by calculating the integral from x1 to x2, in a normal distribution we can use the z scores instead
#to calculate the aeria from an x point to the mean, by relating to the standart normal distribution. We also know that the total aeria which represents 
#all the propabilities whould be one, also the aeia from the left or right to the mean whould be 0.5 due to te symetry.

#so in order to get what we are looking for we need to add the aeria from x1 to the mean and the aeria from x2 to the mean
m <- mean(heights)
s <- sd(heights)
m
#from x1 to m
#the zcrore is
z1 <- (1.60-m)/s
#the aeria from z1 to end
aeriaX1ToEnd <- pnorm(z1)
aeriaX1ToEnd
#knowing that the aria ftom the mean to the end is 0.5, then the aeria from the mean to x1 is
aeriaX1 <- 0.5 - aeriaX1ToEnd
aeriaX1
#from x2 to m
z2 <- (1.68-m)/s
aeriaX2ToEnd <- pnorm(z2)
aeriaX2ToEnd
aeriaX2 <-  aeriaX2ToEnd -0.5
aeriaX2
#since the one x is on the left of the mean and the other on the right we can just add them, otherwise we whould have to also remove a common aeria
TotalAeria <- aeriaX1+aeriaX2
TotalAeria
#that is equal to 30% propability that a persons hight is exacly inbetween 1.60 to 1.68
#to measure the p value we need to also measure the propability of someone having a height less that 1.60
aeriaX1ToEnd
#and the propability of someone having a height greater than 1.68
aeriaX2ToEnd
#and add them up
pValue <- TotalAeria + aeriaX1ToEnd + aeriaX2ToEnd
pValue
#that is a large p value and therefor shows that having a height close to the mean is not uncommon






