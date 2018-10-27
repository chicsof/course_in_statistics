##########################################################################################
################################### BASIC PROPABILITY ####################################
##########################################################################################

#we are going to try an examine if the propability of getting heads or tails when tossing a coin if 0.5 by
#conducting n bernouli trials, summing the results of heads or tails and seing if they are about half

#using the random number generator
N <- 100
#will return true (1) if the outpout from 0 to 1 is >0.5
flips <- runif(N, min=0, max=1)>0.5 
#we sum them up, it will add all the ones for true and zero's for false of the above list of flips
#so we expect half of them to be true and so to get a result close to 0.5
#we can increase the N to get closer
sum(flips)/N

#using sample function to perform a simulation
sample.space <- c(0,1)
flips <- sample(sample.space, N, replace = TRUE, prob = c(0.5, 0.5))
sum(flips)/N

##########################################################################################
######################### BIRTHDAY PROBLEM ###############################################
##########################################################################################

#finding the propability that at least two people having the same birthday from a sample of n people.
#we ignore leap years, seasonal varietions


#All the propable birthdays of the people whould be 365^k
#if everyones birthday was unique (no one has the same birthday),
#there whould be factorial(365)/factorial(365-k) ways for that to be true, from the permutations formula
#so the propability that at least two people have the same birthday whould be 1 - (propability no one having the same birthday)


#this library allows us to use larger numbers
install.packages("Rmpfr")
library("Rmpfr")

#create function to calculate permutations, this is the standart formula however we use the rmpfr library
#that will let us deal with larger numbers
perm <- function(n, k) {
  factorialMpfr(n)/factorialMpfr(n-k)
}

#this is our n
count <- 100
#double so we can plot it
p <- double(count)

for(k in 1:count){
  #the formula from above
  d<-(1 - perm(365, k)/ (mpfr(365, precBits = 1024)^k))*100
  p[k] <- asNumeric(d)
}

plot(1:count, p, xlab="number of people", ylab ="propability in %")










