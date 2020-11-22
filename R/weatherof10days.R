# Question 4

# The function to describe the process of only the weathter for 10 days

dayweather <- function (initial, days) {

    # declaring the function parameters
    sunny = rainy = p = numeric(0)

    y = initial  # The initial day conditions whether rainy or sunny

    # the random seed
    set.seed(13983)

    i=1

    # since the initial value is defined by the user, the while loop will run for days-1 times
    while(i<days){

    if  (y[i]==1) {
        y[i+1] = rbinom(1,1,0.85)         # If a day is sunny, the probability that the next day is sunny is 0.85
        if (y[i+1]==0) p=c(p,rexp(1,2))   # If a day is sunny, there can be no rain, p =0
      }
      else{
        y[i+1] = rbinom(1,1,0.65)         #  If a day is rainy, the probability that the next day is sunny is 1-0.35=0.65, since the Y models number of sunny days in our function
        if (y[i+1]==0) p=c(p,rexp(1,2))   #  If a day is rainy, the amount of rainfall accumulation in the city is governed by an exponential distribution (lambda=2)
      }
      i=i+1
    }
    sunny = length(which(y==1))           # The number of projected sunny days
    rainy = length(which(y==0))           # The number of projected sunny days

  return (list("trend"=y, "sunnydays"=sunny, "rainydays"=rainy, "projectedaccum"=p))
}
nowe = weather(1,10)
str(nowe)
