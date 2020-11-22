

# Question 4: Repeat this 1000 times and return the average of all these simulations.



weathersimulate <- function (N, initial, days) {

  sunny = rainy = 0 ; p=numeric(0) ; y = initial

  # Assume user defined conditions on the initial day
  mylist=mylist2=rep(list(initial),N)

  set.seed(1998)

  for(j in 1:N){
    i=1
    while(i<days){                      # since the initial value is defined by the user, the while loop will run for days-1 times
      if  (y[i]==1) {
        y[i+1] = rbinom(1,1,0.85)       # If a day is sunny, the probability that the next day is sunny is 0.85
        if (y[i+1]==0) p=c(p,rexp(1,2))
      }
      else{
        y[i+1] = rbinom(1,1,0.65)       # If a day is rainy, the probability that the next day is sunny is 1-0.35=0.65, since the Y models number of sunny days in our function
        if (y[i+1]==0) p=c(p,rexp(1,2))
      }
      mylist[[j]][i+1] = y[i+1]
      i=i+1
    }
    sunny[j] = length(which(y==1))      # The number of projected sunny days
    rainy[j] = length(which(y==0))      # The number of projected rainy days
    mylist2[[j]] = p
  }
return (list("sunnydays"=sunny, "rainydays"=rainy, "meanaccum"=p))
}

nowe = weathersimulate(1000,1,10)

str(nowe$meanaccum)
