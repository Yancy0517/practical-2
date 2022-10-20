##------Names-----------
## Jialong He (s2281875)
## Linshen Shu (s2317223)
## Heyu Nie (s2404675)

##---Address of github repo-----
# https://github.com/Yancy0517/practical-2.git
 
##----Team member contribution----
## Linshen was responsible for first 2 parts(35%); 
## Jialong for part 3 to part 5(35%)
## Heyu for last part and debugging and overall comments(30%)

find_one <- function(card, n, k, strategy){
  # This function is a function created to simplify the following Pone and Pall 
  # functions, which can reduce the repeated code in the latter two functions. And 
  # this function is used to calculate whether a single prisoner can successfully 
  # find their own number under each strategy.  
  
  # We estimate that when the number of picking up boxes is less than n and 
  # the prisoner succeeds in finding his number, then success is 1, or it's 0.
  
  success <- 0  
  # Strategy 1: The prisoners start by opening the box with their number on it, 
  # check the card number say k, if k is not their prisoner number they go and 
  # open box k. Repeat the process until they have found the card with their 
  # number on it.
  if(strategy==1){ 
    box <- k 
    j <- 1 
    while (j <= n) { 
      if(card[box]==k){ 
        success <- 1
        break
      }
      else{
        box <- card[box] 
        j = j+1 
      }  
    }
  }
  # Strategy 2: same As strategy 1, but starting from a randomly selected box.
  else if(strategy == 2){ 
    box <- sample(1:(2*n), 1) 
    j <- 1
    while (j <= n) {
      if(card[box] == k){
        success <- 1
        break
      }
      else{
        box <- card[box]
        j = j+1
      }  
    }
  }
  # Strategy 3: prisoners open n boxes at random, checking each card for their number.
  else{ 
    box <- sample(1:(2*n), n)
    for(i in 1:n){
      one_box <- box[i] 
      if(card[one_box] == k){
        success <- 1
        break
      }
    }
  }
  return(success)
}

Pone <- function(n, k, strategy, nreps){
  # This function tests the probability that ONE prisoner with number k will 
  # successfully find his or her own number. We find the number of successes
  # using find_one and divide it by the number of simulations(nreps=10000).
  success <- 0 
  card <- 1:(2*n) 
  for(i in 1:nreps){
    ncard <- sample(card,length(card))
    success = success + find_one(ncard, n, k, strategy)
  }
  return(success/nreps)
}
Pone(5, 1, 1, 10000);Pone(50, 1, 1, 10000)
Pone(5, 1, 2, 10000);Pone(50, 1, 2, 10000)
Pone(5, 1, 3, 10000);Pone(50, 1, 3, 10000)

Pall <- function(n, strategy, nreps){ 
  # This function tests the probability that all 2n prisoners succeed in finding 
  # their number. We still find the number of successes with the help of the 
  # function find_one and divide it by nreps.
  success <- 0
  card <- 1:(2*n)
  for(i in 1:nreps){
    ncard <- sample(card,length(card)) 
    for(j in 1:(2*n)){
      if(find_one(ncard, n, j, strategy)!=1){
        break
      }else if( j == (2*n) && find_one(ncard, n, j, strategy) == 1){
        success= 1 + success
      }
    }
  }
  return(success/nreps)
}
Pall(5, 1, 10000);Pall(50, 1, 10000)
Pall(5, 2, 10000);Pall(50, 2, 10000)
Pall(5, 3, 10000);Pall(50, 3, 10000)



