poi_process <- function(lambda,n){
  
  # initialize vector of total wait time for the arrival of each event:
  s<-numeric(n+1)
  # set S_0 = 0
  s[1] <-0
  # generate vector of iid Exp random variables:
  x <-replicate(n,rexp(1,lambda))
  # assign wait time to vector s in for loop:
  for (k in 1:n){
    
    s[k+1] <-sum(x[1:k])
    
  }
  # return vector of wait time
  return(s)
  
}

n<-2

lambda <-55

# simulate list of wait time:
s_list <-poi.process(lambda,n)

# plot function:

pois
plot(stepfun(0:(n-1), s_list), 
     do.points = TRUE,
     pch = 16,
     col.points = "red",
     verticals = FALSE,
     main = 'Realization of a Poisson process with lambda = 3',
     xlab = 'Time of arrival',
     ylab = 'Number of arrivals')
