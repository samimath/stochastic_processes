library(ggplot2)
lambda <-3
t <- 4
vars <-replicate(1000,rpois(1,lambda*t))
hist(vars,xlim=rev(c(min(vars)-1, max(vars))),xlab = 'N(t)',ylab = 'Frequency',col = 'orange')

## Poisson process simulation 

poi.process <- function(lambda,n){
  
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

# define parameters for simulation:
n<-20

lambda <-3



spatial_pois <-function(trials, t, lambda,a_x, a_y){
  # trials : number of simulations
  # x: fixed point in the space 
  # t: radius centered around x 
  # lambda : intensity of arrival
  # a_x = width of the rectangle (x-dim)
  # a_y = length of the rectange (y-dim)
  
  A <- a_x*a_y
  x0 <-runif(1,min = 0,max = a_x)
  y0 <-runif(1,min = 0,max = a_y)
  simlist <-numeric(trials)
  process <-list()
  
  for (i in 1:trials){
    # simulate number of events with rate lambda*area
    N <-rpois(1,lambda*A)
    print(N)
    # simulate uniformly distributed points (x-dim)
    x <-runif(N,min = 0,max = a_x)
    # simulate uniformly distributed points (y-dim)
    y <-runif(N,min = 0,max = a_y)
    # simulate number of points in a circle centered at (x0,y0) with radius t :
    D <- sum(((x-x0)^2 + (y-y0)^2)<= t^2)
    simlist[i] <- D
    process$x <-x
    process$y <-y
    proceess$simliist <-simlist
    
  }
  return(process)
}
lambda =0.5
n=20
# simulate list of wait time:
s_list <-poi.process(lambda,n)

# plot function:
plot(stepfun(s_list,0:(n+1)), 
     do.points = TRUE,
     pch = 16,
     col.points = "red",
     verticals = FALSE,
     main = 'Realization of a Poisson process with lambda = 3',
     xlab = 'Time of arrival',
     ylab = 'Number of arrivals')

#assign inter-arrival time
t_list<-c(2,1,0.5,3,4)

# set number of arrivals
n<-length(t_list)

# initiate total wait time 
s_list <-numeric(n+1)
s_list[1]<-0

# assign cumulative wait time for each event:
for (i in 1:n){
  
  s_list[i+1]<- sum(t_list[1:i])
  print(s_list)
}

# plot function:
plot(stepfun(s_list,0:(n+1)), 
     do.points = TRUE,
     pch = 16,
     col.points = "red",
     verticals = FALSE,
     main = 'Realization of a Poisson process',
     xlab = 'Time of arrival',
     ylab = 'Number of arrivals')



### Service station example
P_tilde <- rbind(c(0,1,0,0,0),
                 c(4/9,0,5/9,0,0),
                 c(0,4/9,0,5/9,0),
                 c(0,0,4/9,0,5/9),
                 c(0,0,0,1,0))

Q<- rbind(c(-1/4,1/4,0,0,0),
          c(1/5,-9/20,1/4,0,0),
          c(0,1/5,-9/20,1/4,0),
          c(0,0,1/5,-9/20,1/4),
          c(0,0,0,1/5,-1/5))

q_vec <- c(1/4,9/20,9/20,9/20,1/5)



Q <-matrix(Q,nrow = 5,ncol = 5,byrow = FALSE)

Pt<-function(Q,t){
  output <- expm::expm(Q*t)
  return(output)
}


## Compare matrix exponential vs its approximation


matrix_approx<- function(Q,N){
  
  I <- diag(nrow(Q))
  P<- list()
  PM <-I
  
  for(i in 1:N){
    PM <-PM+(Q%^%i)/factorial(i)
  }
  
  return(PM)
  
}

Pt.approx <- matrix_approx(Q,30)

Pt.exact <- expm::expm(Q)

round(Pt.approx-Pt.exact,2)


### simulate a CTMC:

ctmc<- function(init,P,n,q,labels) { 
  output<-list()
  if (missing(labels)) {
    labels <- 1:length(init)
  }
  
  # generate an empty vector for the simulation result, here there are n+1 because we are including the initial state
  simlist <- numeric(n+1)
  holding_time<-numeric(n+1)
  time <-numeric(n+1)
  # here assuming the initial state is a 1 by S vector, where S = # of states 
  states <- 1:length(init)
  # simulate the first step of the chain using the initial distribution
  simlist[1] <- sample(states,1,prob=init)
  time[1]<-0#rexp(n=1,rate=q[simlist[1]])
  print(simlist[1])
  # for the rest of the chain, use the recursive logic where the ith value of the chain is generated with a transition probability 
  
  for (i in 2:(n+1)){ 
    
    simlist[i] <- sample(states,1,prob=P[simlist[i-1],])
    
    print(simlist[i])
    holding_time[i]<-rexp(n=1,rate=q[simlist[i]])
    time[i]<-time[i-1]+holding_time[i-1]
    print(i)
    print('holding time')
    print(holding_time[i])
    
  }
  output$states <- simlist
  output$time <-time#cumsum(holding_time)
  return(output)
}



ctmc.sim<- function(state0,P,q, Tend, plotflag = FALSE){
  # Modified from https://rdrr.io/cran/spuRs/src/R/CMCSimulation.R
  # initialize output
  output <-list()
  # set number of states (0,1,2,...n)
  n <- nrow(Q) - 1
  # initialize list of states with input state0
  statehist <- c(state0)
  # initialize list of time stamps with 0
  timehist <- c(0)
  # initialize time step
  time <- 0
  # initialize current state with input state0
  currentstate <- state0
  # counter
  jump <- 2
  
  while(time < Tend){
    # create accumulated count of holding time
    time <- time + rexp(1, q[currentstate+1])
    # keep track of number of times a state changes
    timehist[jump] <- time
    # sample current state from embedded DTMC matrix
    currentstate <- sample(0:n, 1, prob=P[currentstate+1,])
    
    statehist[jump] <- currentstate
    
    jump <- jump + 1
  }
  
  if (plotflag){
    plot(timehist, statehist, type="s", xlab="Time", ylab="State", 
         ylim=c(0,n), xlim=c(0,Tend), yaxt="n")
    axis(2, at=0:n)
  }
    output$statehist <-statehist
    output$timehist <-timehist
    return(output) 
}

set.seed(201)

ctmc_output2<-ctmc.sim(state0 = 1,P = P_tilde,q = q_vec,Tend = 60,plotflag = TRUE)






set.seed(20990)
s<-c(0)
for (i in 1:50){
  x<-rexp(1,0.5)
  s<-append(s,s[i]+x)
}

N<-c(0)
t<-0

while(t<s[51]){
  t<-t+1
  for(k in 0:50){
    if(t<s[k+1]){
      N<-append(N,k)
      break
    }
  }
}

plot(stepfun(N,0:length(N)), pch=16, verticals = FALSE, xlim = c(0,50), xlab = "t", ylab = "N", main = "Poisson Process Simulation")


#P(N_50 = 100) = 
1 - ppois(50,25)