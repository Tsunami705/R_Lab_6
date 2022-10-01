###Raw Data

RNGversion(min(as.character(getRversion()),"3.5.3"))
##old sampler used for backward compatibility
## suppressWarnings() can be used so that the above warning is not displayed
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )


###Brute force search
brute_force_knapsack<-function(x,W,parallel=FALSE){
  stopifnot(ncol(x)==2)
  stopifnot(colnames(x)==c("w","v"))
  stopifnot(all(x>0))

  #Binary
  #cost of time
  timestart<-Sys.time()

  i=0
  max_value=0
  n=nrow(x)
  while(i<=(2**n-1)){
    i=i+1
    binary_num=as.numeric(intToBits(i))
    the_weight=sum(binary_num*x$w)
    if(the_weight<=W){
      the_value=sum(binary_num*x$v)
      if(the_value>max_value){
        max_value=the_value
        elements=which(binary_num==1)
      }
    }
  }

  #running time
  timeend<-Sys.time()
  runningtime<-timeend-timestart
  print(runningtime)
  return(list("value"=max_value,"elements"=elements))
}

brute_force_knapsack(x = knapsack_objects[1:16,], W = 10000)

#Question:How much time does it takes to run the algorithm for n = 16 objects?
#Time difference of 0.146131 secs


###Dynamic programming
knapsack_dynamic<-function(x,W){
  stopifnot(ncol(x)==2)
  stopifnot(colnames(x)==c("w","v"))
  stopifnot(all(x>0))

  #cost of time
  timestart<-Sys.time()

  lens=nrow(x)

  #state/initialization
  #create 2d_array
  dp_array<-matrix(0,nrow=lens,ncol=W)

  #state transition function
  #Fill in row by row, not column by column
  for(i in 2:lens){
    for(j in 2:W){
      if(j>=x$w[i-1]){
        dp_array[i,j]=max(dp_array[i-1,j],dp_array[i-1,j-x$w[i-1]]+x$v[i-1])
      }else if(j<x$w[i-1]){
        dp_array[i,j]=dp_array[i-1,j]
      }
    }
  }

  #find the path

  path=c()
  i=lens
  j=W
  while(dp_array[i,j] != 0){
    if(dp_array[i,j]>dp_array[i-1,j]){
      path=c(path,i)
      j=j-x$w[i-1]
      i=i-1
    }else{
      i=i-1
    }
  }



  #running time
  timeend<-Sys.time()
  runningtime<-timeend-timestart
  print(runningtime)

  return(list("value"=dp_array[lens,W],"elements"=path))
}

knapsack_dynamic(x = knapsack_objects[1:500,], W = 3500)

#Question How much time does it takes to run the algorithm for n = 500 objects?
#Time difference of 3.077327 secs


###Greedy heuristic
greedy_knapsack<-function(x,W){
  stopifnot(ncol(x)==2)
  stopifnot(colnames(x)==c("w","v"))
  stopifnot(all(x>0))

  #cost of time
  timestart<-Sys.time()

  #choose items in terms of value to weight ratio
  x$r=x$v/x$w

  corder=order(x$r,decreasing = TRUE)
  curr_value=0
  curr_weight=0
  i=1

  while(curr_weight<=W){
    curr_weight=curr_weight+x$w[corder[i]]
    curr_value=curr_value+x$v[corder[i]]
    i=i+1
  }

  itemlist=corder[1:(i-2)]
  curr_weight=curr_weight-x$w[corder[i-1]]
  curr_value=curr_value-x$v[corder[i-1]]

  #running time
  timeend<-Sys.time()
  runningtime<-timeend-timestart
  print(runningtime)

  return(list("value"=curr_value,"elements"=itemlist))
}

test_df=rbind(knapsack_objects,knapsack_objects,
              knapsack_objects,knapsack_objects,
              knapsack_objects)
greedy_knapsack(x = test_df, W = 10000)

#Question How much time does it takes to run the algorithm for n = 1000000 objects?
#Time difference of 0.0007300377 secs
