#' brute_force_knapsack
#'
#' `brute_force_knapsack()`solve the knapsack problem by brute force
#'
#' @param x a dataframe
#' @param W the weight the knapsack can take,a interger
#' @param parallel choose if you want to use parallel computing
#' @returns a list contain the best value and specific plan(how to choose the items)
#' @seealso https://en.wikipedia.org/wiki/Knapsack_problem
#' @examples
#' RNGversion(min(as.character(getRversion()),"3.5.3"))
#' set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
#' n <- 2000
#' knapsack_objects <-
#'   data.frame(
#'     w=sample(1:4000, size = n, replace = TRUE),
#'     v=runif(n = n, 0, 10000)
#'   )
#'
#' brute_force_knapsack(x = knapsack_objects[1:16,], W = 10000)
#'
#' @import parallel
#' @export
#'
brute_force_knapsack <-
function(x,W,parallel=FALSE){
  stopifnot(ncol(x)==2)
  stopifnot(colnames(x)==c("w","v"))
  stopifnot(all(x>0))

  i=0
  max_value=0
  n=nrow(x)
  if(parallel==FALSE){

  #Binary
  while(i<=(2**n-1)){
    i=i+1
    binary_num=as.numeric(intToBits(i))
    binary_num=binary_num[1:n]
    the_weight=sum(binary_num*x$w)
    if(the_weight<=W){
      the_value=sum(binary_num*x$v)
      if(the_value>max_value){
        max_value=the_value
        elements=which(binary_num==1)
      }
    }
  }
  }
  else if(parallel==TRUE){
    j=1:(2**n-1)
    cores <- parallel::detectCores()
    cl <- makeCluster(cores, type = "PSOCK")
    res <- parLapply(cl, j,
                      fun =function(j,x,n,W){
                        the_value=0
                        elements=0
                        binary_num=as.numeric(intToBits(j))
                        binary_num=binary_num[1:n]
                        the_weight=sum(binary_num*x$w)
                        if(the_weight<=W){
                          the_value=sum(binary_num*x$v)
                          elements=which(binary_num==1)
                        }
                        return(the_value)
                      },x,n,W)
    stopCluster(cl)
    res1= as.vector(unlist(res))
    i=which(res1==max(res1),arr.ind=TRUE)
    elements=which(as.numeric(intToBits(i))==1)
    return(list("value"=max(res1),"elements"=elements))
  }else{
    stop("parallel must be TRUE or FALSE.")
  }
  return(list("value"=max_value,"elements"=elements))
}
