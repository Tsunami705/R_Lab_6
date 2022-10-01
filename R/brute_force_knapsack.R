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
#' @export
#'
brute_force_knapsack <-
function(x,W,parallel=FALSE){
  stopifnot(ncol(x)==2)
  stopifnot(colnames(x)==c("w","v"))
  stopifnot(all(x>0))

  #Binary
  i=0
  max_value=0
  n=nrow(x)
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

  return(list("value"=max_value,"elements"=elements))
}
