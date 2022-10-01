#' greedy_knapsack
#'
#' `greedy_knapsack()`solve the knapsack problem by greedy heuristic
#'
#' @param x a dataframe
#' @param W the weight the knapsack can take,a interger
#' @returns a list contain the best value and specific plan(how to choose the items)
#' @seealso https://en.wikipedia.org/wiki/Knapsack_problem#Greedy_approximation_algorithm.
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
#' greedy_knapsack(x = knapsack_objects, W = 10000)
#'
#' @export
#'
greedy_knapsack <-
function(x,W){
  stopifnot(ncol(x)==2)
  stopifnot(colnames(x)==c("w","v"))
  stopifnot(all(x>0))

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

  return(list("value"=curr_value,"elements"=itemlist))
}
