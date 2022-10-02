#' knapsack_dynamic
#'
#' `knapsack_dynamic()`solve the knapsack problem by dynamic programming
#'
#' @param x a dataframe
#' @param W the weight the knapsack can take,a interger
#' @returns a list contain the best value and specific plan(how to choose the items)
#' @seealso https://en.wikipedia.org/wiki/Knapsack_problem#0.2F1_knapsack_problem
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
#' knapsack_dynamic(x = knapsack_objects[1:500,], W = 3500)
#'
#' @export
#'
knapsack_dynamic <-
function(x,W){
  stopifnot(ncol(x)==2)
  stopifnot(colnames(x)==c("w","v"))
  stopifnot(all(x>0))

  lens=nrow(x)

  #state/initialization
  #create 2d_array
  dp_array<-matrix(0,nrow=lens+1,ncol=W+1)
  value=x$v
  weight=x$w

  #state transition function
  #Fill in row by row, not column by column
  for(i in 2:lens+1){
    for(j in 2:W+1){
      if(j>=weight[i-1]){
        dp_array[i,j]=max(dp_array[i-1,j],dp_array[i-1,j-weight[i-1]]+value[i-1])
      }else{
        dp_array[i,j]=dp_array[i-1,j]
      }
    }
  }

  valueresult=dp_array[lens+1,W+1]

  #find the path

  path=c()
  i=lens+1
  j=W+1
  while(dp_array[i,j] != 0){
    if(dp_array[i,j]>dp_array[i-1,j]){
      path=append(path,i-1)
      j=j-weight[i-1]
      i=i-1
    }else{
      i=i-1
    }
  }

  return(list("value"=valueresult,"elements"=path))
}
