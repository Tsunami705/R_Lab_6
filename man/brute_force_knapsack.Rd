% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/brute_force_knapsack.R
\name{brute_force_knapsack}
\alias{brute_force_knapsack}
\title{brute_force_knapsack}
\usage{
brute_force_knapsack(x, W, parallel = FALSE)
}
\arguments{
\item{x}{a dataframe}

\item{W}{the weight the knapsack can take,a interger}

\item{parallel}{choose if you want to use parallel computing}
}
\value{
a list contain the best value and specific plan(how to choose the items)
}
\description{
`brute_force_knapsack()`solve the knapsack problem by brute force
}
\examples{
RNGversion(min(as.character(getRversion()),"3.5.3"))
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )

brute_force_knapsack(x = knapsack_objects[1:16,], W = 10000)

}
\seealso{
https://en.wikipedia.org/wiki/Knapsack_problem
}
