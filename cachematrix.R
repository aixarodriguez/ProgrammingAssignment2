## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix - Works like a class that define the usign of solve function to obtains the cache result 
## of inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  s<-NULL
  set<-function(y){
    x<<-y
    s<<-NULL
  }
  get<-function() x
  setSolve<-function(solve) s<<-solve
  getSolve<-function() s
  list(set=set,get=get,
      setSolve=setSolve,getSolve=getSolve)
}


## This function obteins the cache value of a matrix inverse previously calculated

cacheSolve <- function(x,...) {
        ## Return a matrix that is the inverse of 'x'
  s<-x$getSolve()
  if(!is.null(s)){
    message("Getting Cached Data")
    return(s)
  }
  data<-x$get()
  s<-solve(data,...)
  x$setSolve(s)
  s
}

#Using Example
#> my_mat<-matrix(rnorm(1:16),nrow=4,ncol=4)
#> mmat<-makeCacheMatrix()
#> mmat$set(my_mat)
#> mmat$get()
#           [,1]       [,2]       [,3]       [,4]
#[1,] -1.2294798 -0.2102174  0.4401325  0.4806962
#[2,] -0.9725740 -0.3798900 -0.5601481  0.2658546
#[3,]  0.2315410 -0.5423295 -0.2496990 -1.0628247
#[4,] -0.2414938 -1.1294815 -2.1869005 -0.1940190
#> cacheSolve(mmat)
#          [,1]      [,2]         [,3]       [,4]
#[1,]  0.837416 -2.377216 -0.365425093  0.8191562
#[2,] -3.227276  4.530238  0.004032427 -1.8103431
#[3,]  1.442109 -1.865091  0.131736008  0.2956476
#[4,]  1.490415 -2.391357 -1.053505993  1.0327648
#> cacheSolve(mmat)
#Getting Cached Data
#          [,1]      [,2]         [,3]       [,4]
#[1,]  0.837416 -2.377216 -0.365425093  0.8191562
#[2,] -3.227276  4.530238  0.004032427 -1.8103431
#[3,]  1.442109 -1.865091  0.131736008  0.2956476
#[4,]  1.490415 -2.391357 -1.053505993  1.0327648
