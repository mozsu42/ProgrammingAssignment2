## I created an r script which can be used to 
## cache the inverse of matrices 
## thus you can save computing time
## In the below example, you may see using the R script
##
## > mat<-matrix(1:4,2,2)
## > mat1<-matrix(6:9,2,2)
## > mat2<-matrix(1:9,3,3)
## > mat
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > mat1
##      [,1] [,2]
## [1,]    6    8
## [2,]    7    9
## > mat2
##      [,1] [,2] [,3]
## [1,]    1    4    7
## [2,]    2    5    8
## [3,]    3    6    9
## > t<-makeCacheMatrix(mat)
## > t1<-makeCacheMatrix(mat1)
## > t2<-makeCacheMatrix(mat2)
## > cacheSolve(t)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(t1)
##      [,1] [,2]
## [1,] -4.5    4
## [2,]  3.5   -3
## > cacheSolve(t2)
## matrix has no inverse
## > cacheSolve(t)
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## >

## makeCacheMatrix function calculates a special object, which contains 2D list of matrix elements 
## and a 2D list of inverse matrix elements 
## and function to set and get these matrices
  makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(a_matrix) {
      x <<- a_matrix
      inv <<- NULL
    }
    get <- function() x
    setinverse <- function(a_inv) inv <<- a_inv
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse, 
         getinverse = getinverse)
  }

## the cacheSolve function mputes the inverse
## of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {

  i_matrix<-x$getinverse()
  if(!is.null(i_matrix)) {
    message("getting cached data")
    return(i_matrix)
  }
  data<-x$get()
  if(det(data)!=0){
    i_matrix<-solve(data, ...)
    x$setinverse(i_matrix)
    return(i_matrix)
  }
  message("matrix has no inverse")
}
