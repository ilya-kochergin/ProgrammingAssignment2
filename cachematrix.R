## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Get a matrix argument
## Returns list object, that stores function's  closures 
## that in turn contain original matrix and result of inversion
## Usage:
## mat<-matrix(c(1,2,-3,0),2,2)
## mat_list <- makeCacheMatrix (mat)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
## Get a list object, that stores function's  closures 
## that in turn contain original matrix and result of inversion
## If matrix inversion already computed return it w/o new call of sovle() function
## Usage: inv_mat<-cacheSolve(mat_list)
##

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv

}
