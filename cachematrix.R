
## Overall description: The functions in this program calculate the inverse of a matrix and stores it in 
## the cache. If the inverse of the matrix has already been calculated then the 
## program uses the value in the cache and does not recalculate the inverse.

## makeCacheMatrix is a combination of functions. If we assign a matrix to this function
## (for example, x<-makeCacheMatrix(m1) ) then this function allows for a number of operations on
## the matrix m1. 

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL       # this assigns a NULL value to the inverse.
  
  set <- function(y) {  # set assigns a matrix  to the function makeCacheMatrix, e.g. x$set(m1).
    x <<- y             # assign the matrix y to x. Note that this is a global assignment.  
    inv <<- NULL        # the value is the inverse is set to NULL globally.
  }
  get <- function() x  # this can be used to check which matrix has been assigned
  
  setinv <- function(solve) inv <<- solve  # this sets the value of the inverse of the matrix
  getinv <- function() inv                 # this can be used to display the inverse
  list(set = set, get = get,               # list the functions in makeCacheMatrix
       setinv = setinv,
       getinv = getinv) 
  

}


## cacheSolve checks whether the inverse of the matrix has already been calculated
## if not then it calculates inverse of the matrix by using solve.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinv()           # get the value of the inverse
  if(!is.null(inv)) {         # check if the inverse has already been calculated.
    message("getting cached data")  # if the inverse has been calculated then print this message
    return(inv)                   # and return the value stored.
  }
  m1 <- x$get()                   # if the inverse has not been calculated then get the matrix from makeCacheMatrix 
  inv <- solve(m1, ...)           # calculate the inverse of the matrix
  x$setinv(inv)                   # assign the value of the inverse to variable inv
  inv                             # print the inverse
  
}
