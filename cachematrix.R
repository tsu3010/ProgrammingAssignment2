##cachematrix has two overall functions namely makeCacheMatrix and cacheSolve

##Description of makeCacheMatrix

## makeCacheMatrix accepts a matrix x as an argument 
## It has 4functions get, set ,getinv and seinv which are created but not run
## when makeCacheMatrix is called. These functions are used by cacheSolve to get 
## matrix x or get inverse m or set inverse m .

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() {x } 
  setinv <- function(inv) { m <<- inv }
  getinv <- function() { m }
  list(set = set, get = get,setinv = setinv,getinv = getinv)
}


## cacheSolve checks if the inverse of the matrix has already been cached .
## If it has been cached it retrieves cached data in the if loop
## If inverse has been cached it calls the functions get() to accept input matrix x .
## It calls the function solve to find the inverse of the matrix and assigns it to m.
## It passes m to setinv() function which uses <<- superassignment operator to set 
## the value of m so that it can be cached the next time we pass the same input .

cacheSolve <- function(x , ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m

  ## Return a matrix  m that is the inverse of 'x'
}
