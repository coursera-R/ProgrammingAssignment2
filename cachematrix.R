## Put comments here that give an overall description of what your
## functions do
# Write a short comment describing this function

## makeCacheMatrix function takes in a matrix as an argument. Let's say x 
## makeCacheMatrix has 3 functions defined in it.
## 1. get function will return the contents/value of x;
## 2. setInverse will set the value of the inverse of x (inverse is calculated in cacheSolve);
## 3. getInverse will return the inverse of x (the value set by setInverse function)

## 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

## cacheSolve returns the inverse of a input matrix.
## It calls function getInverse to get the inverse of the matrix.
## If cacheSolve is called on the same input matrix again, it prints the message
## "getting cached data" and returns cached inverse
## If the argument is new, it gets the matrix by calling get(), the matrix is stored in a variable 'data' 
## it then uses solve to calculate the inverse of 'data' and
## caches/sets the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return (m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
