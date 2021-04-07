
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL # clear previously cached inverted matrix
  
  # function to set x object to matrix, and resets s 
  set <- function(matrix) {
    x <<- matrix 
    s <<- NULL
  }
  
  #defines get to return x (the matrix)
  get <- function() x
  
  #sets the inverse s to inverse
  setinverse <- function(inverse) s <<- inverse
  
  #returns the inverse matrix
  getinverse <- function() s
  
  #assigns the each name in the list to the functions just defined (to be able to call the using "$" in cacheSolve)
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'. If the s object is not null, the function returns the inverse of the matrix by placing the result of previously 
  #defined function get() to data, and then applying the solve() function to invert the matrix contained in data. Then setinverse returns the inverted matrix
  s <- x$getinverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinverse(s)
  s
  
}
