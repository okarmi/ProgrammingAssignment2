## makeCacheMatrix and cacheSolve are used to calculate and cache a matrix and it's inverse.
## makeCacheMatrix is used to create and maintain the cacheMatrix object (which is a list with set and get functionality of 
## both the matrix value and the inverse value)
## cacheSolve is used to return the inverse value of a cachematrix object. It calculates the value if it was not yet calculated.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## makeCacheMatrix receives a matrix as an input
## makeCacheMatrix assumes that the matrix supplied is always invertible
## makeCacheMatrix returns a list with the four names (set,get,setinverse,getinverse)
## To get the inverse of a matrix use $getinverse. If no inverse has been calculated yet, a NULL is returned. 

makeCacheMatrix <- function(x = matrix()) {
  
  matrixinverse <- NULL
  set <- function(y) {
    x <<- y
    matrixinverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) matrixinverse <<- inverse
  getinverse <- function() matrixinverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache. If the invese has not been calculated than cacheSolve calculates it and sets it in the cacheMatrix object.
## cacheSolve recieves a cachematrix object.
## cacheSolve returns the inverse of the matrix in the cacheMatrix object and updates that result in the object as well.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  matrixinverse <- x$getinverse()
  if(!is.null(matrixinverse)) {
    message("getting cached data")
    return(matrixinverse)
  }
  data <- x$get()
  matrixinverse <- solve(data, ...)
  x$setinverse(matrixinverse)
  matrixinverse
  
}
