## Matrix inversion is usually a costly computation and their may be some benefit to 
##caching the inverse of a matrix rather than compute it repeatedly.  

##There are 2 functions::
## 1.makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## 2.cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##   If the inverse has already been calculated (and the matrix has not changed), 
##   then the cachesolve should retrieve the inverse from the cache.

## Write a short comment describing this function
## makeCacheMatrix function creates/stores a matrix to be inversed, returns the matrix, 
##stores the inverse and also returns the inverse when requested  

makeCacheMatrix <- function(x = matrix()) {  
  m <- NULL                   #initializing/resetting the matrix in question in the current environment
  
  
  set <- function(y) {        #function to take the input matrix and store the matrix in a different environment     
    x <<- y                   #caches the given input matrix
    m <<- NULL                #initializing/resetting the matrix in question  in a different environment
  }
  
  get <- function() x         #function to return the input matrix from cache
  
  setinverse <- function(inverse) m <<- inverse # function to cache the inverted matrix 
  
  getinverse <- function() m                    #function to return the inverted matrix from cache
  
  list(set = set, get = get,      #returns all the 4 functions created in this function 
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## returns an inverted matrix from cache if exists and computes, stores in cache and then returns otherwise
cacheSolve <- function(x, ...) {
  m <- x$getinverse()           ##Looks for the inverted matrix in cache
  if(!is.null(m)) {
    message("getting cached data")
    return(m)                   ##returnsinverted matrix from cache
  }
  
  m <- solve(x$get(), ...)      ##creates an inverted matrix for the given input matrix 
  x$setinverse(m)               ## calling a function to cache the nverted matrix 
  m
}