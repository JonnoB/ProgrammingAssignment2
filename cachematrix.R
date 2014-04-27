## This function pair work together to reduce the amount of computing 
## time required to repeatedly call a matrix inversing operation when 
## no change has occured to the matrix. The first Function is called when
## the matrix needs to be changed, the second function when the 
## inverse is required

## The makeCacheMatrix stores the inverse and ensures that if the matrix 
## changes then the cache is erased, it also contains the base data 
## required for he second function to operate

## Note some inline comments may go on to the line below

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL         ##If the function has been called again then the 
  ##inverse is no longer valid and needs to be set to NULL
  
  set <- function(y) {
    x <<- y     ## resets the Matrix at the global level 
    inv <<- NULL ## If the function has been reset the inverse is no 
    ## longer valid and needs to be set to NULL
    
  }
  get <- function() x ##Simply prints the matrix
  setinverse <- function(inverse) inv <<- inverse ##caches the inverse of
  ##the matrix outside the function environment, this function is 
  ##called by the "cachesolve" function
  
  getinverse <- function() inv ##prints the cached inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) #prints the list of functions
  
}


## The CacheSolve function checks to see if the is already an inverse 
##matrix stored if there is then this result is printed if not then it 
##will calculate the inverse and print it.

cacheSolve <- function(x, ...) {
  
  inv <- x$getinverse() #checks to see if there is a cached inverse
  if(!is.null(inv)) {
    message("getting cached data") 
    return(inv) #if there is a chached inverse the value is returned
  }
  # if no cached value is present then the get sub function of the 
  ##makeCacheMatrix function is called
  
  data <- x$get() 
  inv <- solve(data,...) ## The matrix is inverted using the solve 
  ##function
  
  x$setinverse(inv) ## The setinverse sub function is called in
  ##makeCacheMatrix caching the the inverse for future use.
  
  inv #prints inverse independent of whether it was cached or not.
  
}
