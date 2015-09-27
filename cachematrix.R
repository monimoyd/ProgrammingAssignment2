## makeCacheMatrix creates a special matrix which itself and its inverse can be cached
## cacheSolve checks if the inverse of matrix is already cached, then it returns
## cached inverse of matrix ,else it calculates inverse suing solve function and caches it
##
## Uasge and Example:
## input a square invertible matrix, X
##  X <- matrix(1:4, 2, 2)
##  cacheX <- makeCacheMatrix(X)
##
## First time calling cacheSolve will calculate inverse and cacches it
##
## inverseX <- cacheSolve(cacheX)
##
## Second time calling cacheSolve will return the cached inverse and you will see a
## message in console "getting cached data"
##
## inverseX1 <- cacheSolve(cacheX)



##Function  makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of matrix
## get the value of the inverse of matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    
    setInverse <- function(invValue) inv <<- invValue
    
    getInverse <- function() inv
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}



## cacheSolve returns a matrix that is the inverse of special matrix 'x'
##The function first checks to see if the invese has already been calculated. If so,
## it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the
## inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
  
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
    inv
        
}
