## This functions are used to create a special object thats stores a matrix 
## and cache's its inverse

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(i){
        inverse <<- i   
    } 
    getInverse <- function() inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached inverse matrix")
        return(i)
    }
    data <- x$get()
    i <- ginv(data)
    x$setInverse(i)
    i 
}
