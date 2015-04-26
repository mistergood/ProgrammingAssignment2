## The following are functions to store a matrix object as a list
## and create a cache of the inverse


## Function "makeCacheMatrix" creates and returns matrix object as a list
## with functionality to cache the inverse
## "set" sets the value of the matrix and nulls the inverse cache
## "get" retrieves the value of the matrix
## "setInv" sets the inverse property of the matrix object
## "getInv" retrieves the inverse property of the matrix object
## returns : list

makeCacheMatrix <- function(x = matrix()) {
  
    ## Initialize the inverse
    inv <- NULL
  
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x

    setInv <- function(inver) inv <<- inver
    
    getInv <- function() inv
    
    ##  Create list to return with named function references
    list(set = set, 
         get = get,
         setInv = setInv,
         getInv = getInv)
    
}


## Function "cacheSolve" retrieves or creates the cache of the inverse of the matrix list object
## returns : inverse matrix

cacheSolve <- function(x, ...) {

    ## Attempt to retrieve the cached inverse of the matrix
    inv <- x$getInv()
    
    ## If cached inverse retrieved, return it
    if(!is.null(inv)) {
        message("cached inverse found, returning data")
        return(inv)
    }
    
    message("cached inverse not found, creating cache")
    
    ## Retrieve the matrix
    data <- x$get()
    
    ## Create inverse
    inv <- solve(data, ...)

    ## Store cached inverse
    x$setInv(inv)

    ## Return inverse
    inv
    
}
