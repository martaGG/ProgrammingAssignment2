## makeCacheMatrix creates a "matrix" object whose inverse can be stored in cache
## cacheSolve calculates the inverse of a makeCacheMatrix object if not stored in cache
## we always assume that the numeric matrices used are invertible

## Creates a new objet composed by a numeric matrix and a NULL cached inverse
## The numerix matrix and its inverse can be recovered ant its cached inverse set

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() {
        x
    }
    setinv <- function(inv){
        inverse <<- inv
    }
    getinv <- function(){
        inverse
    }
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}


## returns the cached inverse of a makeCacheMatrix object
## calculates it if not previously set

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}
