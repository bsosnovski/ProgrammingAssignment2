## The cachematrix.R file creates a special "matrix" object that 
## can cache its inverse.


## The makeCacheMatrix function creates a special "matrix", 
## which is really a list containing a function to
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the inverse
## 4) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        Inv <- NULL
        set <- function(y) {
                x <<- y
                Inv <<- NULL
        }
        get <- function() x
        setInv <- function(solve) Inv <<- solve
        getInv <- function() Inv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## Function 'cacheSolve' calculates the inverse of the special "matrix" 
## created with the function makeCacheMatrix. However, it first checks 
## to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value 
## of the inverse in the cache via the setInv function.

cacheSolve <- function(x, ...) {
        Inv <- x$getInv()
        if(!is.null(Inv)) {
                message("getting cached data")
                return(Inv)
        }
        data <- x$get()
        Inv <- solve(data, ...)
        x$setInv(Inv)
        Inv
}
