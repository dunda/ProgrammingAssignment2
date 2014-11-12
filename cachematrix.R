## Matrix inversion is usually a costly computation and their may 
## be some benefit to caching the inverse of a matrix rather than
## compute it repeatedly. Here are a pair of functions that cache 
## the inverse of a matrix.

## makeCacheMatrix: This function creates a vector containing:
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
                inv <- NULL
                set <- function(y) {
                        x <<- y
                        inv <<- NULL
                }
                get <- function() x
                setinverse <- function(inverse) inv <<- inverse
                getinverse <- function() inv
                list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## The following function calculates the inverse of the special "vector" 
## created with the above function. However, it first checks to see if the
## inverse has already been calculated. If so, it gets the inverse from the cache
## and skips the computation. Otherwise, it calculates the inverse of the data
## and sets the value of the inverse in the cache via the setinverse function.

ccacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
        ## Return a matrix that is the inverse of 'x'
}
