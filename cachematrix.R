## The following two functions can be used to compute the inverse
## of a matrix and save to cache if it hasn't been computed. If it has
## the inverse will be taken directly from cache.

## The makeCacheMatrix has four functions.
## 1. set. This function sets x as the input matrix and inv as NULL. 
## 	     Both values are saved in the the cache by using <<- operator.
## 2. get. This function displays the matrix in the cache.
## 3. setinverse. This function sets inv to be the inverse and save the value
##         in the cache.
## 4. getinverse. This function displays the inverse in the cache.

makeCacheMatrix <- function(x = matrix()) {
	  inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## The cacheSolve function will take a list as its input. It will then
## output the inverse of the matrix if it has been computed. If not it
## will compute and output the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	  inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv

}

