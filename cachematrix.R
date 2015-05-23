## ---------------------------------------------------------------- ##
## ----------------------------------------------------------------	##
##             CACHING INVERSE OF MATRIX							##
##																	##
##	This program will first create a matrix object that can cache	##
##	its inverse														##
##	The function will then compute the inverse of the matrix 		##
##	returned, using any values that were already saved instead of	##
##	trying to re-compute those values.								##
##																	##
## Calling:															##
## a <- makeCacheMatrix()											##
## a$set(matrix(1:4,2,2)) ##example only (use any size matrix)		##
##																	##
## cacheSolve(a)													##
## ---------------------------------------------------------------- ##
 
## This function sets the Matrix values, gets the values
## Then sets the value of the inverse and gets the inverse value
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL

		set <- function(y) { ## setting the matrix
                x <<- y
                m <<- NULL
        }

        get <- function() x 
        setinverse <- function(solve) m <<- solve ## solve function returns the inverse of the matrix
        getinverse <- function() m				  ## the inverse of the matrix.

        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
 
## Calculates the inverse of the matrix created in the previous function.
## It will first check if the inverse was cached - if yes then it will get
## the inverse from cache.  If not, then it will calculate with solve()
## and setinverse function.
cacheSolve <- function(x, ...) {
        m <- x$getinverse()  	## pull back the cached value of the inverse

        if(!is.null(m)) {  		## if the inverse was cached, then it displays m
                message("getting cached data")
                return(m)
        }

        data <- x$get()  		## gets x
        m <- solve(data, ...)	## solves x
        x$setinverse(m)			## sets inverse
        m
}
