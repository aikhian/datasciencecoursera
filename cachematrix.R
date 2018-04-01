## Put comments here that give an overall description of what your
## functions do

# The R functions serve to avoid performing repeated recomputations,
# especially if the computation involves a very big vector, by extracting 
# the cached data, to save on computer resources and time.

## Write a short comment describing this function
# Similarly, the function:
# 1. Sets the value of the matrix
# 2. Gets the value of the matrix
# 3. Sets the value of the inverse of the matrix and
# 4. Gets the value of the inverse of the matrix

#=====================
# The following was copied over for reference only.

# makeVector <- function(x = numeric()) {
#         m <- NULL
#         set <- function(y) {
#                 x <<- y
#                 m <<- NULL
#         }
#         get <- function() x
#         setmean <- function(mean) m <<- mean
#         getmean <- function() m
#         list(set = set, get = get,
#              setmean = setmean,
#              getmean = getmean)
# }
#=====================

makeCacheMatrix <- function(x = matrix()) {
        my_inv <- NULL
        set <- function(y) {
                x <<- y
                my_inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) my_inv <<- inverse
        getInverse <- function() my_inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

#=====================
## Write a short comment describing this function

# THe function performs the inverse of the matrix and checks
# if the inverse has already been computed. If it was already done,
# it gets the inverse matrix from the cache directly without 
# any computation. Otherwise, it does the invserse and sets the value 
# in the cache via the setInverse function.


#=====================
# The following was copied over for reference only.

# cachemean <- function(x, ...) {
#         m <- x$getmean()
#         if(!is.null(m)) {
#                 message("getting cached data")
#                 return(m)
#         }
#         data <- x$get()
#         m <- mean(data, ...)
#         x$setmean(m)
#         m
# }

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        my_inv <- x$getInverse()
        if (!is.null(my_inv)) {
                message("getting cached data")
                return(my_inv)
        }
        mat <- x$get()
        my_inv <- solve(mat, ...)
        x$setInverse(my_inv)
        my_inv
}

