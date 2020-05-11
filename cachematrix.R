## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# generate a list of functions which will:
# 1. set the matrix
# 2. get the matrix
# 3. set the inverse
# 4. get the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function

# Return the inverse of a matrix:
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        # check if the matrix has been already used. 
        # if it is then we will use the cache data
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        # if the matrix has not be used before,
        # we calculate the inverse here:
        data <- x$get()
        inv <- solve(data, ...)
        # and we set the calculated inverse value in the 
        # global environnment. 
        x$setinv(inv)
        inv
}
