## This function first calculates the inverse of the function and 
## and cache it and the next function cache solve calculates the inverse of the matrix solved previously 
## if inverse has been already calculated then it will retrive its value

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
          x <<- y
          m <<- NULL
        }
        get <- function() x 
        setinverse <- function(solve) m <<- solve
        getinverse <- function () m 
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}


##This function will calculate the inverse of the matrix and 
## if it is already available then it will just return the value 

cacheSolve <- function(x, ...) {
       m <- x$getinverse()
       if(!is.null(m)) {
         message("getting cached data")
         return(m)
       }
       data <- x$get()
       m <- solve(data, ...)
       x$setinverse(m)
       m 
}
