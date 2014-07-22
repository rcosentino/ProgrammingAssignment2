# This two functions create a special object that stores a matrix and cache's 
# its inverse.



# The MakeCacheMatrix function creates a list containing functions to: i) set 
# the value of the matrix ii) get the value of the matrix iii) set the value of
# the inverse matrix and iv) get the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        i <-NULL
        set <- function(y){
            x <<- y
            i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}



# The cacheSolve function calculates the inverse of the matrix given in the
# above function, but first it checks if it has already been calculated. If so,
# it gets the inverse matrix from the cache and skips the computation.
# Otherwise, it calculates the inverse matrix of the data and sets the value of
# inverse matrix in the cache via the setinverse function

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i

}
