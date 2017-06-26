## Week 3 Programming Assignment, R-Programming on Coursera

makeCacheMatrix <- function(x = matrix()) {
        
        # Initialise two objects called x and m within the makeCacheMatrix() environment
        m <- NULL
        
        # Create a function to assign input argument into x and to clear any cashe of m
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        # Create a function to retrieve matrix from the makeCacheMatrix() environment
        get <- function() x
        
        # Create a function to set the inverse of the matrix into the cache
        setinverse <- function(solve) m <<- solve
        
        # Create a function to retrieve the inverse of the matrix from the cache
        getinverse <- function() m
        
        # Create and return a list of the funtions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        
        # Assign the inverse the inverse matrix from the object in makeCacheMatrix()
        m <- x$getinverse()
        
        # To chack if the value in the cache is not NULL. 
        # If m is not NULL, return to the cache with the inverse matrix 
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        # To assign the matrix into an object called "data"
        data <- x$get()
        
        # To compute the inverse of the matrix and assign the result into an object called "m"
        m <- solve(data, ...)
        
        # To set the inverse of the matrix in "m" into the cache
        x$setinverse(m)
        
        # Print the inverse of the matrix
        m
}

## Test
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
mx1 <- makeCacheMatrix(m1)
cacheSolve(mx1)