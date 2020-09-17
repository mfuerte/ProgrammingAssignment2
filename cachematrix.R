## In lieu of a general description, I've written
## comments beside the code to describe what each
## section is doing. This has helped ensure that
## I not just blindly following the example code.

## makeCacheMatrix creates an R object that stores
## a matrix and its inverse. It must be used with
## cacheSolve for it to work.

makeCacheMatrix <- function(x = matrix()) {             ## We initialize x to an empty matrix and i to a NULL value that will later 
        i <- NULL                                       ## store an inverse.     
        
        set <- function(y) {                            ## We create the "setter". y is assigned to x in the parent environment and i
                x <<- y                                 ## is again assigned to a NULL value. This also allows the function to clear
                i <<- NULL                              ## its cache if we're calculating a new inverse. If it is an old one, set is
        }                                               ## not called and cache is not cleared.
        
        get <- function() x                             ## We create the "getter". x is retrieved from the parent environment. We define
        setinverse <- function(solve) i <<- solve       ## the setter by assigning its input to the value of i from the parent environment.
        getinverse <- function() i                      ## We define the getter by again retrieving i from the parent environment.
        
        list(set = set, get = get,                      ## We create a list to be returned by our function in which each element in the
             setinverse = setinverse,                   ## list is named so that we can later access them by name instead of the extract
             getinverse = getinverse)                   ## operator.
}                                                       

## cacheSolve will take the inputted matrix and its
## cached inverse and will return the cached inverse,
## if it exists, or calculate a new one and make it
## the new cached inverse.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()                             ## Our function retrieves an inverse from the object passed as its input.

        if(!is.null(i)) {                               ## If it retrieves a non null value, this inverse has already been calculated and 
                message("getting cached data")          ## the function will return a message confirming that it is retrieving a cached 
                return(i)                               ## result as well as that actual result. If it retrieves a NULL, the function 
        }                                               ## continues.
        
        data <- x$get()                                 ## With no cached inverse, our function will get the matrix from the input object,
        i <- solve(data, ...)                           ## use solve() to get the inverse, set the inverse in the input object to be cached, 
        x$setinverse(i)                                 ## and then prints it so that it can be passed to the parent environment.
        i
}