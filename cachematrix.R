## These functions will calculate the inverse of a matrix and cache the data for 
## easy retreval

## This function will create a list that becomes the Cache, this function must be run
## first on a matrix or the calculation won't know what to do, all this does is build
## a frame to store the cached information in

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL # set the inital inverse to NULL
    set <- function(y) {
        x <<- y # set a value for the matrix
        inv <<- NULL # set the inverse to NULL
    }
    get <- function() x # gets the value of the original matrix
    setinverse <- function(solve) inv <<- solve # sets the value of the inverse
    getinverse <- function() inv # gets the value of the inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse) #returns a list of 4 functions that store the data around the inverse function
}

## This function checks to see if the inverse matrix has already been cached
## and if it hasn't it calculates the inverse and saves the infromation to the cache
## This function will not work if the list from makeCacheMatrix has not been created

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse() 
    if(!is.null(inv)) { #this line checks to see if there is a getinverse value for this matrix, if there is the if function runs
        message("getting cached data") # this line lets you know the data was cached
        return(inv) # returns the inverse matrix
    }
    data <- x$get() # if its not cached it gets the matrix
    inv <- solve(data, ...) # this is where it solves the inverse function
    x$setinverse(inv) # it sets the inverse in the list from makeCacheMatrix here
    inv # prints the inverse function
}