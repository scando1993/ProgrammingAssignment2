## Here my functions create a cachematrix and then you can compute the inverse
## of the cachematrix if it is square

## This function creates a cachematrix from a regular matrix 

makeCacheMatrix <- function(x = matrix()) {
    ##Set the inverse to null
    inv <- NULL
    ##Make the set method
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    #Make the get method
    get <- function() x
    #Make the methods for setting and getting the inverse
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inverse
    #Make a list to access the attributes
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This Function takes a cacheMatrix and returns it's inverse if it's a square matrix if not returns null

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    ##Getting data from the matrix
    inv <- x$getinverse()
    ##Verifing if is not null
    if(!is.null(inv)) {
      ##if it's not it means that the inverse has been already computed and it has to be return
      message("getting cached data")
      return(inv)
    }
    ##no prev inverse found so we must find it
    ##Get the original matrix
    data <- x$get()
    ##Check if the dimmensions are not the same if it's true finish and return null if not continue
    if(dim(data)[1] != dim(data)[2]){
      message("Dimensions not equal, Matrix is not square")
      return(NULL)
    }
    ##Calculate inverse
    inverse <- solve(data, ...)
    ##Set the inverse to the original vector
    x$setinverse(inverse)
    ##Show the inverse
    inv
}
