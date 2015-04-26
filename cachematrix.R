## The first function, makeCacheMatrix, creates a special "matrix" object that 
## can cache its inverse.
## The second function, cacheSolve, computes the inverse of the special "matrix" 
## returned by "makeCacheMatrix above. If the inverse has already been 
## calculated, the cacheSolve should retrieve the inverse from the cache.


## The following function is to cache a matrix and its inverse (no calculations!)

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    
    setmatrix <- function(y) {          # function to replace input matrix
        x <<- y
        inv <<- NULL                    # for the new input matrix we need calculate another inverse, therefore reset the one which is stored.
    }
    getmatrix <- function() {           # function to return input matrix
        x
    }
    setinverse <- function(inverse) {   # function to store the inverse matrix
        inv <<- inverse
    }    
    getinverse <- function() {          # function to return the inverse matrix
        inv
    }
    
    list(setmatrix = setmatrix,         # combine all functions in a list
         getmatrix = getmatrix,
         setinverse = setinverse,
         getinverse = getinverse)

}



## The following function checks whether the inverse of a matrix has already been calculated.
## If so, it get's the inverse from the cache. Otherwise, it calculates the 
## inverse and sets this value in the cache via the setinverse function.

cacheSolve <- function(x, ...) {        # input of this function is the function above (fun1)
    
    inv <- x$getinverse()               # get the inverse matrix from fun1 and check if it is not empty
    
    if(!is.null(inv)) {                 # check that this inverse matrix is not empty, otherwise calculate as in 2nd part.
        message("getting cached data")
        return(inv)
    }
    
    input <- x$getmatrix()              # store input matrix in fun1
    inv <- solve(input, ...)            # calculate the inverse of the input matrix
    x$setinverse(inv)                   # store inverse in fun1
    inv                                 # return inverse
    
}



# To test, run for example: 
#m <- matrix(rnorm(9), 3, 3)
#m
#a <- makeCacheMatrix(m)
#cacheSolve(a)
#cacheSolve(a)


