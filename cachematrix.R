## With makeCacheMatrix an object(matrix) is created where the state of the (latest)
## matrix 'lives' outside the cacheSolve function thanks to the use of lexical scoping.
## That way the last results are preserved, because the original object environment is
## not cleaned up after calling the function. 
## The matrix and it's inverse can be accessed/manipulated  through the 4 subfunctions 
## which will be inherited onto the matrix that is created with this function.

## When the cacheSolve function is called, it checks whether a previous result is stored
## in the makeCacheMatrix object (if not, or a new matrix has been created with the
## function, the inv variable is NULL. If there is a cached answer, it will return this
## else it will calculate the inverse matrix using the solve function and the result is
## stored in makeCacheMatrix using the setInv function


## When the makeCacheMatrix function is called to create a matrix, it calls the 
## set function automatically and put value 'NULL' in the inv object to indicate 
## that no caching is possible. Because of the list statement at the end, the 4 
## methods/subfunctions can be called directly using the $-reference
## Notice that in the set and setinv functions the <<- method is used to make the
## object referenceable outside of the function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(m_inverse) inv <<- m_inverse
    getinv <- function() inv
    list(set = set, get = get, 
         setinv = setinv, getinv = getinv)
}



## For cacheSolveirst the inv value is sked from the subfunction of makeCacheMatrix
## If there is a value stored yet (value not 'NULL') then is give the stored
## value of inv back.
## Otherwise it gets the matric data through the get subfunction, calculates the
## inverse with the solve function and stores it in the cache using the setinv
## subfunction before returning the inverse matrix as a result of the function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}

