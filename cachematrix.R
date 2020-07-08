## These functions create an object that can cache the inverse of a matrix.  
## BTW: Thank you for reviewing my work, I appreciate it :)

## In this function I set the value of the matrix, then "get" the value of the matrix. After, I set the inverse and then "get" the inverse with solve()

makeCacheMatrix <- function(x = matrix()) {
    s <- null
    set <- function(y) {
        x <<- y
        s <<- null
    }
    get <- function()x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get, 
         setsolve = setsolve,
         getsolve = getsolve)
}


## This function computes the inverse of the matrix returned by makeCacheMatrix. If its inverse has been calculated previously, it will recover the inverse from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)){
            message("getting inverse matrix")
            return(s)
        }
        
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
