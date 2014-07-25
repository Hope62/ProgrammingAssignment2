## This assignment asks us to compute the inverse of a matrix using a cache
## for the case where we are opperating on a large matrix in which taking the inverse is costly.

## makeCacheMatrix: Creates an empty matrix and works as a cache for solved inverses.
## Exposes functions via a list to:
## 1) Set the value of the matrix
## 2) Get the value of the matrix
## 3) Set the inverse of the matrix
## 4) Get the value of the inverse of the matrix
## ARGS: none
## RETURN: List of functions defined. 
##        
##
## NOTE: Because the purpose of this assignment is to learn about lexical scope and the <<- operator I 
## will note that the reason for using x <<- y, m <<- NULL and m <<- inverse is because these variables (x and m)
## are defined in the parent function makeCacheMatrix. In order to access them from a child function (a function 
## defined within the function) the <<- operator must be used to search the parent scope for the variable.

makeCacheMatrix <- function(x = matrix()) {
    m = NULL
    set = function(y) {
        x <<- y
        m <<- NULL
    }
    get = function() x
    setinverse = function(inverse) m <<- inverse
    getinverse = function() m
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## cacheSolve: This function will check to see if the inverse we are looking for exists in the cache.
## If it doesn't it will calculate the inverse and store it in the cache.
## ARGS: A square matrix that is not singular (where the determinate != 0). 
## RETURNS: The inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m = x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data = x$get()
    m = solve(data)
    x$setinverse(m)
    m
}

