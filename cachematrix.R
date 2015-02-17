## The functions below allow to cache matrix inversion.
## Generally speaking, it is an attempt to use object-oriented paradigm in R.
##
## Function makeCacheMatrix creates a special object for the given matrix,
## that stores the matrix itself and the inverse matrix, which is recalculated 
## if and only if the matrix is changed. The object returned by the function 
## is a list containing two pairs of setters and getters for the matrix and its 
## inverse. The state of an object is always valid, because using setter for 
## a matrix guarantees recalculation of its inverse.
## 
## Function cacheSolve returns the inverse matrix for the object created
## by makeCacheMatrix. If the matrix did not change since last call, its
## inverse is not computed once more, just the cached value is returned.
##
## Example of usage:
## m_cached <- makeCacheMatrix(matrix(1:4, 2, 2))
## m_inv <- cacheSolve(m_cached)

## Create a matrix with cached inverse from given invertible matrix 'x'
makeCacheMatrix <- function(x = matrix()) 
{
    inv <- NULL
    
    ## the function to set matrix
    set <- function(y) 
    {
        x <<- y
        
        ## if the matrix is set, the inverse will be updated later on demand
        inv <<- NULL
    }
    
    ## the function to get matrix
    get <- function() x
    
    ## the function to set matrix inverse
    setinv <- function(inv) inv <<- inv
    
    ## the function to get matrix inverse
    getinv <- function() inv
    
    ## the result is a list of the 4 functions defined
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## Get inverse matrix for the given object 'x' created by makeCacheMatrix
cacheSolve <- function(x, ...) 
{   
    ## trying to get the cached inverse
    inv <- x$getinv()    
    if(!is.null(inv)) {
        ## the cached inverse value is valid, returning cached data
        return(inv)
    }
    
    ## the cached inverse value is NULL, because the matrix was
    ## just created or updated, calculating the inverse
    matrix <- x$get()
    inv <- solve(matrix)    
    
    ## caching the inverse
    x$setinv(inv)
    
    ## returning the inverse
    inv
}
