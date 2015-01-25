## Put comments here that give an overall description of what your
## functions do

## Function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) 
{
     inv <- NULL
     set <- function(y)
     {
        x <<- y
        inv <<- NULL
     }
     get <- function() x
     ## Set Inverse to Cache
     setInverse <- function(solve) inv <<- solve
     ## Get Inverse from Cache
     getInverse <- function() inv
     list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then 
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
{
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    ## If cache contains non-null data, then return the data from cache
    if(!is.null(inv)
    {
       message("getting cached data")
       return(nv)
    }
    ## If cache is null, then get & set data to/from cache.
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
