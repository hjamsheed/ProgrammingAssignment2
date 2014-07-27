## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix creates matrix within a list with the following attributes
## set - stores the value of the matrix in the list
## get - retrieves the matrixes stored in the list
## setinvervse - stores the inverse of the matrix in the list
## getinvervse - retrieves the inverse of the matrix in the list

makeCacheMatrix <- function(x = matrix()) {
    inv <-NULL
    set <- function(y) {
      x<<-y
      inv<<-NULL
    }
    get <-function() x
    setinverse <-  function(inverse) inv <<-inverse
    getinverse <- function() inv
    list(set=set,
         get=get,
         setinverse = setinverse,
         getinverse=getinverse)
}


## Write a short comment describing this function
## cacheSolve first checks to see of a CacheMatrix list contains a inverse stored, if it is it will just retrieve the values
## otherwise it will calculate the inverse and store it within the CacheMatrix list

calculates the inverse of  within a list with the following attributes

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<- x$getinverse()
        if(!is.null(inv)) {
            message("getting cached data") 
            return(inv)
        }
        data <-x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
