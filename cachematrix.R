## The functions below cache the inverse of a matrix. 
## If the contents of the matrix do not change, the cached inverse is returned rather than the inverse being recomputed. 
## The functions below take the advantage of the scoping rules of the R language and 
## preserve the state of the inverse matrix inside an R object. 
## The <<- operator is used to assign a value to an object in an environment that is different from the current environment. 
## Furthermore, it is assumed that the matrix supplied is always invertible. 



## The makeCacheMatrix function creates a special matrix object that can cache its inverse. 
## The function does the following:
## 1.	Sets the value of the matrix
## 2.	Gets the value of the matrix
## 3.	Sets the value of the inverse of the matrix
## 4.	Gets the value of the inverse of the matrix
## Note that the special operator (<<-) is used inside the set functions.
makeCacheMatrix <- function(x = matrix()) {
   
    get <- function(){
        x
    }
    
    getinverse <- function(){
        iv
    }
    
    iv <- NULL
    
    set <- function(y) {
        x <<- y # special op
        iv <<- NULL # special op
    }
    
    setinverse <- function(inverse){
        iv <<- inverse #special op
    }
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## The cacheSolve function calculates the inverse of the special matrix created in the above function. 
## If the inverse has already been calculated (and the matrix has not changed), 
## the cacheSolve function retrieves the inverse from the cache. 
## Computing the inverse of a square matrix is done by using the solve function in R. 
cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    
    iv <- x$getinverse()
    
    if(iv != NULL){
        message("getting cached data")
        return(iv)
    }
    
    data <- x$get()
    
    iv <- solve(x, ...)
    
    x$setinverse(iv)
    
    iv
}