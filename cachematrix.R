## R PROGRAMMING ASSIGNMENT 2

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        ## declare the inverse matrix variable 
        inv <- NULL
        
        ##set the value of the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ##get the value of the matrix
        get <- function(){
                x
        }
        
        ##set the value of inverse of the matrix
        setinverse <- function(inverse){ 
                inv <<- inverse
        }
        
        ##get the value of inverse of the matrix
        getinverse <- function(){ 
                inv
        }
        
        ## Return the list of functions with object 
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)   
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse
## from the cache

cacheSolve <- function(x, ...) {
        
        ## Get the Inverse Veriable Value
        inv <- x$getinverse()
        
        ## If it is not Null, i.e it is already Inversed before then, 
        ## just return the inversed matrix from the cache
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        
        ## OR Take the new Matrix 
        data <- x$get()
        
        ## Find inverse of a square matrix done with the solve function
        inv <- solve(data)
        
        ## Set the inverse to x for future use
        x$setinverse(inv)
        
        ## return the newly created Inverse
        inv
}


## ---- END -----------