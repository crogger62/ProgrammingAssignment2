##
## Cache the Inverse of a Matrix
##
## rprog-12 - Assignment #2 - Craig Lewis
##
##
#
# makeCacheMatrix - Creates  a special matrix object that can cache its inverse
#

makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL                               ## Inv is the cached inverted matrix
  set <- function (y) {                     ## Set function; cached coies of Matrix and inverse 
    x <<- y                                 ## Set matrix to X
    Inv <<- NULL                            ## Set to cached inverse to null to start
    
  }
  get <- function () x                      ## Return the matrix
  getinverse <- function() Inv              ## Return the cached Inversion matrix
                                           
  setinverse <- function(inverse) Inv <<- inverse    ## Set the cached Inversion matrix
  
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)  ## Setup names of functions
  
}


##
## cacheSolve - Computes the inverse of the special matrix returned by CacheMatrix
##

cacheSolve <- function(x, ...) {
  
   m <- x$getinverse()                    ## Check current value of inverse in m
   if (!is.null(m)){                      ## if m is NOT NULL then we can return the cached instances
     message("getting cached data")
     return(m)
   }

   data <- x$get()                        ## Otherwise get the cached matrix and put it into data
   m <- solve(data,...)                   ## solve for m the inverted matrix using input data
   x$setinverse(m)                        ## set the inverse for later
   m                                      ## return the inverted matrix
}
