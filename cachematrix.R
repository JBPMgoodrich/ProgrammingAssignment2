
##
## R function that caches a potentially time-consuming computations
## 
##    Cache the inverse of a matrix
##    Look up value from cache when needed
##    ??? Recompute if the value changes?
##
##    Patterned after example about mean
##

##
## Create a "matrix" object that can cache its inverse
##
makeCacheMatrix <- function(x = matrix()) {
      ## initialize inverse to avoid error for getinverse 
      i<- NULL
      
      ## setters and getters for matrix
      set<- function(y){
            x<<- y
            i<<- NULL
      }
      get<- function() x
      
      ## setters and getters for inverse of matrix
      setinverse<- function(solve) i<<- solve
      getinverse<- function() i
      
      ## create the return objection, which is list for "calls"
      list( get=get
            ,getinverse=getinverse
            ,set=set
            ,setinverse=setinverse
            )
}

##
## Calculate inverse of "matrix" object created with function makeCacheMatrix.
##
##    Check if inverse already 
##      If so, get inverse from cache (skip computation)
##	  Else, calculate inverse then set using function setinverse
##
cacheSolve <- function(x, ...) {
      ## Get local copy of i, the  inverse of input matrix object x
      i<- x$getinverse()
      
      ## If inverse already exists then return it rather than calculate
      if( !is.null(i) ){
            message("Getting cached data...")
            return(i)
      }
      
      ## Otherwise calculate inverse, then set and return it
      i<- solve( x$get() )
      x$setinverse(i)
      i
}
