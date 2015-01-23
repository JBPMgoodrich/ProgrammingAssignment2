
##
## R function that caches a potentially time-consuming computations
## 
##    Cache the inverse of a matrix
##    Look up value from cache when needed
##
##    Patterned after example about mean
##

##
## Constructor for "matrix" object that can cache its inverse
##
makeCacheMatrix <- function(x = matrix()) {
      ## initialize local variable inverse to avoid error on some calls to getinverse 
      i<- NULL
      
      ## setters and getters for matrix
      set<- function(y){
            x<<- y
            i<<- NULL   ## when setting new value of matrix, its inverse should be "cleared"
      }
      get<- function() x
      
      ## setters and getters for inverse of matrix
      setinverse<- function(inverse) i<<- inverse
      getinverse<- function() i
      
      ## create the return object, which is a list of function "calls"
      list( get=get
            ,getinverse=getinverse
            ,set=set
            ,setinverse=setinverse
            )
}

##
## Calculate inverse of object type "matrix"
##
##    Check if inverse already cached
##      If so, retrieve (skip computation)
##	  Else, calculate then set using function setinverse
##
cacheSolve <- function(x, ...) {
      ## Get local copy of i, the  inverse of input matrix object x
      i<- x$getinverse()
      
      ## If inverse already exists then return it rather than re-calculate
      if( !is.null(i) ){
            message("Getting cached data...")
            return(i)
      }
      
      ## Otherwise calculate inverse, then set and return it
      i<- solve( x$get() )
      x$setinverse(i)
      i
}


## Test Cases
##
## Case 1   1x1
##    matrix<- makeCacheMatrix(1)
##    cacheSolve(matrix)      ## 1st call caches inverse
##    cacheSolve(matrix)      ## 2nd call retrieves inverse
##
## Case 2   2x2
##    matrix<- makeCacheMatrix( matrix(1:4 ,nrow=2) )
##    cacheSolve(matrix)      ## 1st call caches inverse
##    cacheSolve(matrix)      ## 2nd call retrieves inverse
##
## Case 3   3x3   [located solvable 3x3 on internet]
##    matrix<- makeCacheMatrix( matrix(c(1,0,5,2,1,6,3,4,0) ,nrow=3) )
##    cacheSolve(matrix)      ## 1st call caches inverse
##    cacheSolve(matrix)      ## 2nd call retrieves inverse

