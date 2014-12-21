##*********************************************************
## Function stores an initial matrix and contains getters
## and setters that allow the matrix and inverse matrix
## to be changed and read.
##*********************************************************
makeCacheMatrix <- function(x = matrix()) {
  
  ## inverse matrix "invmat" is set to NULL
  invmat <- NULL
  
  ## setter function to set matrix x to the matrix y
  set <-function(y){
    x <<- y
    invmat <<- NULL  ## reset inverse matrix to NULL if matrix x is changed
  }  
  
  ## getter function to return matrix x
  get <-function(){ x }
  
  ## calculates and stores the inverse of matrix x into "invmat"
  setinverse <-function(ymat){
    invmat<<-ymat
  }
  
  ## returns current "invmat"
  getinverse<-function() { return(invmat) }
  
  ## required by calling object so it knows how to access these internal
  ## methods
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

##*********************************************************
## Return a matrix that is the inverse of 'x'
## It will try to use cached value if it exists, otherwise
## it is recalculated in this function
##*********************************************************
cacheSolve <- function(x, ...) {

  ## retrieves currently saved inverse matrix into "invmat"
  invmat <- x$getinverse()

  ## if a value other than NULL is returned, then use this "cached" value
  if(!is.null(invmat)){
    message("getting cached data")
    return (invmat)
  }
  
  ## inverse matrix is NOT already cached, time to calculate
  ## grab the matrix in x
  mymat<-x$get()  
  
  ## calculate inverse matrix
  invmat<-solve(mymat)
  
  ## store this invmat
  x$setinverse(invmat)
  
  return(invmat)
  ##return(x$get())
}

        