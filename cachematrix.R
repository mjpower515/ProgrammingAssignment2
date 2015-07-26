## Together these functions will take the inverse of a matrix. In the case of needing to repuse this inverse
##throughout the program it will have cached the reults making it more efficient.

## This functions takes cahces the original matrix as well as functions to be used later

makeCacheMatrix <- function(x = matrix()){
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i<<-inv
  getinverse <- function() i
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}



## This function wiill check for the previously cached inverse and if it does not exist find the inverse. 
##It then outputs the inverse.

cacheSolve <- function(x,...){
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <-x$get()
  i <- solve(data,...)
  x$setinverse(i)
  i
}
  
