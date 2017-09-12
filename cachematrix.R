## Cache the inverse of a matrix.

##Make a list that sets the value of a matrix, gets the value of a matrix, sets the matrix inverse, 
##and gets the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y=matrix()){
    x<<-y
    inv<-NULL
  }
  get<-function() x
  setinverse<-function(inverse) inv<<-inverse
  getinverse<-function() inv
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Calculates the inverse of a matrix if it hasn't been calculated yet

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
  inv<-x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data, ...)
  x$setinv(inv)
  inv
}
