## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
library(MASS)

makeCacheMatrix <- function(x = matrix()) {
  
  invmatrix<-NULL            
  set<-function(y){
    x<<-y
    matrix<<-NULL
  }
  get<-function()x            
  setinv<-function(inverse)  invmatrix<<-inverse
  getinv<-function(){ 
    inver<-ginv(x)
    inver%*%x          
  }
  list(set = set, get = get, 
       setinv = setinv, 
       getinv = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getinv()                  
  if(!is.null(inv)){                 
    message("Received Cached Data!")
    return(inv)
  }                      
  
  data<-x$get()
  matrix<-solve(data,...)             
  x$setinv(inv)
  inv   ## Return a matrix that is the inverse of 'x'
}

