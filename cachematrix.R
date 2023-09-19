## Put comments here that give an overall description of what your
## functions do

## There are two functions makeCacheMatrix, makeChaceMatrix
## makeCacheMatrix consists of set, get, setinv, getinv
##library(Mass) is used to calculate inverse for non squared as well as square matrices
+library(MASS)
makeCacheMatrix <- function(x = matrix()) {
 inv<-NULL          #initializing inverse as NULL
 set<-function(y){
                x<<-y
                inv<<-NULL
                }
 get<-function()x              #function to get matrix x
 setinv<-function(inverse)inv<<-inverse
 getin<-function(){
                   inver<-ginv(x)
                   inver%*%x                #function to obtain inverse of the matrix
                   }
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  }


## Write a short comment describing this function
## This is used to get the cache data

cacheSolve <- function(x, ...) ##gets cache data
 {
  inv<-x$getinv()
  if(!is.null(inv)) {               #checking whether inverse is NULL
                         message( "getting cached data")
                         return(inv)                  #return inverse value
  }
  data<-x$get()
  inv<-solve(data,...)                 #calculates inverse value
  x$setinv(inv)
  inv  ## Return a matrix that is the inverse of 'x'
}
  

        
