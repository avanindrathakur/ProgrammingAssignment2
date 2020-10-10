makeCacheMatrix <- function(x= matrix()){
  inv<- NULL # initializing inverse function as null
  set<- function(y){
    x<<- y
    inv <<- NULL
  }
  get<- function(){x} # function to get the matrix x
  setInverse <- function(inverse) {inv<<- inverse}
  getInverse <- function() {inv} 
  list(set=set, get=get, setInverse=setInverse, getInverse= getInverse)
}

cacheSolve <- function(x, ...){
  inv<-x$getInverse() #function to get inverse
  if(!is.null(inv)) {    #checking whether inverse is null
    message("getting cached data")
    return(inv)
  }  #function to get cached data
  mat<-x$get()
  inv<-solve(mat, ...)  #calculating the value of inverse
  x$setInverse(inv)
  inv  #returning the inverse value
}