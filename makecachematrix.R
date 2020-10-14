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

source("makeCacheMatrix.R")
cmatrix<-makeCacheMatrix(matrix(4:1, nrow=2, ncol=2))
cmatrix$get()
cacheSolve(cmatrix)
cmatrix$getInverse()
set.seed(1)
rpois(5, 2)
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
getwd()
setwd("F:/Library/R Programing/specdata")
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
names(outcome) 
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])
best <- function(state, outcome) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
}


