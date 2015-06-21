
## Solve for an inverse matrix of a given matrix, however firstly it checks whether it was computed before
## First it sets the given matrix in universe, then it gets it, then it sets its mean and gets the mean


##This part returns list of functions to set and get values of matrix and its mean

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  
  ## sets the matrix
  setz<-function(y){
    x<<-y
    inv<<-NULL
  }
  ##gets the matrix
  getz<-function()x
  
  ##sets the inverse
  setzinv<-function(solve) inv <<-solve
  
  ##gets the inverse
  getzinv<-function() inv
  
  #returns functions above
  list(setz=setz, getz=getz, setzinv=setzinv, getzinv=getzinv)
  
}


## Here we compute the inverse matrix, however check first whether it was computed before, if it was ->
## -> reuturn cached value. If it wasn't then compute the inverse

cacheSolve <- function(x, ...) {
  ## Firstly, check if it was computed
  inv<-x$getzinv()
  
  if (!is.null(inv)) {
    message("Getting Cached Data")
    ## returns cached matrix
    return(inv)
  }
  
  ## If inverse wasn't calculated we compute it here
  v <- x$getz()
  inv <- solve(v)
  
  # After computation we cache it
  x$setzinv(inv)
  
  #returns inverse matrix
  inv
  
}

##Sample input

x<-matrix(1:4,2,2)
x1<-makeCacheMatrix(x)
#computing first time
cacheSolve(x1)
#computing second time
cacheSolve(x1)
#checking if the answer is right
solve(x)