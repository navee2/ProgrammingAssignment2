## Following code retuns inverse of an invertible matrix
## If inverse has already been calculated it doesn't calculate it again
"Test :-
z<- matrix(rnorm(4),2,2)
m<- makeCacheMatrix(z)
cacheSolve(m) ## matrix inverse calculation takes here
cacheSolve(m) ## This time cache output is printed
solve(z) ## to check correctness of output"
  

### Function makeCacheMatrix assigns getters and setters
### There is a getter and setter for matrix and another pair for inverse
### the list of functions returned here is then used in cachesolve

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  #setter for matrix
  set <- function(y) {
    x <<- y ### assign value in parent environment
    m <<- NULL
  }
  #getter for matrix
  get <- function() x
  #setter for inverse
  setinv <- function(inv) m <<- inv
  #getter for inverse
  getinv <- function() m
  #retuns the list containing four functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Following function check if inverse has already been calculated.
## If so it retuns the calculated inverse else returns newly calculated
## return

cacheSolve <- function(x, ...) {
  ##Checking if there exists inverse in the environment
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m) ## returning preexisting inverse
  }
  ## calculate inverse as no pre-existing inverse found
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}
