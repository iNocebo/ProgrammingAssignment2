## Put comments here that give an overall description of what your
## functions do
## -> makeCacheMatrix creates an environment which stores information of the input matrix and the 
##    inverse of the matrix. The information is accessible via functions, which are stored
##    in a list. This list is the main ouptut of the makeCacheMatrix function.

## -> cacheSolve reads and writes content from and to the environment of the makeCacheMatrix
##    function. It calculates 
##    the inverse of a matrix and stores it in the variable m, which is fed back to
##    makeCacheMatrix. It checks whether or not m is NULL before is starts calculating,
##    providing the caching opportunity (if(m == NULL) {start solving the matrix} else
##    {show me the existing value of m == cached value}).

## Write a short comment describing this function.
## -> Please see above each line. I know, not very readable, but I used it to explain it to myself.

makeCacheMatrix <- function(x = matrix()) {
  
  ## First you assign the value NULL to the variable m. This is used later by the caching
  ## function cacheSolve.
  
  m <- NULL
  
  ## The following set function can be used to change a matrix you already put through
  ## the makeCacheMatrix function and the cacheSolve function. With the operator "<<-"
  ## the function assigns the new value (y) to the already existing variable (x) from the parent
  ## environment, the makeCacheMatrix.
  ## Using the already known operator "<-" would only create a new variable, instead of using
  ## the existing variable x defined in the environment of the makeCacheMatrix function.
  
  set <- function(y) {
    x <<- y
    
    ## If you change the matrix used before, m is set to NULL signaling
    ## cacheSolve to calculate the inverse and not to get the cached data. It's the built
    ## in check for changes, which require a new calculation instead of displaying the cached
    ## value.
    
    m <<- NULL
  }
  
  ## get() prints the matrix which was used as input. It needs to be function, because
  ## this way it first checks for a new available value. This is especially important because
  ## of the set() function, which may have altered the matrix in the meantime.
  
  get <- function() x
  
  ## with setinverse() you can add a inverse manually - even one which is not correct
  ## for the matrix used as input. This circumstance is not perfect, because it should not
  ## be possible for a user to change this manually, but it is is needed
  ## for cacheSolve to set the a new inverse of the matrix.
  
  setinverse <- function(inverse) m <<- inverse
  
  ## getinverse() basically prints the calculated inverse of the matrix. It needs to be a function
  ## because this way it always uses the newest value of m.
  
  getinverse <- function() m
  
  ## This is the main output of the function makeCacheMatrix: A list of the functions,
  ## which are accessible via subsetting by writing 'your_variable_name$getinverse()'.
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## -> Please see above each line. I know, not very readable, but I used it to explain it to myself.


cacheSolve <- function(x, ...) {
  
  ## This first line gets the inverse of the matrix out of the environment created by
  ## makeCacheMatrix and stores it in a new local variable (m).
  ## It just sebsets the list for the necessary function and writes the output value to the
  ## variable.
  
  m <- x$getinverse()
  
  ## This is the main feature of this caching function. If the value of m is not NULL the already
  ## existing value of m will be returned, accompanied by the message "getting cached data".
  
  if(!is.null(m)) {
    message("getting cached data")
   
    return(m)
  }
  
  ## If there's no cached data (= m is NULL), the whole input matrix is extracted from the
  ## environment created by makeCacheMatrix by using the 'get' function. The extracted matrix
  ## is assigned to the variable data.
  data <- x$get()
  
  ## Now the function solve, which calculates the inverse of a matrix is called and its ouput
  ## written to the variable m.
  m <- solve(data, ...)
  
  ## Here comes the feedback to the environment of makeCacheMatrix. As I've written above, this
  ## function should actually not be available to the user to manipulate in makeCacheMatrix. But
  ## this function is needed for cacheSolve to feed the calculated value back to makeCacheMatrix.
  x$setinverse(m)
  
  ## As a last step, m is printed after cacheSolve was used.
  m
}
