## makeCacheMatrix will act to store a vector and it's inverse when we use the 
## solve funcion in cacheSolve so that when we need the same inverse matrix 
## again we dont have to recalculate it. 

## This is will store the value of the original matrix (x) and allow cacheSolve 
## to get and set the value of m depending on whether it has been previously 
## calculated and stored.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL # default value of matrix is NULL
      set <- function(y) { # this allows us to change the contents of the same 
                           # matrix and return the default value to null so that 
                           # CacheSolve know the value of the matrix needs to be
                           # recalculated using x$setinv where x is the name of 
                           # the matrix
            x <<- y
            m <<- NULL
      }
      get <- function() x # this allows another function  to "extract" the
                          # matrix we input as x
      setinv <- function(solve) m <<- solve # this allows us to set the value of
                                            # m with another function
      getinv <- function() m # this allows us to get the value of m with another
                             # function, stored previously as inverse matrix or 
                             # NULL
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv) # gives a list of the function defined above so that
                            # we can extract the values as we need them with the
                            # $ operator
}


## This will access the makeCacheMatrix function to grab the matrix we input and
## either 'extract' the inverse matrix if previously calculated or solve the
## inverse of the matrix if the value of m is NULL and set the value in 
## makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinv()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
            # if makeCacheMatrix has a value other than Null, will return the
            # stored value
      }
      data <- x$get() # store the matrix input into makeCacheMatrix as data
      m <- solve(data, ...) # solve the inverse of the matrix with other
                            # arguments
      x$setinv(m) # set the value of m in makeCacheMatrix for future access
      m # prints the inverse of the matrix
}
