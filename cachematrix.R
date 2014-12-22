## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {      #  input x will be a matrix
    m <- NULL                                    #  m will be our 'mean' and it's reset to 
    set <- function(y) {                         #  time makeCacheMatrix is called                        
                   
      x <<- y
      m <<- NULL
    }
    get <- function() x                          # this function returns the value of the original matrix
    setmean <- function(mean) m <<- mean         # this is called by cachemean() during the first cachemean()
                                                          #  access and it will store the value using superassignment
    getmean <- function() m                      # this will return the cached value to cachemean() on
                                                 # subsequent accessess
    list(set = set, get = get,                   # this is a list of the internal functions
         setmean = setmean,
         getmean = getmean)
  }

}


## Write a short comment describing this function

cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                                # the input x is an object created by makeCacheMatrix
  m <- x$getmean()              # accesses the object 'x' and gets the value of the mean
  if(!is.null(m)) {             # if mean was already cached (not NULL) ...
    message("getting cached data") # ... send this message to the console
    return(m)                       # ... and return the mean ... "return" ends 
  }                                 #   the function cachemean(), note
  data <- x$get()                   # we reach this code only if x$getmean() returned NULL
  m <- mean(data, ...)              # if m was NULL then we have to calculate the mean
  x$setmean(m)                      # store the calculated mean value in x (see setmean() in makeCacheMatrix
  m                                 # return the mean to the code that called this function
}
}