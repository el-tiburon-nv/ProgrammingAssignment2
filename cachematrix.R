## makeCacheMatrix introduces two variables within the
## function object itself (x and m) and a set of nested
## functions (get, set, getinverse, and setinverse) that 
## allows a matrix and its inverse to be cached, retrieved,
## and updated during the life of the makeCacheMatrix object.
##
## The function returns a list corresponding to the 
## various operations/functions that can be invoked
## on a makeCacheMatrix object (i.e. $get(), $set(), 
## $getinverse(), and $setinverse()).

makeCacheMatrix <- function(x = matrix()) {
        ## set cached matrix inverse (m) to NULL
        m <- NULL
        
        ## cache a new matrix in x ($set()) and throw away 
        ## the previously cached matrix inverse value (m)
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## return the currently cached matrix ($get())
        get <- function() x
        
        ## cache a new matrix inverse ($setinverse())
        setinverse <- function(inverse) m <<- inverse
        
        ## return the currently cached matrix inverse ($getinverse())
        getinverse <- function() m
        
        ## return a list of operations allowable on a 
        ## makeCacheMatrix object
        list(set = set, get = get,
             setinverse = setinverse,
	     getinverse = getinverse)
          
}


## Returns the inverse of a matrix that was previously 
## cached via a call to function makeCacheMatrix().
## Note: parameter x represents a makeCacheMatrix object;
## makeCacheMatrix must be called before cacheSolve is
## called.
cacheSolve <- function(x, ...) {
          ## try to get the inverse for the matrix from cache
          m <- x$getinverse()
          
          ## if the inverse isn't null, i.e. it's in the cache, 
          ## then issue a message that the cache value is being 
          ## returned
          if(!is.null(m)) {
                  message("getting cached data")
          } else {
                  ## otherwise, compute the inverse for
                  ## the matrix and then cache it 
                  cachedMatrix <- x$get()
                  m <- solve(cachedMatrix, ...)
                  x$setinverse(m)
          }
          
          ## returns a matrix that is the inverse of 'x$get()'
          m
}

