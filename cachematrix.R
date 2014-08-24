## Create a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  I <- NULL
  
  set <- function(y) {              ## Input matrix
    x <<- y
    I <<-  NULL
  }
  get <- function() x               ## Return matrix
  setInverse <- function(Inverse) I <<- Inverse       ## Cache inverse
  getInverse <- function() I                            ## Return the cached value
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Checks to see if the matrix has an inverse cached. If not, it returns the inverse and caches it

cacheSolve <- function(x, ...) {
  I <- x$getInverse()
  
  if(!is.null(I)) {          ## Check if the inverse in cached
    message("getting cached data")
    return(I)                ## Return the cached inverse
  }
  matrix <- x$get()            
  I <- solve(matrix, ...)           ## Caculate the inverse
  x$setInverse(I)            ## Cache inverse
  I                          ## Return inverse
}
