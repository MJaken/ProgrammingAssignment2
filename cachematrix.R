## Create a "special" matrix - a list of functions and solve and store cached 
## inverse of input matrix


## makeCacheMatrix FUN creates a list of functions inside an object and  
## caches matrix itself and it's inverse if it was set or cached and solved by 
## next function

makeCacheMatrix <- function(x = matrix()) {
  
      inv <- NULL
      
      set <- function(y) {  ##FUN to set new matrix into "special" matrix
            x <<- y
            inv <<- NULL
      }
      
      get <- function() x   ##Returns "special" matrix's inside matrix
      
      setinv <- function(inverse) inv <<- inverse ## Sets matrix's inverse cache
      
      getinv <- function() inv ##Returns cached inverse
      
      ##Finally creating list of FUNs that makes our "special" matrix
      list(set = set, get = get, setinv = setinv, getinv = getinv) 
      
}


## cacheSolve FUN solves matrix's inverse and sets(caches) it to "special" matrix
## in case it's not cached already

cacheSolve <- function(x, ...) {
  
      inv <- x$getinv() ## Get cached inverse from "special" matrix
      
      if(!is.null(inv)) { ## Check if cached inverse is not NULL, if it' not:
            message("getting cached data") ## Returning cached inverse 
            return(inv)
      }
        else {
            data <- x$get()  ## Get matrix 
            inv <- solve(data, ...) ## Solve for it's inverse and load it to inv
            x$setinv(inv) ## Set inverse to cache of "special" matrix
            
        }
        inv ## Return a matrix that is the inverse of 'x'
}
