# makeCacheMatrix makes an object that allows for ## caching of a matrix and its inverse. cacheSolve
# returns the inverse stored in memory or calculates
# a new one if no inverse has been set yet. 


##Takes an nxn matrix 'mat' and returns a list of
##functions that allow setting and getting of said
##matrix/its inverse. 
makeCacheMatrix <- function(mat = matrix()) {
  i <- NULL ##will store the inverse. 
  
  set <- function(y){
    mat <<- y
    i <<- NULL
  }
  
  get <- function() mat
  
  setInv <- function(inv) {
    i <<- inv
  }
  getInv <- function() i
  
  list(set = set, get = get,
       setInv = setInv, getInv = getInv)
}


## Returns the determinant of a "makeCacheSolve" matrix by
## either calculating and setting it or returning it from memory. 
cacheSolve <- function(mat, ...) {
  inv <- mat$getInv()
  
  if(!is.null(inv)){
    return(inv)
  }
  newInv <- solve(mat$get())##calculate/store the inverse
  mat$setInv(newInv)
  newInv
}


