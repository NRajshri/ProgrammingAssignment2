

#This first function creates a special "matrix" object that can cache its 
#inverse and will :

# set the value of the matrix
# get the value of the matrix
# set the value of the inverse matrix
# get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()){
  
  invMat <- NULL
  
  set <- function(y) {
    
    x <<- y
    invMat <<- NULL
    
  }
  
  get <- function() x
  setinv <- function(inverseMatrix) invMat <<- inverseMatrix
  getinv <- function() invMat
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
  
}

#cacheSolve: This function computes the inverse of the special "matrix" 
#returned by makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve the 
# inverse from the cache


cachesolve <- function(x, ...){
  
  invMat <- x$getinv()
  
  if(!is.null(invMat)){
    
    message("getting the cached data")
    return(invMat)
    
  }
  
  data <- x$get()
  
  invMat <- solve(data) # solve(X) returns the inverse of a matrix.
  x$setinv(invMat)
  invMat
  
}


# Test Trial run

# testMatrix <- rbind(c(9, -6.3), c(-6.3, 9))
# mc <- makeCacheMatrix(testMatrix)
# mc$get()
# 
#      [,1] [,2]
# [1,]  9.0 -6.3
# [2,] -6.3  9.0
# 
# 
# First run : No result in cache
# cachesolve(mc)
#           [,1]      [,2]
# [1,] 0.2178649 0.1525054
# [2,] 0.1525054 0.2178649


# Second run : bringing the result from cache
# cachesolve(mc) 
# getting the cached data
# [,1]      [,2]
# [1,] 0.2178649 0.1525054
# [2,] 0.1525054 0.2178649
# 
# 

