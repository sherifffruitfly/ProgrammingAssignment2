## Put comments here that give an overall description of what your
## 

## Write a short comment describing this function

makeCacheMatrix <- function(mat = matrix())
{
  cachedInv <- NULL
  cachedOriginal <<- mat
  
  set <- function(y)
  {
    mat <<- y
    cachedInv <<- NULL
  }
  
  get <- function()
  {
    mat
  }
  
  setInv <- function(inv)
  {
    cachedInv <<- inv
  }
  
  getInv <- function(inv)
  {
    cachedInv
  }
  
  getOriginal <- function(inv)
  {
    cachedOriginal
  }
  
  list(
    set = set
    , get = get
    , setInv = setInv
    , getInv = getInv
    , getOriginal = getOriginal
  )
}


## cacheSolve returns a matrix, which is the inverse of the supplied makeCacheMatrix object. 
## If run once, the inverse is calculated, and then cached in parent environment. Repeated runs on the same argument will return the cached answer.
## The actual inverse calculation is performed by solve(), andt is subject to all it's limitations.

cacheSolve <- function(mat, ...)
{
  inv <- mat$getInv()
  
  if(!is.null(inv))
  {
    message("getting cached data")
    return(inv)
  }
  
  data <- mat$get()
  inv <- solve(data, ...)
  mat$setInv(inv)
  inv
}

