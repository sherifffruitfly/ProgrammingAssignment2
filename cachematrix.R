## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(mat = matrix())
{
  cachedInv <- NULL
  
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
  
  list(
    set = set
    , get = get
    , setInv = setInv
    , getInv = getInv
  )
}


## Write a short comment describing this function

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

