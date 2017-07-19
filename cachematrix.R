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


## cacheSolve2 improves cacheSolve by working on shallow copies instead of only deep copies
## cacheSolve2 searches cache environment for any cacheMatrix objects matching the input, and gets their input
cacheSolve2 <- function(mat, ...)
{
  # check current matrix for cached inverse
  inv <- mat$getInv()
  
  if(is.null(inv))
  {
    # searches parent environment (cache) for identical cached matrix & inverse
    cache_obs <- ls(parent.env(environment()))
    obs_wo_inverse <- c()
    
    # check all the objects
    if (length(cache_obs) > 0)
    {
      for (i in 1:length(cache_obs))
      {
        # objects of length 5 (the makecachedmatrix list length) are what we want
        if (length(get(cached_obs[i]) == 5))
        {
            # elements 4 and 5 have known names
            if (names(get(cached_obs[i])[4])[1] == "getInv" & names(get(cached_obs[i])[5])[1] == "getOriginal")
            {
              message("Checking ", cache_obs[i], " for equality with input.")
              
              # check cached object for value identity
              if (all(get(cached_obs[i])$getOriginal() == mat))
              {
                # we have same cached matrix - now check for cached inverse
                inv <- get(cached_obs[i])$getInv()
                if(!is.null(inv))
                {
                  message("Getting cached inverse from object ", cache_obs[i])
                  mat$setInv(inv)
                  return(inv)
                } else
                {
                  message("Object ", cache_obs[i], " has no inverse cached.")
                          obs_wo_inverse <- c(obs_wo_inverse, cache_obs[i])
                }
              }
            }
        }
      }
    }
    
    # we didn't hit that return in the innermost if above, so calculate the inverse on current object
    data <- mat$get()
    inv <- solve(data, ...)
    mat$setInv(inv)
    
    # now we set the inverse for all identical cacheMatrix objects that were found without inverses, so we don't have to go thru this again on them.
    for (i in 1:length(obs_wo_inverse))
    {
      get(obs_wo_inverse[i])$setInv(inv)
    }
    
  } else
  {
    message("Getting cached inverse from current object.")
    return(inv)
  }
}
