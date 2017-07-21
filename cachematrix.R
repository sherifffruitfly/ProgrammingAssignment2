## This set of functions sets up an OOP-style matrix object, with methods for 
## setting & getting the matrix and its inverse.
##
## The course-supplied functions are restricted to working on on deep copies of
## the matrix object. The version i supply works on these, but ALSO on mere shallow copies.
##
## If needed, cacheSolve.old is the weaker version the course suggested.


## makeCacheMatrix
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


## cacheSolve improves cacheSolve.old by working on shallow copies instead of only deep copies
## cacheSolve searches cache environment for any cacheMatrix objects matching the input, and gets their cached inverse.
## cacheSole also assigns inverse to all matching parent cacheMatrix objects, minimizing the "need" to ever invert that matrix again.

cacheSolve <- function(mat, verbose=FALSE,  ...)
{
  if (verbose) message("Solve starting in verbose mode.")
  
  # check current matrix for cached inverse
  inv <- mat$getInv()
  
  if(is.null(inv))
  {
    if (verbose) message("No cached inverse found for input. Searching for other matching objects...")
    
    # searches parent environment (cache) for identical cached matrix & inverse
    cache_obs <- ls(parent.env(environment()))
    obs_wo_inverse <- c()
    
    # check all the objects, looking for cacheMatrix objects. there should be a simpler way.
    if (length(cache_obs) > 0)
    {
      for (i in 1:length(cache_obs))
      {
        # objects of length 5 (the makecachedmatrix list length) are what we want
        if (length(get(cache_obs[i])) == 5)
        {
          # elements 4 and 5 have known names
          if (!is.null(names(get(cache_obs[i])[4])[1]) & !is.null(names(get(cache_obs[i])[5])[1]))
          {
            if (names(get(cache_obs[i])[4])[1] == "getInv" & names(get(cache_obs[i])[5])[1] == "getOriginal")
            {
              if (verbose) message("Checking object ", cache_obs[i], " for equality with input.")
              
              # check cached object for value identity
              if (all(get(cache_obs[i])$getOriginal() == mat$get()))
              {
                # we have same cached matrix - now check for cached inverse
                if (verbose) message("Object ",  cache_obs[i], " matches input. Looking for cached inverse.")
                inv <- get(cache_obs[i])$getInv()
                if(!is.null(inv))
                {
                  if (verbose) message("Getting cached inverse from object ", cache_obs[i])
                  mat$setInv(inv)
                  return(inv)
                } else
                {
                  if (verbose) message("Object ", cache_obs[i], " has no inverse cached.")
                  obs_wo_inverse <- c(obs_wo_inverse, cache_obs[i])
                }
              }
            }
          }
        }
      }
    }
    
    # we didn't hit that return in the innermost if above, so calculate the inverse on current object
    if (verbose) message("No matching cached inverse found. Calculating inverse for input, and caching it on all matching existing objects")
    data <- mat$get()
    inv <- solve(data, ...)
    mat$setInv(inv)
    
    # now we set the inverse for all identical cacheMatrix objects that were found without inverses, so we don't have to go thru this again on them.
    if (length(obs_wo_inverse) > 0)
    {
      for (i in 1:length(obs_wo_inverse))
      {
        get(obs_wo_inverse[i])$setInv(inv)
      }
    }
    
    return(inv)
  } else
  {
    if (verbose) message("Getting cached inverse from current object.")
    return(inv)
  }
}


## cacheSolve.old returns a matrix, which is the inverse of the supplied makeCacheMatrix object. 
## If run once, the inverse is calculated, and then cached in parent environment. Repeated runs on the same argument will return the cached answer.
## The actual inverse calculation is performed by solve(), andt is subject to all it's limitations.
## this is the gimpware function expected for the class - only works for deep copies of the cacheMatrix object

cacheSolve.old <- function(mat, ...)
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