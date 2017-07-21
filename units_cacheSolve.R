# Matrix inverse unit tests


# Unit 0
# display test 1

rm(list = ls()) # clean the environment
source('C:/cdjProgramming/coursera/r/week3/ProgrammingAssignment2/cachematrix.R') # load functions to be tested

verboseQ <- FALSE
testMatrix <- rbind(c(1, -1/4), c(-1/4, 1))

testMatrixInv <- solve(testMatrix)
cacheMatrixInv <- cacheSolve(makeCacheMatrix(testMatrix), verboseQ)

message("Original: ")
print(testMatrix)
message("Solve: ")
print(testMatrixInv)
message("cacheSolve: ")
print(cacheMatrixInv)

message("Unit 0, disp1: Test complete.")


# Unit 1
# accuracy test 1

rm(list = ls()) # clean the environment
source('C:/cdjProgramming/coursera/r/week3/ProgrammingAssignment2/cachematrix.R') # load functions to be tested

verboseQ <- FALSE
testMatrix <- rbind(c(1, -1/4), c(-1/4, 1))

testMatrixInv <- solve(testMatrix)
cacheMatrixInv <- cacheSolve(makeCacheMatrix(testMatrix), verboseQ)

if(all(testMatrixInv == cacheMatrixInv))
{
  message("Unit 1, acc1: Success")
} else
{
  message("Unit 1, acc1: Fail")
}


# Unit 2
# accuracy test 2 - Vandermonde

rm(list = ls()) # clean the environment
source('C:/cdjProgramming/coursera/r/week3/ProgrammingAssignment2/cachematrix.R') # load functions to be tested

verboseQ <- TRUE
testMatrix <- rbind(c(1, 1, 1, 1), c(2, 3, 4, 5), c(4, 9, 16, 25), c(8, 27, 64, 125))

testMatrixInv <- solve(testMatrix)
cacheMatrixInv <- cacheSolve(makeCacheMatrix(testMatrix), verboseQ)

if (all(testMatrixInv == cacheMatrixInv))
{
  message("Unit 2, acc2: Success")
} else
{
  message("Unit 2, acc2: Fail")
}


# Unit 3
# cache test 1 - Vandermonde

rm(list = ls()) # clean the environment
source('C:/cdjProgramming/coursera/r/week3/ProgrammingAssignment2/cachematrix.R') # load functions to be tested

verboseQ <- FALSE
testMatrix <- rbind(c(1, 1, 1, 1), c(2, 3, 4, 5), c(4, 9, 16, 25), c(8, 27, 64, 125))

testMatrixInv <- solve(testMatrix)
cm <- makeCacheMatrix(testMatrix)
cacheMatrixInv <- cacheSolve(cm, verboseQ)
cacheMatrixInv <- cacheSolve(cm, verboseQ)

if (all(testMatrixInv == cacheMatrixInv))
{
  message("Unit 3, cache1: Success")
} else
{
  message("Unit 3, cache1: Fail")
}


# unit 4
# many test 1: integer matrices with integer inverses

rm(list = ls()) # clean the environment
source('C:/cdjProgramming/coursera/r/week3/ProgrammingAssignment2/cachematrix.R') # load functions to be tested

verboseQ <- FALSE
result <- c()
maxruns <- 100
peek <- sample(1:maxruns, 1, replace = TRUE)
for (i in 1:maxruns)
{
  a <- sample(1:10, 1, replace = TRUE)
  b <- sample(1:10, 1, replace = TRUE)
  c <- sample(1:10, 1, replace = TRUE)
  d <- sample(1:10, 1, replace = TRUE)
  e <- sample(1:10, 1, replace = TRUE)
  f <- sample(1:10, 1, replace = TRUE)
  
  factor1 <- rbind(c(1, a, b), c(0, 1, c), c(0, 0, 1))
  factor2 <- rbind(c(1, 0, 0), c(d, 1, 0), c(e, f, 1))
  
  testMatrix <- factor1 %*% factor2
  cm = makeCacheMatrix(testMatrix)
  
  testMatrixInv <- solve(testMatrix)
  cacheMatrixInv <- cacheSolve(cm, verboseQ)
  
  # we just pick one of these to show the user
  if (i == peek)
  {
    message("Unit 4: Peeking into run #", i)
    message("Original: ")
    print(testMatrix)
    message("Solve: ")
    print(testMatrixInv)
    message("cacheSolve: ")
    print(cacheMatrixInv)
  }
  
  if(all(testMatrixInv == cacheMatrixInv) & all(testMatrix == cm$getOriginal()))
  {
    result <- c(result, TRUE)
  } else
  {
    message("Unit 4, many1[", i, "]: Fail:")
    message("Failed on base matrix", testMatrix)
    result <- c(result, FALSE)
  }
  
}

if (all(result == c(TRUE)))
{
  message("Unit 4, many1: All ", maxruns, " tests passed.")
} else
{
  message("Unit 4, many1: Some tests failed, see above messages.")
}


## Interestingly the integer matrix generator once came up with this almost-singular matrix,
## showing that Solve() can choke on ill-conditioned matrices (condition number ~ 50,000 here!).
##
##    Original
##    [,1] [,2] [,3]
##    [1,]  109   54    9
##    [2,]   12    6    1
##    [3,]    2    5    1
##
##    Inverse
##    [,1] [,2]          [,3]
##    [1,]    1   -9 -4.106807e-15
##    [2,]  -10   91 -1.000000e+00
##    [3,]   48 -437  6.000000e+00
##
## but the coded inverse was the same as Solve()'s (i.e. both wrong), so the test passes.



##### end units




