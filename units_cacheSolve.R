# Matrix inverse unit tests


# Unit 1
# accuracy test 1
rm(list = ls()) # clean the environment
source('C:/cdjProgramming/coursera/r/week3/ProgrammingAssignment2/cachematrix.R') # load functions to be tested

testMatrix <- rbind(c(1, -1/4), c(-1/4, 1))

testMatrixInv <- solve(testMatrix)
cacheMatrixInv <- cacheSolve(makeCacheMatrix(testMatrix))

if(all(testMatrixInv == cacheMatrixInv))
{
  print("Unit 1, acc1: Success")
} else
{
  print("Unit 1, acc1: Fail")
}


# Unit 2
# accuracy test 2
rm(list = ls()) # clean the environment
source('C:/cdjProgramming/coursera/r/week3/ProgrammingAssignment2/cachematrix.R') # load functions to be tested

testMatrix <- rbind(c(1, 1, 1, 1), c(2, 3, 4, 5), c(4, 9, 16, 25), c(8, 27, 64, 125))

testMatrixInv <- solve(testMatrix)
cacheMatrixInv <- cacheSolve(makeCacheMatrix(testMatrix))

if (all(testMatrixInv == cacheMatrixInv))
{
  print("Unit 2, acc1: Success")
} else
{
  print("Unit 2, acc2: Fail")
}


# Unit 3
# cache test 1
rm(list = ls()) # clean the environment
source('C:/cdjProgramming/coursera/r/week3/ProgrammingAssignment2/cachematrix.R') # load functions to be tested

testMatrix <- rbind(c(1, 1, 1, 1), c(2, 3, 4, 5), c(4, 9, 16, 25), c(8, 27, 64, 125))

testMatrixInv <- solve(testMatrix)
cm <- makeCacheMatrix(testMatrix)
cacheMatrixInv <- cacheSolve(cm)
cacheMatrixInv <- cacheSolve(cm)

if (all(testMatrixInv == cacheMatrixInv))
{
  print("Unit 2, cache1: Success")
} else
{
  print("Unit 2, cache1: Fail")
}

