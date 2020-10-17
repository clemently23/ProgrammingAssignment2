## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#The first function, makeCacheMatrix creates a list containing a function to

#set the value of the matrix
#get the value of the matrix
#set the matrix of the inverse
#get the matrix of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<-y
    m <<- NULL
  }
  get <- function() x 
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  
  list(set = set, get = get, setinverse= setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

#The following function calculates the inverse of the matrix set in the above function  
#However, it first checks to see if the inverse has already been calculated. 
#If so, it gets the inverse from the cache and skips the computation. 
#Otherwise, it calculates the inverse of the matrix and sets the inverse in the cache via the solve function.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message('getting cached data')
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}

#testing
test_matrix <- matrix(c(-3,5,1,0), nrow =2, ncol =2)
?solve
solve(test_matrix)
t <- makeCacheMatrix(matrix(c(-3,5,1,0), nrow =2, ncol =2))
t1 <- makeCacheMatrix(matrix(c(-3,6,1,0), nrow =2, ncol =2))
cacheSolve(t)

