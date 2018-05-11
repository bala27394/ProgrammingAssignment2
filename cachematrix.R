## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x   ##################Getting the data##############
  setinv <- function(inv) m <<- inv ######Caching the inversed data#####
  getinv <- function() m
  list(
    set = set,
    get = get,
    setinv = setinv,
    getinv = getinv
  )
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)###############Inverse matrix function###############
  x$setinv(m)
  m
}


 ###Sample output

# > y<-matrix(rnorm(4),2,2)
# > y
# [,1]       [,2]
# [1,]  0.3984673 -0.2677129
# [2,] -0.6380710  0.3598796
# > k<-makeCacheMatrix(y)
# > k$get()
# [,1]       [,2]
# [1,]  0.3984673 -0.2677129
# [2,] -0.6380710  0.3598796
# > cacheSolve(k)
# [,1]       [,2]
# [1,] -13.12489  -9.763554
# [2,] -23.27060 -14.532198
# > cacheSolve(k)
# getting cached data
# [,1]       [,2]
# [1,] -13.12489  -9.763554
# [2,] -23.27060 -14.532198



