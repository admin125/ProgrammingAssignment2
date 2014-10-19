## Put comments here that give an overall description of what your
## functions do

#
# These two functions are supposed to be used complemetary, in order to make
# a matrix for which the inverse is cached. They could be used like this:
#> m=matrix(sample(1:100,rep=T),10,10)
#> mc=makeCacheMatrix(m)
#> cacheSolve(mc)cache<-x$getcache()
# a second (and later) invocation(s) of cacheSolve will retrieve the inverse from cache as
# opposed to recalculating

## Write a short comment describing this function

# makeCacheMatrix returns a list of the following functions
# 1. setcache; stores the inverse of matrix in the cache
# 2. getcache; retrieves the inverse from the cache
# 3. setmatrix; sets the matrix 
# 4. getmatrix; gets the matrix (not used)


makeCacheMatrix <- function(x = matrix()) {
  cache<-NULL;
  setcache<-function(matrix) cache<<-solve(matrix)
  getcache<-function() cache
  setmatrix<-function(matrix) { 
    x<<-matrix
    cache<<-NULL
  }
  getmatrix<-function() x
  list(setcache=setcache,getcache=getcache,setmatrix=setmatrix,getmatrix=getmatrix)
}


## Write a short comment describing this function

# given a vector of functions, use these functions to
# retrieve a cached inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
        cache<-x$getcache()
        if(!is.null(cache)) {
          message("getting cached data")
          return(cache)
        }
        matrix<-x$getmatrix()
        x$setcache(matrix)
        cache<-x$getcache()
        cache
  
}
