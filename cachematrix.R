## Put comments here that give an overall description of what your
## functions do

##My assignment is to write a pair of functions that cache the inverse
 #of a matrix.

## Write a short comment describing this function

##The makeCacheMatrix is a function that creates a special "matrix" object that can
 #cache its inverse. It is of argument x which is set to be a matrix
 #and the inv is set as NULL.
 #set function takes y argument where the default input of y would be same as x. It also
 #enables us to set a new input of x for the environment as well as resets
 #any cached inv value to NULL. Get function calls the input vector for x argument, 
 #setinverse function sets the inverse value where the default
 #value of inverse would be the same as inv & getinverse function calls the inv of the matrix.


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x    
  
  setinverse <- function(inverse) {
    inv <<- inverse
  }
  
  getinverse <- function() inv
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

##cacheSolve: This function computes the inverse of the special
 #"matrix" returned by the aforementioned makeCacheMatrix. If the inverse has already 
 #been calculated (and the matrix has not changed), then the cacheSolve 
 #retrieves the inverse from the cache .
 #The function casheSolve takes "t" as the argument. t is a makeCacheMatrix object.
 #Then the getiverse function (which calls the cashed inverse of the makeCacheMatrix function)
 #within the t object is called and set as inv. If the value of inv is not NULL
 #then it shows the message "getting cached data". Then it returns inv. This is
 #done by using an If condition. Then the get function
 #of the t object is called and the output matrix is set as data. Then the inverse
 #of data (which is a matrix) is calculated and set as inv. Then the value of inv is 
 #cached using the setinverse() function of the t object. 
 #Finally the inv is returned as the output of this function..

cacheSolve <- function(t) {
  inv <- t$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- t$get()
  inv <- solve(data)
  t$setinverse(inv) #setting inverse in the cache
  inv #output
}
