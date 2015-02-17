## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This function can cache its inverse, It gets a vector x and return a special "matrix" of 4 functions.
makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
# if our input changed, define a function to set a new vector as
#input. this will be one of four functions in list
        set <- function(y){
                x <<- y      #set new vector as x
                s <<- NULL   #rest the inverse
        }
#define a function which returns our vector.
        get <- function()x   
#define a function which get the solve of our vector and set it in 's'
        setinverse <- function(solve) s <<-solve 
#define a function which returns the 's'
        getinverse <- function()s
#make a list with our four functions
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Write a short comment describing this function
#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If there was not the inverse of the underlying vector in cache, it calculates the inverse of vector, but if the inverse 
#has been calculated before(and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  #get the inverse of matrix and assign it to the "s"
  s <- x$getinverse()
  # if s isn't empty, then inverse has calculated before and show
  # this. It returns the inverse
  if(!is.null(s)){
    message("getting cached data")
    return(s)
  }
  # if s is empty and inverse of matrix hasen't calculated before
  # get the x amount and calculate its inverse by solve function
  data <- x$get()
  s <- solve(data)
  #set 's' as inverse of current matrix to use it later
  x$setinverse(s)
  s #show 's' as output
}
