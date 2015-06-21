## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## 1
## the following function creates a special "matrix" object 
## that can cache its inverse

## makeCacheMatrix creates and retuns a list
## include: set, get, set_inv, get_inv
## in the function uses <<- assigment operator so that
## the internal VARs cannot exposed to outside enviroment

makeCacheMatrix <- function(x = matrix()) {
  # to store inverse Matrix result
  inversem <- NULL
  
  # set a matrix that create by "makeCacheMatrix"
  set <- function(y) {
    x <<- y;
    #initialize inverse matrix to NULL
    inversem <<- NULL;
  }
  
  #return input matrix
  get <- function() x
  
  #set inverse matrix
  set_inv <- function(inv) inversem <<- inv
  # return inverse matrix
  get_inv <- function() inversem
  
  #return a list that contain the following 4 function
  list(set = set, 
       get = get, 
       set_inv = set_inv, 
       get_inv = get_inv))
}


## Write a short comment describing this function

## 2
## following is to creats a function computes the inverse of 
## the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  #get inverse Matrix from X
  m <- x$get_inv()
  # check if the inverse matrix existence
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # if not: calculate the inverse matrix
  m <- solve(x$get())
  # set the inverse matrix
  x$set_inv(m)
  m
}


