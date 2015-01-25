### Program Assignment 2 ###

###This function creates a special kind of matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL        #initalizing the inverted matrix as a NULL matrix 'inv'
  
  set <- function(y){        #method to initialize the matrix to be inverted 'matrix'
    matrix <<- y             #setting its values with the values of the typed matrix 'y' 
    y <<- NULL
  }
  
  get <- function(){         #method to return the matrix to be inverted 'matrix'
    matrix
  }

  set_inv <- function(inverse){         #method to set the values of the inverted matrix 'ínv' and store it
    inv <<- inverse                    
  }
  
  get_inv <- function(){                  #method to return the inverted matrix 'inv'
    inv
  }
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)     #return the list of methods

}


#This functions checks if the matrix was already inverted, then print it if yes; else it inverts the matrix
#and save it in the upper-environment variable 'inv', then returns the matrix 'ínv'
cacheSolve <- function(x, ...) {
 
  inv <- x$get_inv()           #local variable 'inv' gets the value of the upper-environment variable 'inv'
  
  if(!is.null(inv)) {                           #if 'the inverted matrix was already calculated, then 'inv' 
    message("getting cached data")              #will ve different than NULL. In this case, just print 'inv'.
    return(inv)
  }

  matrix <- x$get()                   #local var 'matrix' gets the upper-environment var 'matrix'
  m <- solve(matrix)                  #m gets the inverse of the local var 'matrix'
  x$setInverse(m)                     #upper-environment var ínv' gets the local var 'm' 
  return(x)                           #print the inverted matrix 'm' 

}
