
#makeCacheMatrix creates a "matrix" object that can cache its inverse or store the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  n1<-NULL                  
  set<-function(Matrix1){
    x<<-Matrix1               #assigns Matrix1 to x in the parrent environment
    n1<<-NULL                  #create variable n1in parent environment to later store inverse in and clear cache
  }
  get<-function()x                           #getter for matrix x             
  setinverse<-function(solve)n1<<-solve       #setter for inverse n1
  getinverse<-function()n1                    #getter for the inverse n1
  list(set=set, get=get,                      #store   results in list so it can be accessed using the $ operator
       setinverse = setinverse,
       getinverse = getinverse)
}



## CacheSolve returnsva matrix that is the inverse of x. If possible it retrieves the inverse cached using makeCacheMatrix

cacheSolve <- function(x, ...) {
        n1<-x$getinverse                          #call getinverse()function on the input object
        if(!is.null(n1)){                         # checks if n1 != NULL and returns it to parent environment, if a value is found
          message("getting cached data")
          return(n1)                              #retruns cached inverse
        }
    data<-x$get()                                 # if no cached inverse is found, gets the matrix from input object
    n1<-solve(data)                               # calculates solve()
    x$setinverse(n1)                              #sets inverse in the input object
    n1                                            #prints inverse
} 
