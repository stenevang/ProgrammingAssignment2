## This is Assignment 2 of the R-Programming course in the Data Science Specialization
## My name is Theodor Stenevang Klemming and this file is available from
## https://github.com/stenevang/ProgrammingAssignment2
## The assignment is an exercise is lexical scoping and the use of objects in different environments

## This function creates a special "matrix" object that can cache the inverse of a matrix
## It is really a list of elements where each element is a function and the cached inverse of the matrix
## is an object in the Parent Environment of each of those functions

## NOTE NOTE ! In my code, I have removed the set function.
## This is because the set function was actually never run: It is a completely unnecessary function. 
## All the function does is take x from the parent environment, and assign that back to to x in the parent environment.
## And the set function was never called!                


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                         # We are now creating the object and inv is set to NULL - we have not calculated it yet
        
        get <- function() {                 # get is a function that just returns the matrix x
                message("get was run")
                x
        }
        setinv <- function(inverse) {       # setinv is a function called from cacheinverse() taking an argument "inverse" and assigns that to inv in the Parent Environment
                message("setinv was run")
                inv <<- inverse             # inv now contains the cached inverse matrix
        }
        
        getinv <- function() {              # getinv is a function that just returns the cached inverse matrix value stored in inv
                message("getinv was run")
                inv
        }
        list(                               # creating a list with one element for each function, elements named like the functions
                get = get,                  # FOR SOME REASON, = signs MUST be used, not '<-'   
                setinv = setinv,
                getinv = getinv)
}


## This function will return the cached inverse matrix if it exists
## or otherwise calculate the inverse matrix, store it, and return it

cacheSolve <- function(m, ...) {
        inv <- m$getinv()                         # running the getinv function from inside the x list to get the cached inverse matrix
        if(!is.null(inv)) {                       # Unless inv is NULL (getinv returned inv == NULL)
                message("getting cached data")    # then we say that we will get cached data...
                return(inv)                       # and we return inv (which we found saved in the object inv in the makeCacheMatrix() environment)
        }
        mtx <- m$get()                            # But if inv was NULL we need to calculate it so we get the matrix data
        inv <- solve(mtx)                         # then calculate the inverse of the matrix and assign to inv in the environment of cacheSolve()
        m$setinv(inv)                             # then call the setinv function from inside the x list, providing inv as the argument, 
                                                  # storing the inverse matrix as inv in the makeCacheMatrix() environment
        return(inv)                               # and we return inv, the (newly) calculated inverse matrix
}
