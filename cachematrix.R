## Functions helps to save computer time on inverting matrix task by caching 
## Input example for makeCacheMatrix is
## 1. my_matrix1 <- matrix() # create input matrix
## 2. makeCacheMatrix(my_matrix1) # start function
## 3. my_matrix1$... # replace ... to set(), get(), setsolve() or getsolve() to see it's result
## 4. my_matrix2 <- matrix() # create 2nd input matrix
## 5. my_matrix1$set(my_matrix2) # change matrix in cache
## 6. my_matrix1$... # replace ... to set(), get(), setsolve() or getsolve() to use functioin

makeCacheMatrix <- function(x = matrix()){
    ## intitializing makeCacheMatrix objects 
        ## x - matrix by default
        ## im - NULL object
    im <<- NULL
    ## by adding set() to command in row 7 we can change input matrix for cache
    ## also it'll change im to NULL once again if it wasn't empti
    set <- function(y) {
        x <<- y
        im <<- NULL
    }
    ## by adding get() to command in row 7 we can read cached matrix
    get <- function() x
    ## by adding setsolve() to command in row 7 we can start inverse of cached matrix
    setsolve <- function(x){
        im <<- solve(x)
    }
    ## by adding getsolve() to command in row 7 we can read cached inverse matrix
    getsolve <- function() im
    ## list makes list() object with functions stored by it's names for access from other environment
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## Function read im object from makeCacheMatrix environment
## IF im != NULL return messaget "getting cached data" and cached matrix 
## If im == NULL calculate and return inverse matrix
## We can call functions from another environment with ... in function(x, ...)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    ## read im from result of getsolve() function of makeCacheMatrix environment
    im <- x$getsolve()
    ## if im has data - return message and stored matrix
    if(!is.null(im)) {
        message("getting cached data")
        return(im)
    }
    ## if im == 0 - takes new matrix, calculate inverse for the matrix
    ## set result to solve() in makeCacheMatrix environment
    ## set im in makeCacheMatrix environment to new result
    m_data <- x$get()
    im <- solve(m_data, ...)
    x$setsolve(im)
    ## return im from makeCacheMatrix environment
    im
}
