## Catching the Mean of a Vector

##  The first function, makeVector creates a special "vector", which is really a list containing a function to 
## 1.set the value of the vector
## 2.get the value of the vector
## 3.set the value of the mean
## 4.get the value of the mean

## The following was presented on the Discussion Forum by Fu Sheng Wang and it has been altered to answer this assignment:
  
## The example function "makeVector" is a function with the following properties
## 1. it takes an argument x of type numeric vector
## 2. it returns a list with 4 list items  (they are actually 4 functions wrapped in a list)
makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

## so you when you create a "special vector" using makeVector function, you will get a list
> a <- makeVector(c(1,2,3))
> class(a)
[1] "list"


## you may call any of the functions in the list:
  > class(a$get)
[1] "function"
>
  > a$get()
[1] 1 2 3
>
  > a$set(c(4,5,6))
> a$get()
[1] 4 5 6

## Caching the Inverse of a Matrix

## The function cacheMean is a client function that uses the makeVector function in its implementation.
## The input is expecting a "special vector" made from makeVector (ignore the ... for now).
## The output is the mean coming whether from the special vector's  cache or computation:
cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}
cachemean <- function(x, ...) {
        m <- x$getmean()           #query the x vector's cache         
        if(!is.null(m)) {           #if there is a cache
              message("getting cached data") 
              return(m)             #just return the cache, no computation needed
}
        data <- x$get()             #if there's no cache
        m <- mean(data, ...)        #we actually compute them here
        x$setmean(m)                #save the result back to x's cache
        m                           #return the result
}

usage:
  > b <- cachemean(a)
> b
[1] 5
>
  > b <- cachemean(a)
getting cached data
> b
[1] 5


## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

> a <- makeCacheMatrix(matrix(1:4,2))
>
  > a$get()
[,1] [,2]
[1,]    1    3
[2,]    2    4
>
  > a$getInverse()
NULL
>
  > a$set(matrix(5:8,2))
> a$get()
[,1] [,2]
[1,]    5    7
[2,]    6    8
> 
  
## The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

  > cacheSolve(a)
[,1] [,2]
[1,]   -4  3.5
[2,]    3 -2.5
> 
  > cacheSolve(a)
getting cached data
[,1] [,2]
[1,]   -4  3.5
[2,]    3 -2.5
> 
  > a$getInverse()
[,1] [,2]
[1,]   -4  3.5
[2,]    3 -2.5
>
  >
  > #test inverse correctness
  > b = a$getInverse()
> a$get() %*% b           #this matrix multiplication should show identity matrix
[,1]         [,2]
[1,]    1 3.552714e-15    #3.552714e-15 ~= 0 (due floating point rounding error)
[2,]    0 1.000000e+00    #1.000000e+00 = 1
> 
  
