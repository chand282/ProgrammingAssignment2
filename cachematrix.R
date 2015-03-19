## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix`: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

	invse <- NULL
            set <- function(y) {
                    x <<- y
                    invse <<- NULL
            }
            get <- function() x
            setinverse <- function(inverse) invse <<- inverse
            getinverse <- function() invse
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)

}


## Write a short comment describing this function
## cacheSolve`: This function computes the inverse of the special"matrix" returned by `makeCacheMatrix` above. If the inverse ## has already been calculated (and the matrix has not changed), then
##  cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	 invse <- x$getinverse()
            if(!is.null(invse)) {
                    message("getting cached data")
                    return(invse)
            }
            data <- x$get()
            invse <- solve(data, ...)
            x$setinverse(invse)
            invse
}

## By coolchandra@gmail.com
## How to test them:
## a<- 1:2
## b<- 3:4
## x<- rbind(a,b)
## > x
##    [,1] [,2]
## a    1    2
## b    3    4


## Run CacheMatrix
## > mCM = makeCacheMatrix(x)
## > mCM$get()
##  	[,1] [,2]
## a    1    2
## b    3    4


## First run - No Cache
## > cacheSolve(mCM)
##       a    b
## [1,] -2.0  1.0
## [2,]  1.5 -0.5

## Second run -Using Cache
## > cacheSolve(mCM)
## getting cached data.
##           a    b
##    [1,] -2.0  1.0
##    [2,]  1.5 -0.5

