## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## I hate the really short names given in the "Vector" example
## In the process of making the names longer and more informative to me
## I discovered that the "Cache" routine is really just doing 4 things:
## "saving the raw data" (set_raw_data)  (set in the class example)
## "retrieving the raw data" (get_raw_data) (get in the class example)
## "saving the calculated result" (set_calculcated_data)  (setmean in the class example)
## "retrieving the calculated result" (get_calculated_data) (getmean in the class example)

## this function really doesn't return a matrix.  It returns a LIST
## that has 4 elements which are actually more functions within the "cache"
makeCacheMatrix <- function(x = matrix()) {
        saved_calculated_data <- NULL
        set_raw_data <- function(y) {
                x <<- y
                saved_calculated_data <<- NULL
        }
        get_raw_data <- function() {
        	x
        }
        set_calculated_data <- function(calculated_data) {
        	saved_calculated_data <<- calculated_data
        }
        get_calculated_data <- function() {
        	saved_calculated_data
        }
        list(get_raw = get_raw_data,
        	 set_raw = set_raw_data,
             set_cache = set_calculated_data,
             get_cache = get_calculated_data)
}


## Write a short comment describing this function

## this function is REALLY given the cache vector and NOT a matrix 'x'
## the matrix is INSIDE the cache vector
## so having said that, this function does the following:
## retrieves the cached value.
## if it has already been calculated then it returns that value
## otherwise, it gets the raw data, calculates the inverse,
## saves the inverse in the cache vector
## returns the newly calculated inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        already_saved_inverse <- x$get_cache()
        if(!is.null(already_saved_inverse)) {
                return(already_saved_inverse)
        }
        raw_matrix <- x$get_raw()
        inverted_matrix <- solve(raw_matrix, ...)
        x$set_cache(inverted_matrix)
        inverted_matrix
}

## n<-makeCacheMatrix(matrix(4:7, 2, 2))
## cacheSolve(n)
## cacheSolve(n)
