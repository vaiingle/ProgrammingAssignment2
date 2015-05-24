makeCacheMatrix <- function(m=matrix()) {

## Creates a list of functions that

## can cache the inverse of a matrix.

   inmatrix <- NULL

    set <- function(outmatrix) {

        inmatrix <<- outmatrix

        inmatrix <<- NULL

    }

    get <- function() m

    sInvert <- function(invert) inmatrix <<-invert

    gInvert <- function() inmatrix

    list(set = set, get = get,

         sInvert = sInvert,

         gInvert = gInvert)


}




cacheSolve <- function(m, ...) {

## Computes the inverse of the matrix returned

## by makeCacheMatrix(), unless the inverse has

## already been calculated, in which case

## it retrieves it from the cache.

    inmatrix <- m$gInvert()

    if ( ! is.null(inmatrix)) {

        print("getting cached data")

        return(inmatrix)

    }
##Assume the matrix supplied is always invertible and is a square matrix
    inmatrix <- solve(m$get())

    m$sInvert(inmatrix)

    inmatrix

}


