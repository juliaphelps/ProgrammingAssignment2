## Reference:
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

## test

y <- c(6, 3, 9, 25, 4)
x <- makeVector(y)
x
$set
function (y) 
{
    x <<- y
    m <<- NULL
}
<environment: 0x000000002c9d36c0>
    
    $get
function () 
    x
<environment: 0x000000002c9d36c0>
    
    $setmean
function (mean) 
    m <<- mean
    <environment: 0x000000002c9d36c0>
        
        $getmean
    function () 
        m
    <environment: 0x000000002c9d36c0>
    
cachemean(x)
[1] 9.4

## test matrix

testMatrix <- matrix(1:4, 2, 2)
testMatrix
[,1] [,2]
[1,]    1    3
[2,]    2    4

testMatrix2 <- solve(testMatrix)
testMatrix2
[,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5

> testMatrix5 <- matrix(c(1,0,5,2,1,6,3,4,0), 3, 3)
> testMatrix5
[,1] [,2] [,3]
[1,]    1    2    3
[2,]    0    1    4
[3,]    5    6    0

> solve(testMatrix5)
[,1] [,2] [,3]
[1,]  -24   18    5
[2,]   20  -15   -4
[3,]   -5    4    1