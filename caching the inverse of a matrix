#create a matrix that can 1 set the value of the matrix 2 get the value of the matrix 3 set the inverse of the matrix 4 get the inverse of the matrix

makeCacheMatrix <- function(x){
	m <- NULL
	set <- function(y){
		x <<- y
		m <<- NULL
	}
	get <- function(){x} 
	setinverse<-function(inverse) {m <<- inverse}
	getinverse<-function(){m} 
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 
}

#calculte the inverse of the matrix
#if the inverse has already been calculated then retrieve the inverse from cache

cacheSolve <- function(x, ...){
	m <- x$getinverse()
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}
