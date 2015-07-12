## read the R script
## create a *square* matrix (because `solve` only handles square matrices)
## create the matrix during the call of makeCacheMatrix()
## example : a<-makeCacheMatrix()
##           a$set( matrix(c(1,2,12,13), nrow = 2, ncol = 2) );
##           a$get();
## after doing this you will get your matrix

makeCacheMatrix <- function(x = matrix()) {
                m<-NULL
                set<-function(y){
                                x<<-y
                                m<<-NULL
                                 }
                get<-function() x
                setmatrix<-function(solve) m<<- solve
                getmatrix<-function() m
                list(set=set, get=get,
                setmatrix=setmatrix,
                getmatrix=getmatrix)

}


## now to do inverse of your matrix we will use the below function
## after you have created your matrix as shown above do the following 
## cacheSolve(a)
## after calling this function you will get inverse of your matrix
## now again call the same function to see weather we are getting
## the cached matrix or not.
## when you will call the fucntion again and again it will display
## getting cached data 
cacheSolve <- function(x, ...) {
                m<-x$getmatrix()
                if(!is.null(m)){
                                message("getting cached data")
                                return(m)
                                }
                matrix<-x$get()
                m<-solve(matrix, ...)
                x$setmatrix(m)
                m
        ## Return a matrix that is the inverse of 'x'
}
