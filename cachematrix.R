#makeCacheMatrix() is a function that expects a matrix as its sole argument
#It might make sense to include some validation checks because for cacheSolve to work
#this argument needs to be an invertible matrix.  We could simply test that the 
#determinant of the matrix is non-zero or if we want something even more basic
#we could have a simple test that the matrix is square.  As we are told in the
#assignment instructions that we may assume the supplied matrix is always
#invertible we do not do any validation in this code. 

#makeCacheMatrix() receives an invertible matrix as an input, and produces four FUNCTIONS.
#The function set() is a way of altering our matrix without having to run all of makeCacheMatrix again.
#The function get() is how we read the current value of the matrix of interest.
#The function setinverse() allows us to state what the inverse of our matrix is, without necessarily calculating it.
#The function getinverse() is how we read the current value of the inverse of our matrix of interest.

#cacheSolve expects as its input an output from makeCacheMatrix()
#It makes use of the variable inv that is used in the definition of getinverse()
#The use of superassignment ("<<-") in set() and setinverse() ensures that this inv is NULL if a new matrix of interest is set up
#Therefore, if inv is not NULL cacheSolve() just returns the cached inverse matrix from a previous call of cacheSolve()
#If inv is NULL then the new matrix is inverted instead and stored in the cache and output


##The function that creates the 4 function set(), get() setinverse() and getinverse() (given a matrix)

makeCacheMatrix<- function(x=matrix()){    			#We are assuming that a (non-singular and square) matrix will be given as input
    inv<-NULL										#Sets up the local variable inv
    set<-function(y){								#The function set() expects a single argument (which is meant to be another matrix)
        x<<-y										#Using the "<<-" operator means that the argument supplied for set() is assigned globally to x
        inv<<-NULL									#and inv is reset globally to NULL - important in the cacheSolve function below
    }
    get<-function()x								#This allows us to see what is currently assigned to x, our matrix of interest
    setinverse<-function(inverse) inv<<-inverse	    #This allows us to overwrite whatever is currently assigned as the inverse of our matrix. 
    getinverse<-function()inv						#NB using the "<<-" operator allows us to cache.  See comments below
    list (set=set, get=get,
          setinverse=setinverse, getinverse=getinverse)
}


## Return the current cached inverse matrix if the matrix of interest is unchanged, otherwise calculate the new matrix's inverse and put the result into the cache

cacheSolve<-function(x){    				    	#The expected input here is the output from makeCacheMatrix i.e. a list of four functions
    inv<-x$getinverse()							    #This is null straight after makeCacheMatrix is called or set() is called (if we didn't have the superassignment in setinverse() then inv would be NULL here instead of the inverted matrix)
    if(!is.null(inv)){                              #this will be non-null if cacheSolve() was run and no new matrix set by using makeCacheMatrix() or set()
        message("getting cached result")
        return(inv)
    }
    data<-x$get()                                   #if inv was NULL then there is a matrix, which gets inverted by solve() and stored using setinverse() then returned
    inv<-solve(data)
    x$setinverse(inv)
    inv
}