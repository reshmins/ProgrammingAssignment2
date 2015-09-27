makeCacheMatrix<-function(x=matrix())
{
  inv<-NULL
  set <- function(y)
  {
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  setInv<-function(inverse) inv<<-inverse
  getInv<-function() inv
  list(set=set,
       get=get,
       setInv=setInv,
       getInv=getInv)
  
}

cacheSolve<- function(x,...)
{
  inv<-x$getInv()
  if (!is.null(inv))
  {
    message("getting cached data")
    return(inv)
    
  }
  mat<-x$get()
  inv<-solve(mat)
  x$setInv(inv)
  inv
    }
