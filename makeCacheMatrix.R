
makeCacheMatrix <- function (mm = matrix())
{
  
  ############# Defining Vector 
  
 ### newmat <<- NULL
  
  setmatrix <- function(y = matrix())
  {
    
    mm  <<-  y
    
    newmat <<- NULL
    
    setadjacent(mm)
    
    
  }
  
  getmatrix <- function() mm
  
  
  setadjacent <- function(mm)
    
  {
    ########### Column Bind from 1st and 2nd (original Matrix ) c
    
    mm <- cbind(mm,mm[,1],mm[,2])
    
    ########### Row Bind from 1st and 2nd (above matrix (which is not original matrix ) c
    
    mm <- rbind(mm,mm[1,],mm[2,])
    
    ############## dummy vector which will be used for creating Adjacent matrix
    
    dummy <- c()
    
    ############ 
    
    rowlevel <- nrow(mm) -1
    collevel <- ncol(mm)- 1
    
    ################ Calculating for Adjacent matrix ##################
    
    for (cl in c(2:4) )
    {
      
      for (r in c(2:4))
      {
        ## ([rn,cn]*[rn+1,cn+1]-([rn,cn+1])*[rn+1,cn]
        dummy[length(dummy)+1] <- ( ( mm[r,cl] * mm[r+1,cl+1])) - ( mm[r,cl+1] *mm[r+1,cl] )
        
      }
    }
    
    
    ################# Moving dummy vector values to matrix#####################
    
    newmat <<- matrix(dummy,3,3,byrow=T)
    
    
  }
  
  getadjacent <- function() newmat
  
  list(getadjacent=getadjacent,setadjacent=setadjacent,getmatrix=getmatrix,setmatrix=setmatrix)
  
}


cacheSolve <- function(x,...)
{
  
  xxx <- c(0,1,2,1,2,3,3,1,9) 
  
  i_mat <- matrix(xxx,3,3 ,byrow = T)
  
  ################# Input Matrix 
  
  print(i_mat)
  
  bc <- x$getmatrix() 
  
  print(bc) 
  
  print(all.equal( bc ,i_mat))
  
  if ( isTRUE(all.equal ( bc ,i_mat)) == TRUE && !is.null(x$getadjacent()) )
  {
    print("Matrix already cached")
    newmat <- x$getadjacent()
    print(newmat)
    print("see")
    
  }
  
  else  
  {
    print("Fresh Run")
    newmat <- x$setadjacent(i_mat)
    ## mm <- x$getadjacent()
    
    
  } 
  
  
  
  #################  Finding determinant #########################
  
  dett <-  det(matrix(i_mat,3,3,byrow = T))
  
  ###############  Final Inverse value #########################
  
  print (paste("1/",dett)) ; print (newmat)
  
  
  
}





