#' Homework 4  - Question 3   - Reference to the code structure of setting classes by Justina Xie
#' 
#' @slot dims the dimensions of the matrix
#' @name sparse.matrix
#' @param x value of the matrix
#' @return a the matrix of the operation: addition, multiplication and transpose
#' @rdname sparse.matrix-methods
#' @importFrom methods 
#' @export sparse.matrix
#' @docType methods
#' 

setClass("sparse.matrix", contains = "data.frame", slots = c(dims = "vector"))
sparse.matrix<-function(i,j,x,dims=NULL){
  if(is.null(dims)){
    dims<-c(max(i),max(j))
  }
  
  df<-data.frame(i = i, j = j , x = x)
  df<-df[order(df$i),]
  row.names(df)=NULL
  
  Z<-new("sparse.matrix",df,dims = dims)
  return(Z)
}

#' Addition
#'
#' @docType methods
#' @rdname sparse.matrix-methods
#' @aliases +, sparse.matrix
#' @usage {+}{sparse.matrix,sparse.matrix}(e1,e2)
#' @inheritParams e1,e2 matrix addtion
#' 
setMethod("+",signature(e1= "sparse.matrix", e2 = "sparse.matrix"), function(e1, e2){
  if ((attributes(e1)$dims[1] == attributes(e2)$dims[1]) &&
      (attributes(e1)$dims[2] == attributes(e2)$dims[2])) {
    a <- data.frame(i = e1$i, j = e1$j, x = e1$x)
    b <- data.frame(i = e2$i, j = e2$j, x = e2$x)
    c <- merge(a, b, by = c("i", "j"), all = TRUE, suffixes = c("1", "2"))
    c$x1[is.na(c$x1)] <- 0
    c$x2[is.na(c$x2)] <- 0
    c$x <- c$x1 + c$x2
    c<-c[, c("i", "j", "x")]
    Z <- sparse.matrix(c$i,c$j,c$x,dims = e1@dims)
    return(Z)
  }else
    stop("the dimensions of two matrix do not match, try another one")
}
)



#' Transpose
#'
#' @param x \code{sparse.matrix} object
#' @docType methods
#' @rdname sparse.matrix-methods
#' @aliases t, sparse.matrix,ANY-method
#' @usage {t}{sparse.matrix}(x)
#' @inheritParams x from the transpose of matrix
#'
setMethod("t",signature="sparse.matrix",definition=function(x){
  dims<-c(x@dims[2],x@dims[1])
  tt<-x$i
  x$i<-x$j
  x$j<-tt
  x<-x[order(x$i),]
  
  row.names(x)=NULL
  Z <- sparse.matrix(x$i,x$j,x$x,dims)
  return(Z)
  }
)


#' Multiplication  - I completely dont understand this part, and this code is refernce to the Justina Xie, 
#' A small modification has been added on to the code. 
#'
#' @docType methods
#' @rdname sparse.matrix-methods
#' @aliases %*%, sparse.matrix,sparse.matrix-method
#' @usage {%*%}{sparse.matrix}(x,y)
#' @inheritParams x,y matrix multiplication
#' 

setMethod("%*%", signature(x= "sparse.matrix", y = "sparse.matrix"),function(x, y){
            if (attributes(x)$dims[2] == attributes(y)$dims[1]) {
              dims = c(attributes(x)$dims[1], attributes(y)$dims[2])
              a <- data.frame(i = x$i, j = x$j, x = x$x)
              ax<-numeric(a[nrow(a),1]+1)
              ax[1]<-1
              for(ra in 2:nrow(a)){
                if(a[ra-1,1]!=a[ra,1]){
                  ax[a[ra,1]]<- ra
                }
              }
              ax[length(ax)] <- nrow(a)+1
              y<-t(y)
              b<-data.frame(i = y$i, j = y$j, x = y$x)
              bx<-numeric(b[nrow(b),1]+1)
              bx[1]<-1
              for(rb in 2:nrow(b)){
                if(b[rb-1,1]!=b[rb,1]){
                  bx[b[rb,1]]<- rb
                }
              }
              bx[length(bx)] <- nrow(b)+1
              df=data.frame()
              for(i in 1:(length(ax)-1)){
                tta=a[ax[i]:(ax[i+1]-1),]
                for(j in 1:(length(bx)-1)){
                  ttb=b[bx[j]:(bx[j+1]-1),]
                  c <- merge(subset(tta,select=c("j","x")), subset(ttb,select=c("j","x")), by = c("j"), all = TRUE, suffixes = c("1", "2"))
                  c$x1[is.na(c$x1)] <- 0
                  c$x2[is.na(c$x2)] <- 0
                  row_sum <-  sum(c$x1 * c$x2)
                  if (row_sum != 0)
                  {
                    df <- rbind(df, data.frame(i=ax[i],j=bx[j],x=row_sum))
                  }
                }
              }
              row.names(df)=NULL
              Z <-sparse.matrix(df$i,df$j,df$x,dims)
              return(Z)
          }else 
           stop("the dimensions of two matrix do not match, try another one")
        }
)
