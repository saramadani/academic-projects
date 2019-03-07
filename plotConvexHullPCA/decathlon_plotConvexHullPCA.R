#' Plot convex hull when performing PCA
#'
#' @export plotConvexHullPCA
#'
#' @return Individual factorial map and Biplot from a PCA
#'
#' @description This function produces the  plot of the individual factorial map and the biplot from a PCA object obtained after perfoming a Principal Components Analysis with FactoMineR package
#' @import ggplot2
#' @import plyr
#' @import ggrepel
#' @importFrom plyr ddply
#' @importFrom FactoMineR PCA
#' @param  pc
#'   A FactomineR object obtained after using PCA function
#' @param  axes
#'   It's a vector of length 2 containing the rank of the components that we want to plot
#' @param  zfactor
#'   It's a vector from the original data, it' the sumplementary factor variable used in the PCA
#' @param  convex
#'   It's a boolean function indicates where or not we plot the Hull Convex
#' @param  varnames
#'   It's a vector containing the names of the active variables
#' @param  indnames
#'   It's a vector containing the names of the individuals, i.e; the rownames
#' @param  biplot
#'   It's a boolean vector indicates whether or not we make a biplot or a simple individual plot
#' @param  varlab
#'   It's vector with values c("text","point","arrow")
#' @param  indlab
#'    It's vector with values c("text","point"
#' @param  alpha
#'    A numeric parameter with values from 0 to 1 and it gives the transparency of the plot
#' @examples
data(decathlon)
require(FactoMineR)
pc<-PCA(decathlon,scale.unit = T,quanti.sup = 11:12,quali.sup = 13,graph = F)
plotConvexHullPCA(pc = pc,axes = c(1,2),zfactor = decathlon$Competition,convex = T,varnames = colnames(decathlon)[1:10],indnames = rownames(decathlon),varlab = c("text","arrow"),indlab = c("text"))


plotConvexHullPCA=function(pc,axes,zfactor,convex=T,
                           varnames,indnames,biplot=T,varlab,indlab,
                           alpha=.4){
  ## This R code uses ggplot2,  plyr, ggrepel
  ## The arguments of this R function are
  ## pc: A FactomineR object obtained after using PCA function
  ## axes:  A vector of length 2 containing the rank of the compenents that we want to plot
  ## zfactor:  A vector from the original data, it' the sumplementary factor variable used in the PCA
  ## convex:  A boolean function indicates where or not we plot the Hull Convex
  ## varnames:  A vector containing the names of the active variables
  ## indnames: A vector containing the names of the individuals, i.e; the rownames
  ## bilplot: A a boolean vector indicates whether or not we make a biplot or a simple individual plot
  ## varlab: A vector with values c("text","point","arrow")
  ## indlab: A vector with values c("text","point")
  
  xlab=paste("Dim ",axes[1],"(",round(pc$eig[axes[1],2],2),"%)",sep="")
  ylab=paste("Dim ",axes[2],"(",round(pc$eig[axes[2],2],2),"%)",sep="")
  
  if(convex==T){
    
    if(biplot==F){
      mydt=cbind.data.frame(x=pc$ind$coord[,axes[1]],y=pc$ind$coord[,axes[2]],zfac=zfactor,ind_lab=indnames)
      find_hull <- function(X) X[chull(X$x, X$y), ]
      hulls <- ddply(mydt, "zfac", find_hull)
      
      p<-ggplot(data = mydt,aes(x=x,y=y,col=zfac))+
        geom_hline(yintercept = 0,alpha=.4)+geom_vline(xintercept = 0,alpha=.4)
      if("text"%in%indlab){
        p<-p+geom_text_repel(data=mydt,aes(col=zfac,label=ind_lab))
      }
      if("point"%in%indlab){
        p<-p+geom_point(data=mydt,aes(col=zfac),alpha=alpha)
      }
      p<-p+xlab(xlab)+ylab(ylab)+
        geom_polygon(data=hulls,alpha=.2,aes(x=x,y=y,fill=zfac))+
        labs(col="",fill="")
      p<-p+theme_bw()
    }
    
    if(biplot==T){
      dt_var=cbind.data.frame(x=pc$var$coord[,axes[1]],y=pc$var$coord[,axes[2]],var_lab=varnames)
      
      dt_var$x=dt_var$x/sqrt(pc$eig[axes[1],1])
      dt_var$y=dt_var$y/sqrt(pc$eig[axes[2],1])
      
      mydt=cbind.data.frame(x=pc$ind$coord[,axes[1]],y=pc$ind$coord[,axes[2]],zfac=zfactor,ind_lab=indnames)
      
      
      mydt$x=mydt$x/sqrt(nrow(mydt)*pc$eig[axes[1],1])
      mydt$y=mydt$y/sqrt(nrow(mydt)*pc$eig[axes[2],1])
      
      find_hull <- function(X) X[chull(X$x, X$y), ]
      hulls <- ddply(mydt, "zfac", find_hull)
      
      d=nrow(dt_var)
      arrows = data.frame(x1 = rep(0, d), y1 = rep(0, d), x2 =dt_var$x,
                          y2 = dt_var$y)
      
      p<-ggplot()+
        geom_hline(yintercept = 0,alpha=.4)+geom_vline(xintercept = 0,alpha=.4)
      if("nothing"%in%indlab){
        p<-p
      }
      else{
        if("point"%in%indlab){
          p<-p+geom_point(data = mydt,aes(x=x,y=y,col=zfac),alpha=alpha)
        }
        if("text"%in%indlab){
          p<-p+geom_text_repel(data=mydt,aes(x=x,y=y,col=zfac,label=ind_lab),alpha=alpha)
        }
      }
      
      if("text"%in%varlab){
        p<-p+geom_text_repel(data = dt_var,aes(x=x,y=y,label=var_lab))
      }
      if("point"%in%varlab){
        p<-p+geom_point(data = dt_var,aes(x=x,y=y))
      }
      if("arrow"%in%varlab){
        p<-p+geom_segment(data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2), colour ="gray65")
      }
      p<-p+xlab(xlab)+ylab(ylab)+
        geom_polygon(data=hulls,alpha=.2,aes(x=x,y=y,fill=zfac))+
        labs(col="",fill="")
      p<-p+theme_bw()
    }
    
  }
  if(convex==F){
    
    if(biplot==F){
      mydt=cbind.data.frame(x=pc$ind$coord[,axes[1]],y=pc$ind$coord[,axes[2]],ind_lab=indnames)
      p<-ggplot(data = mydt,aes(x=x,y=y))+
        geom_hline(yintercept = 0,alpha=.4)+geom_vline(xintercept = 0,alpha=.4)
      if("nothing"%in%indlab){
        p<-p
      }
      else{
        if("text"%in%indlab){
          p<-p+geom_text_repel(data=mydt,aes(x=x,y=y,label=ind_lab),alpha=alpha)
        }
        if("point"%in%indlab){
          p<-p+geom_point(data=mydt,aes(x=x,y=y),alpha=alpha)
        }
      }
      
      p<-p+xlab(xlab)+ylab(ylab)
      p<-p+theme_bw()
    }
    
    if(biplot==T){
      dt_var=cbind.data.frame(x=pc$var$coord[,axes[1]],y=pc$var$coord[,axes[2]],var_lab=varnames)
      
      dt_var$x=dt_var$x/sqrt(pc$eig[axes[1],1])
      dt_var$y=dt_var$y/sqrt(pc$eig[axes[2],1])
      
      mydt=cbind.data.frame(x=pc$ind$coord[,axes[1]],y=pc$ind$coord[,axes[2]],ind_lab=indnames)
      
      mydt$x=mydt$x/sqrt(nrow(mydt)*pc$eig[axes[1],1])
      mydt$y=mydt$y/sqrt(nrow(mydt)*pc$eig[axes[2],1])
      
      
      d=nrow(dt_var)
      arrows = data.frame(x1 = rep(0, d), y1 = rep(0, d), x2 =dt_var$x,
                          y2 = dt_var$y)
      
      p<-ggplot()+
        geom_hline(yintercept = 0,alpha=.4)+geom_vline(xintercept = 0,alpha=.4)
      if("nothing"%in%indlab){
        p<-p
      }
      else{
        #data = mydt,aes(x=x,y=y)
        if("text"%in%indlab){
          p<-p+geom_text_repel(data=mydt,aes(x=x,y=y,label=ind_lab),alpha=alpha)
        }
        if("point"%in%indlab){
          p<-p+geom_point(data=mydt,aes(x=x,y=y),alpha=alpha)
        }
      }
      if("text"%in%varlab){
        p<-p+geom_text_repel(data = dt_var,aes(x=x,y=y,label=var_lab),colour="red")
      }
      if("point"%in%varlab){
        p<-p+geom_point(data = dt_var,aes(x=x,y=y),colour="red")
      }
      if("arrow"%in%varlab){
        p<-p+geom_segment(data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2), colour ="red")
      }
      p<-p+xlab(xlab)+ylab(ylab)
      p<-p+theme_bw()
    }
    
  }
  return(p)
}
data(decathlon)
library(plyr)
require(ggrepel)
pc<-PCA(decathlon,scale.unit = T,quanti.sup = 11:12,quali.sup = 13,graph = F)
plotConvexHullPCA(pc = pc,axes = c(1,2),zfactor = decathlon$Competition,convex = T,
                  varnames = colnames(decathlon)[1:10],indnames = rownames(decathlon),
                  varlab = c("text","arrow"),indlab = c("text"))
