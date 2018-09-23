library(heatmap.plus)
library(colorRamps)
library(gplots)

make.transformation <- function(x,upper,lower,make.log){
							x.temp <- x 
							x.temp[which(x.temp>upper)] <- upper
							x.temp[which(x.temp<lower)] <- 0

							x.temp[x.temp==0] <- NA

							if(make.log=="y"){
										y <- log10(x.temp)
									  }else{
										 y <- x.temp
										 }
			 				return(y)
							}






