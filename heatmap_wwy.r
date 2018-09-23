
####	load custom function and libraries			 
	source("./heatmap_function.r")

####	read.data
	table2<- read.table("P1.csv", na.strings="NA", header=TRUE, sep=",", dec=".", row.names=1)
	x.h <-as.matrix(table2)
	#for PCA
	rel_all <- t(x.h)
	rel_all.pca<-PCA(rel_all)
	print(rel_all.pca)
	#Results for the Principal Component Analysis (PCA)**
	  #The analysis was performed on 51 individuals, described by 642 variables
	#The results are available in the following objects:
	  
	 # name               description                          
	#1  "$eig"             "eigenvalues"                        
	#2  "$var"             "results for the variables"          
	#3  "$var$coord"       "coord. for the variables"           
	#4  "$var$cor"         "correlations variables - dimensions"
	#5  "$var$cos2"        "cos2 for the variables"             
	#6  "$var$contrib"     "contributions of the variables"     
	#7  "$ind"             "results for the individuals"        
	#8  "$ind$coord"       "coord. for the individuals"         
	#9  "$ind$cos2"        "cos2 for the individuals"           
	#10 "$ind$contrib"     "contributions of the individuals"   
	#11 "$call"            "summary statistics"                 
	#12 "$call$centre"     "mean of the variables"              
	#13 "$call$ecart.type" "standard error of the variables"    
	#14 "$call$row.w"      "weights for the individuals"        
	#15 "$call$col.w"      "weights for the variables"          
	write.table(rel_all.pca$var$cos2, sep="\t",file="~/Desktop/P4_all.pca.var.cos2.txt")
	#continue for read data
	x <- make.transformation(x=x.h,upper=100,lower=0.1,make.log="y")

######################
graphics.off()
	svg(filename="ami_heatmap.svg", width=70, height=50, pointsize=12)
  annotation_col = data.frame(Diet = factor(rep(c("Soil", "Humus"), c(1,2))))                     
	ann_colors = list(Diet = c(Soil="#CDC0B0", Humus="#D2691D"))                            
	rownames(annotation_col) = c("Lab288_ms", "Emb289_ms", "Ter290_ms")                            
	pheatmap(x.h,fontsize = 8,fontsize_row = 12, cellwidth = 5, cellheight = 9, cutree_cols = 2, cutree_rows = 6, annotation_col = annotation_col, annotation_colors = ann_colors, cluster_rows = FALSE, gaps_row = c(8,10,15,16,18))                            
	pheatmap(x.h,fontsize = 8,fontsize_row = 12, cellwidth = 5, cellheight = 9, cutree_cols = 2, cutree_rows = 6, annotation_col = annotation_col, annotation_colors = ann_colors)
dev.off()	
	
graphics.off()
#svg(file=phylum.svg) #only if you need it
colors <- colorRampPalette(c("gray98", "black" , "red"), space="rgb")(149);
svg(filename="C_heatmap.svg", width=7, height=5, pointsize=12)
  #heatmap.2(x, Colv=F, scale="none", trace="none", margin=c(10,10), dendrogram=NULL, col=colors, Rowv=NULL, cexRow=0.2)
dev.off()
  
my.breaks <- c(seq(-3.2, -1, length.out=20),seq(-1, 0.2, length.out=65),seq(0.2, 2, length.out=65))
colors2<-colorRampPalette(c("gray98", "gray70", "black", "red"), space="rgb")(165)
my.breaks2 <- c(seq(-3, -2, length.out=35),seq(-2, -1, length.out=30),seq(-1, 0, length.out=36), seq(0, 2, length.out=65))

#my.breaks <- c(seq(-2.1, -1, length.out=20),seq(-1, 0.9, length.out=65),seq(0.9, 1.6, length.out=65)) - fiber fractions
truehist(x, 2)
