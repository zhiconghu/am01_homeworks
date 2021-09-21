#Now a matrix plot with the dependent; but want to control each type of plot
#control density in the diagonal by creating a new function
if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
}
if(!require(GGally)){
  install.packages("GGally")
  library(GGally)
}
if(!require(ggmosaic)){
  install.packages("ggmosaic")
  library(ggmosaic)
}

modified_density = function(data, mapping, ...) {
  ggally_densityDiag(data, mapping, ...) + scale_fill_manual(values = c("red", "blue"))+
  theme(text = element_text(size=8))
  }
#control scatter in the lower triangle  by creating a new function
mod_points=function(data,mapping,...) {
  ggally_points(data, mapping,pch=20, ...) + scale_colour_manual(values = c("red", "blue"))+
    theme(text = element_text(size=8))
}  
#control histogram in the lower triangle for each level of good by creating a new function
mod_bihist=function(data,mapping,...) {
  ggally_facethist(data, mapping, ...) + scale_fill_manual(values = c("red", "blue")) +
theme(text = element_text(size=7))  +
    scale_x_continuous(breaks=c(floor(min(data[,mapping_string(mapping$x)])),round((min(data[,mapping_string(mapping$x)])+max(data[,mapping_string(mapping$x)]))/2),ceiling(max(data[,mapping_string(mapping$x)])))) +
    scale_y_continuous(breaks=c(0,20,50,60)) 
}
#control box plot
mod_box=function(data,mapping,...) {
  
  ggally_box_no_facet(data, mapping, ...) + scale_fill_manual(values = c("red", "blue"))
}
mod_bar=function(data,mapping,...) {
  #control bar chart in diagonal  
  ggally_barDiag(data, mapping, ...) + scale_fill_manual(values = c("red", "blue"))
}
#control correlation
mod_cor=function(data,mapping,...) {
  ggally_cor(data, mapping,size=3,alignPercent=0.4) + scale_colour_manual(values = c("red", "blue")) + theme(panel.grid.major = element_blank(),...)
  
}

my_fn <- function(data, mapping, ...){ 
mapping$fill <- mapping$y
ya <- data[,mapping_string(mapping$y)]
xa <- data[,mapping_string(mapping$x)]
print(mapping)
mapping$y <- NULL
print(mapping)
#xx <- product(xa,ya)
#mapping$x <- xx
#print(xx)
print(mapping)
  ggplot(data = data,mapping) +
    geom_mosaic(mapping) 
}
   
#Then plot ggpairs(df, lower=list(continuous=my_fn))




