library(VennDiagram)
library(eulerr)
library(UpSetR)
library(gplots)
x <- list(a = c("a",'b','c','d','e'), b = c('c','d','z'),c = c('c','z','j','k'),d = c('c','z','j','k','d','e'))
e <- (euler(x))
e
plot(euler(x),quantities = T, legend = list(side = 'left'))


ol <- calculate.overlap(x)


upset(fromList(x))
# overLapper
#systemPipeR  extension of the Venn diagram resources on this site: http://manuals.bioinformatics.ucr.edu/home/R_BioCondManual#TOC-Venn-Diagrams

vv <- venn(x,simplify = TRUE,col = "red")
vv
aa <- attr(venn(x),'intersections')
df <- map_dfr(names(aa),function(name){
  data_frame(Group = name,Genes = paste0(aa[[name]],collapse = ','))
})