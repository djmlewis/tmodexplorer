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
  tibble(Group = name,Genes = paste0(aa[[name]],collapse = ','))
})

venn.diagram(x,NULL)
file.remove(list.files(pattern = "^VennDiagram.*log$"))



Intersect <- function (x) {  
  # Multiple set version of intersect
  # x is a list
  if (length(x) == 1) {
    unlist(x)
  } else if (length(x) == 2) {
    intersect(x[[1]], x[[2]])
  } else if (length(x) > 2){
    intersect(x[[1]], Intersect(x[-1]))
  }
}

Union <- function (x) {  
  # Multiple set version of union
  # x is a list
  if (length(x) == 1) {
    unlist(x)
  } else if (length(x) == 2) {
    union(x[[1]], x[[2]])
  } else if (length(x) > 2) {
    union(x[[1]], Union(x[-1]))
  }
}

Setdiff <- function (x, y) {
  # Remove the union of the y's from the common x's. 
  # x and y are lists of characters.
  xx <- Intersect(x)
  yy <- Union(y)
  setdiff(xx, yy)
}


combs <-  unlist(lapply(1:length(x), function(j) combn(names(x), j, simplify = FALSE)), recursive = FALSE)
names(combs) <- sapply(combs, function(i) paste0(i, collapse = " : "))
str(combs)
#List of 15
# $ A   : chr "A"
# $ B   : chr "B"
# $ C   : chr "C"
# $ D   : chr "D"
# $ AB  : chr [1:2] "A" "B"
# $ AC  : chr [1:2] "A" "C"
# $ AD  : chr [1:2] "A" "D"
# $ BC  : chr [1:2] "B" "C"
# $ BD  : chr [1:2] "B" "D"
# $ CD  : chr [1:2] "C" "D"
# $ ABC : chr [1:3] "A" "B" "C"
# $ ABD : chr [1:3] "A" "B" "D"
# $ ACD : chr [1:3] "A" "C" "D"
# $ BCD : chr [1:3] "B" "C" "D"
# $ ABCD: chr [1:4] "A" "B" "C" "D"

# "A" means "everything in A minus all others"
# "A", "B" means "everything in "A" and "B" minus all others" and so on
elements <-  lapply(combs, function(i) Setdiff(x[i], x[setdiff(names(x), i)]))

n.elements <- sapply(elements, length)
print(n.elements)














