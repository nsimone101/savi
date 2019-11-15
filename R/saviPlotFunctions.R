#' Plot an igraph with colored vertex groups
#'
#' The default plot of an igraph colors each vertex the same.  Since
#' we are trying to see the "groups" the vertices are in, we implement
#' our own plot function.  Note, that the colors are set if the number
#' of vertices is less than or equal to 6.  For larger graphs, we choose
#' random colors from the color palette.  If there are more groups than
#' colors in r, some colors may repeat.  Note if 0 is passed as the number
#' of vertex groups, all vertices will be white.
#'
#' @param g The igraph to graph.
#' @param numOfVertexGroups The desired number of vertex groups in the
#'     graph.  If this number is less than 0 or not an integer, an
#'     error will be thrown.
#' @return A plot of the igraph with vertex groups in different colors.
#' @examples
#'     plotWithGroups(g)
#' @export
#'
plotWithGroups<-function(g,numOfVertexGroups){

  if(numOfVertexGroups<0){
    stop('plotWithGroups: The number of vertex groups must be an integer greater than or equal to 0.')
  }

  if(numOfVertexGroups%%1!=0){
    stop('plotWithGroups: The number of vertex groups must be an integer greater than or equal to 0.')
  }


  if(numOfVertexGroups==1){
    colorArray=c("lightblue")
  }else if(numOfVertexGroups==2){
    colorArray=c("lightblue","lightyellow")
  }else if(numOfVertexGroups==3){
    colorArray=c("lightblue","lightyellow","tomato")
  }else if(numOfVertexGroups==4){
    colorArray=c("lightblue","lightyellow","tomato","lightgreen")
  }else if(numOfVertexGroups==5){
    colorArray=c("lightblue","lightyellow","tomato","lightgreen","hotpink2")
  }else if(numOfVertexGroups==6){
    colorArray=c("lightblue","lightyellow","tomato","lightgreen","hotpink2","gray")
  }else if(numOfVertexGroups<=length(colors())){
    colorArray<-colors()
    colorArray<-colorArray[sample(1:length(colorArray),numOfVertexGroups)]
  }else{
    colorArray<-colors()
    colorArray<-colorArray[sample(1:length(colorArray),numOfVertexGroups,replace=TRUE)]
  }

  plot(g, vertex.label.color="black", vertex.size=20,
       vertex.color=colorArray[V(g)$group])
}
