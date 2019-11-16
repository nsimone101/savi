#' Adds the vertex attribute "group" to each vertex in an igraph
#' randomly.
#'
#' @param g The igraph to add vertex groups to.
#' @param numOfVertexGroups The desired number of vertex groups
#'     for the graph.  It is required that this number be greater
#'     than 1 or the function runSavi will throw an error.  It is
#'     recommended that the number be less than the number of
#'     verticies in the graph.
#'
#' @return The given igraph with each vertex in a random group.
#' @examples
#'     addRandomGroups(g,6)
#' @export
#'
addRandomGroups<-function(g,numOfVertexGroups){
  V(g)$group<-sample(1:numOfVertexGroups,length(V(g)),replace=TRUE)
  return(g)
}

#' Creates a random igraph based on user specifications.
#'
#' @param graphType The graph type desired.  There are two types of
#'     graphs currently available. The first is a random connected
#'     graph ("Connected").  The other is the EFL graph ("EFL"),
#'     which creates a random graph that conforms to the given
#'     conditions of the Erdős–Faber–Lovász conjecture. Note that
#'     this implementation will not create all possible EFL graphs.
#'     However, the graph created will always be an EFL graph.
#' @param graphParams When the graph type is "Connected," the next
#'     two arguements should be the number of verticies desired and
#'     the second arguement should be the probability of each vertex
#'     connecting.  Thus, this arguement should be in the interval
#'     (0,1).  When the graph type is EFL, n is the number of
#'     n-complete graphs to connect together.  A second argument
#'     is not needed.
#' @return The newly created igraph.
#' @examples
#'     createSaviGraph("Connected",c(6,0.8))
#'     createSaviGraph("EFL",4)
#' @export
#'
createSaviGraph<-function(graphType,graphParams){
  if(graphType=="EFL"){
    k<-graphParams[1]
    g<-rep(make_full_graph(k, directed = FALSE, loops = FALSE), k)
    arr<-c(1:k^2)

    for(i in (1:(k-1))){
      s1<-c((k*(i-1)+1):(k*i))
      s2<-c(((k*i)+1):(k*(i+1)))

      vr1<-sampleOne(s1)
      vr2<-sampleOne(s2)

      arr[vr2]<-arr[vr1]
    }

    g<-simplify(contract.vertices(g,arr))
    g<-delete.vertices(g,degree(g)==0)

    return(g)
  }

  #Create a random connected graph
  n<-as.numeric(graphParams[1])
  p<-as.numeric(graphParams[2])

  adjm <- matrix(sample(0:1, n^2, replace=TRUE, prob=c(1-p,p)), nc=n)
  g <- graph_from_adjacency_matrix(adjm, mode=c("undirected"),diag=FALSE)

  #makes sure we produce a connected graph
  if(components(g)$no>1){
    #Get all vectors for each group
    vectorMembership<-components(g)$membership

    #Connect any verticies not already connected
    for(i in 1:length(vectorMembership)){
      if(vectorMembership[i]==1){
        next
      }
      #connect to the vertex to a random vertex in group 1
      v<-sampleOne(which(vectorMembership==1))
      g<-g%>%add_edges(c(v,i))
    }
  }

  return(g)
}
