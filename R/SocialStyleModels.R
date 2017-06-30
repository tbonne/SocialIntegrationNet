#' Social style model
#'
#' This function runs the social style model of network formation, where new nodes copy the style of the parent node. Style here referers to the number and strength of ties.
#' @param obsNet Starting network. This must be a non-directed igraph network.
#' @param Pm Probability of copying number of mother's partner.
#' @param Em Probability of copying strengths of mother's partners.
#' @param Pb Probability of forming ties with mother.
#' @param iter Number of iteration, where each iteration removes one node and introduces a new one.
#' @keywords Network Formation, social models
#' @export
#' @examples
#' startingNet = erdos.renyi.game(n=15, type = c("gnm"), p.or.m = 20)
#' E(startingNet)$weight<-c(2,25,1,1,1,1,4,5,6,9,2,1,2,1,2,3,4,2,3,4)
#' predictedNet<-socialStyle(startingNet, iter=100)
#' plot(startingNet,edge.width=E(startingNet)$weight)
#' plot(predictedNet,edge.width=E(predictedNet)$weight)
#'
socialStyle <- function(obsNet=startingNet, Pb=1, iter=1000){

  nodeCount <- vcount(obsNet)

  for ( i in 1: iter){

    #choose random individual to kill
    obsNet <- delete.vertices(obsNet, round(runif(n=1, min=0.5, max=nodeCount+0.5)))

    #Choose a mother node
    motherNode <- round(runif(n=1, min=0.5, max=nodeCount-0.5))

    #get mother node style
    partner_number <- length(adjacent_vertices(obsNet,motherNode, mode="all" )[[1]])

    #add new node
    obsNet <- obsNet %>% add_vertices(1)

    #choose random partner nodes
    if(partner_number>1){

      #define links
      partner_strength <- sample(E(obsNet)[from(motherNode)]$weight,partner_number-1)
      potential_partners <- c(1:(nodeCount))[-c(motherNode,nodeCount)]
      ran_nodes <- sample(potential_partners)[1:(partner_number-1)]

      #add links to partner nodes
      for (n in ran_nodes){
        partner_strength <- sample(E(obsNet)[from(motherNode)]$weight,1)
        obsNet <- obsNet %>% add_edges(c(nodeCount,n), weight=partner_strength)
      }

    }

    #link to mother node
    if(runif(1)<Pb ){
      if(partner_number>0){
        partner_strength <- sample(E(obsNet)[from(motherNode)]$weight,1)
      } else {
        partner_strength <- 1
      }
      obsNet <- obsNet %>% add_edges(c(nodeCount,motherNode), weight=partner_strength)
    }

  }
  return(obsNet)
}

