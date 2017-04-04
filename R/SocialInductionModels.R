

#' Social induction model
#'
#' This function runs the social induction model of network formation (see Ilany et al. 2016)
#' @param obsNet Starting network. This must be a non-weighted and non-directed igraph network. 
#' @param Pn Probability of forming ties with a mother's partner.
#' @param Pr Probability of forming ties with a non mother's partner.
#' @param Pb Probability of forming ties with mother.
#' @param iter Number of iteration, where each iteration removes one node and introduces a new one.
#' @keywords Network Formation, social models
#' @export
#' @references 
#' Ilany A, Akcay E. Social inheritance can explain the structure of animal social networks. Nature Communications. 2016;7:12084. doi:10.1038/ncomms12084.
#' @examples
#' startingNet = erdos.renyi.game(n=25, type = c("gnm"), p.or.m = 45)
#' predictedNet<-socialInduction(startingNet, Pn=0.4, Pr=0.1, Pb=1, iter=100)
#' plot(startingNet)
#' plot(predictedNet)
#' 
socialInduction <- function(obsNet=startingNet, Pn, Pr, Pb, iter){
  
  nodeCount <- vcount(obsNet)
  
  for ( i in 1: iter){
    
    #choose random individual to kill
    obsNet <- delete.vertices(obsNet, round(runif(n=1, min=0.5, max=nodeCount+0.5)))
    
    #add one individual from a mother node
    motherNode <- round(runif(n=1, min=0.5, max=nodeCount-0.5))
    motherPartners <- adjacent_vertices(obsNet,motherNode, mode="all" )
    obsNet <- obsNet %>% add_vertices(1)
    
    #link to groupMembers
    for (n in 1:nodeCount){
      
      if(n!=nodeCount){ #not the new node
        
        if(n == motherNode){
          
          #probability of forming ties with mother 
          if(runif(1)<Pb) obsNet <- obsNet %>% add_edges(c(nodeCount,motherNode))
          
        } else {
          
          if(n %in% motherPartners[[1]]){
            
            #probability of forming ties with a partner of your mother's
            if(runif(1)<Pn) obsNet <- obsNet %>% add_edges(c(nodeCount,n))
            
          } else {
            
            #probability of forming ties with a non-partner of your mother's
            if(runif(1)<Pr) obsNet <- obsNet %>% add_edges(c(nodeCount,n))
            
          }
        }
      }
    }
  } 
  
  return(obsNet)
}



#' Social induction model with weights
#'
#' This function runs the social induction model of network formation (see Ilany et al. 2016), with the addition of weighted edges.
#' @param obsNet Starting network. This must be a weighted and non-directed igraph network. 
#' @param Pn Probability of forming ties with a mother's partner.
#' @param Pr Probability of forming ties with a non mother's partner.
#' @param Pb Probability of forming ties with mother.
#' @param En1 Alpha parameter for effort into a mother's partner. Where effort is defined by a beta distribution (0,1) using alpah and beta.
#' @param En2 Beta parameter for effort into a mother's partner. Where effort is defined by a beta distribution (0,1) using alpah and beta.
#' @param Er1 Alpha parameter for effort into a non mother's partner. Where effort is defined by a beta distribution (0,1) using alpah and beta.
#' @param Er2 Beta parameter for effort into a non mother's partner. Where effort is defined by a beta distribution (0,1) using alpah and beta.
#' @param maxE Maximum grooming effort used to scale the beta distribution. This choice should represent an upper limit of effort.
#' @param iter Number of iteration, where each iteration removes one node and introduces a new one.
#' @keywords Network Formation, social models
#' @export
#' @examples
#' startingNet = erdos.renyi.game(n=10, type = c("gnm"), p.or.m = 15)
#' E(startingNet)$weight<-c(2,25,1,1,1,1,4,5,6,9,2,1,2,1,2)
#' predWeightedNet<-socialInductionWeighted(startingNet, Pn=0.4,Pr=0.1,Pb=1,En1=0.1,En2=4,Er1=0.1,Er2=4,maxE=100,iter=100)
#' plot(startingNet, edge.width=E(startingNet)$weight)
#' plot(predWeightedNet, edge.width=E(predWeightedNet)$weight)
#' 
#' 

socialInductionWeighted <- function(obsNet=startingNet, Pn, Pr, Pb, En1, En2, Er1, Er2,maxE, iter){
  
  nodeCount <- vcount(obsNet)
  
  for ( i in 1: iter){
    
    #choose random individual to kill
    obsNet <- delete.vertices(obsNet, round(runif(n=1, min=0.5, max=nodeCount+0.5)))
    
    #add one individual from a mother node
    motherNode <- round(runif(n=1, min=0.5, max=nodeCount-0.5))
    motherPartners <- adjacent_vertices(obsNet,motherNode, mode="all" )
    obsNet <- obsNet %>% add_vertices(1)
    
    #link to groupMembers
    for (n in 1:nodeCount){
      
      if(n!=nodeCount){ #not the new node
        
        if(n == motherNode){
          
          #probability of forming ties with mother
          effort<-round(max(rbeta(1,En1,En2)*maxE,1))
          if(runif(1)<Pb) obsNet <- obsNet %>% add_edges(c(nodeCount,motherNode), weight=effort)
          
        } else {
          
          if(n %in% motherPartners[[1]]){
            
            #probability of forming ties with a partner of your mother's
            effort<-round(max(rbeta(1,En1,En2)*maxE,1))
            if(runif(1)<Pn) obsNet <- obsNet %>% add_edges(c(nodeCount,n), weight=effort)
            
          } else {
            
            #probability of forming ties with a non-partner of your mother's
            effort<-round(max(rbeta(1,En1,En2)*maxE,1))
            if(runif(1)<Pr) obsNet <- obsNet %>% add_edges(c(nodeCount,n), weight=effort)
            
          }
        }
      }
    }
  } 
  
  return(obsNet)
}