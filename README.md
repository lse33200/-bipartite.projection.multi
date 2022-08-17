# bipartite.projection.multi

Arguments:
   edgelist: bipartite edge list, with desired projection type in first column 
   weight: boolean, whether to include inverse of group size as edge attribute
Returns:
   new_network: igraph object, unimodal projection with multiedges
Assumes:
   igraph and dplyr packages loaded
