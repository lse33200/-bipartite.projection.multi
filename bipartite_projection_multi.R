


bipartite_projection_multi <- function(edgelist, weight = FALSE){
  ### Arguments:
  ###   edgelist: bipartite edge list, with desired projection type in first column 
  ###   weight: boolean, whether to include inverse of group size as edge attribute
  ### Returns:
  ###   new_network: igraph object, unimodal projection with multiedges
  ### Assumes:
  ###   igraph and dplyr packages loaded
  
  
  #List of type a nodes tied to each type b
  out <- edgelist %>% 
    group_by(type_b = .[[2]]) %>%
    summarize(type_a = paste(sort(unique(get(colnames(edgelist)[1]))), collapse="; "))
  
  
  #Separate and count
  out$type_a <- as.list(strsplit(out$type_a, "; "))
  out$count_type_a <- lengths(out$type_a)
  
  #Create dataframe with triangle of that many rows
  mytriangle <- function(n) (n*(n-1)/2) #Triangle number for (n-1) where n is degree
  out$times <- mytriangle(out$count_type_a)
  
  #Create df with each type b node, mytriangle() times
  new_df <- data.frame(type_b = rep(out$type_b, times = out$times))
  
  #Column where indivs repeats n - 1 times (First half of edgelist)
  #Create empty list
  longindivs <- list()
  
  for (i in 1:nrow(out)){
    times_seq <- seq(out$count_type_a[i] - 1, 0) #Vector counting down from n - 1
    longindivs[[i]] <- rep(out$type_a[i][[1]], times = times_seq) #Repeat elements in indivs 
  }
  
  #Make one long vector
  longindivs <- unlist(longindivs, recursive = TRUE)
  new_df$a1 <- longindivs #Add to new df
  
  #Column cycling through each vector in shorter recursions (second half of edgelist)
  loopyindivs<- list()
  
  for (i in 1:nrow(out)){
    n <- out$count_type_a[i]
    long <- c()
    for (j in 2:n){
      segment <- out$type_a[i][[1]][j:n]
      #print(segment)
      long <- append(long, segment)
    }
    loopyindivs[[i]] <- long
  }
  
  loopyindivs <- unlist(loopyindivs, recursive = TRUE)
  new_df$a2 <- loopyindivs
  
  #Move affiliation to end of the df
  new_df <- new_df %>% relocate(type_b, .after = last_col())
  
  #Add weight as attribute if weight is set to TRUE
  if(weight == TRUE){
    new_df$weight <- rep((1/(out$count_type_a)), times = out$times) #Modified, in mine subtract 1 from denom
  }
  
  #Create graph
  new_network <- graph_from_edgelist(as.matrix(new_df[,1:2]), directed = FALSE)
  
  #Add names and weights as edge attributes
  E(new_network)$type_b <- new_df$type_b
  if(weight == TRUE){
    #Weight (1/number of indivs) as edge attribute
    E(new_network)$weight <- new_df$weight
  }
  
  return(new_network)
}