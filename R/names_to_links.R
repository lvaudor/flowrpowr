# transforms name patterns into links for graph representation
names_to_links=function(data){
  graphdata=NULL
  for (i in 1:length(data$patterns)){
    graphdata=rbind(graphdata,
                    tibble::tibble(index_partial=i,
                                   from=data$parts[i],
                                   to=data$parts[i+1],
                                   sep=data$seps[i],
                                   pair=stringr::str_c(from,sep,to)))
  }
  return(graphdata)
}
