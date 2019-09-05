#' Returns a graph showing all the arguments of a function or the arguments of a function with a given element as part of their name
#' @param functionname the name of a function in an installed and loaded package
#' @param element if provided, part of the name of the argument of interest
#' @return a graph
#' @examples
#' library(flowrpowr)
#' flowr(list_elems=tibble(elems=
#' c("pouet_boum_bam",
#'   "pouet_hop_bam",
#'   "hop_boum_bam",
#'   "hop_boum_pouet_tac",
#'   "hop_boum_pouet_toc")))


flowr=function(list_elems){
  f=function(x){
    y=NULL
    for (i in 2:length(x)){
      y=rbind(y,
              tibble(from=x[i-1],to=x[i]))
    }
    return(y)
  }

  elems_to_protect=list_elems %>%
    group_by(elems) %>%
    nest() %>%
    mutate(data=elems) %>%
    mutate(data=stringr::str_split(data,"_")) %>%
    mutate(data=map(data,f)) %>%
    unnest()%>%
    group_by(from) %>%
    mutate(n=n()) %>%
    filter(n==1) %>%
    mutate(protect_this=str_c(from,"_",to)) %>%
    na.omit() %>%
    pull(protect_this)
  if(length(elems_to_protect)>0){
  for (i in 1:length(elems_to_protect)){
    list_elems=list_elems %>%
      mutate(elems=map_chr(elems,protect_element,elems_to_protect[i]))
  }
  }
  tib=list_elems %>%
    group_by(elems) %>%
    nest() %>%
    mutate(data=elems) %>%
    mutate(data=stringr::str_split(data,"_")) %>%
    mutate(data=map(data,f)) %>%
    unnest() %>%
    mutate(elems=str_replace_all(elems,"xxx","_"))%>%
    mutate(from=str_replace_all(from,"xxx","_")) %>%
    mutate(to=str_replace_all(to,"xxx","_")) %>%
    na.omit()

  g=as_tbl_graph(tib) %>%
    activate(nodes) %>%
    filter(!node_is_isolated()) %>%
    mutate(is_source=as.numeric(node_is_source())) %>%
    mutate(is_leaf=as.numeric(node_is_sink())) %>%
    mutate(type=as.factor(str_c(is_source,is_leaf))) %>%
    ggraph(layout="kk")+
    geom_edge_link2(color="darkgrey", arrow=arrow(length=unit(2,"mm")))+
    geom_node_text(aes(label=name, color=type), show.legend=FALSE, repel=TRUE)+
    theme_graph()
  g
}


