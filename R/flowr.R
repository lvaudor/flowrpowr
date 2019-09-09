#' Returns a graph
#' @param list_elems
#' @param layout
#' @return a graph
#' @examples
#' library(flowrpowr)
#' flowr(list_elems=tibble(elems=
#' c("pouet_boum.bam",
#'   "pouet_hop_bam",
#'   "hop_boum.bam",
#'   "hop_boum_pouet_tac",
#'   "hop_boum_pouet_toc")))


flowr=function(list_elems, layout="kk"){
  f=function(x){
    y=NULL
    for (i in 2:length(x)){
      y=rbind(y,
              tibble(from=x[i-1],to=x[i]))
    }
    return(y)
  }

  elems_to_protect=list_elems %>%
    group_by(root,elems) %>%
    nest() %>%
    mutate(data=elems) %>%
    mutate(data=stringr::str_split(data,"(_)|((?<=\\w)\\.(?=\\w))")) %>%
    mutate(data=map(data,f)) %>%
    unnest()  %>%
    mutate(from_is_function=root==from)%>%
    filter(!from_is_function) %>%
    group_by(root,from) %>%
    mutate(n=n())%>%
    filter(n==1) %>%
    ungroup() %>%
    mutate(pattern=map2_chr(.$from,.$to,~str_c(.x,"(_|\\.)",.y))) %>%
    mutate(protect_this=map2_chr(.$elems,.$pattern,str_extract))%>%
    na.omit() %>%
    pull(protect_this) %>%
    unique()

  if(length(elems_to_protect)>0){
  for (i in 1:length(elems_to_protect)){
    list_elems=list_elems %>%
      mutate(elems=map_chr(elems,protect_element,elems_to_protect[i]))
  }
  }
  tib=list_elems %>%
    group_by(root,elems) %>%
    nest() %>%
    mutate(data=elems) %>%
    mutate(data=stringr::str_split(data,"(_)|((?<=\\w)\\.(?=\\w))")) %>%
    mutate(data=map(data,f)) %>%
    unnest()%>%
    na.omit()
  tib=bind_rows(tib,
                tibble(root=tib$root,
                       elems=tib$elems,
                       from=tib$root,
                       to=tib$from) %>%
                  unique())
  list_elems=list_elems %>% filter(!(elems %in% tib$elems))
  tib=bind_rows(tib,
                tibble(root=list_elems$root,
                       elems=list_elems$elems,
                       from=list_elems$root,
                       to=list_elems$elems))%>%
    mutate(elems=str_replace_all(elems,"xxx","_"))%>%
    mutate(from=str_replace_all(from,"xxx","_")) %>%
    mutate(to=str_replace_all(to,"xxx","_")) %>%
    mutate(elems=str_replace_all(elems,"yyy","."))%>%
    mutate(from=str_replace_all(from,"yyy",".")) %>%
    mutate(to=str_replace_all(to,"yyy","."))

  g=as_tbl_graph(tib) %>%
    activate(nodes) %>%
    filter(!node_is_isolated()) %>%
    mutate(is_source=as.numeric(node_is_source())) %>%
    mutate(is_leaf=as.numeric(node_is_sink())) %>%
    mutate(type=as.factor(str_c(is_source,is_leaf))) %>%
    ggraph(layout=layout)+
    geom_edge_link(color="darkgrey", arrow=arrow(length=unit(3,"mm")),edge_width=0.5)+
    geom_node_text(aes(label=name, color=type), show.legend=FALSE)+
    theme_graph()
  if(layout %in% c("sugiyama","dendrogram","treemap")){
    g=g +
      scale_y_reverse()+
      coord_flip()
  }

  g
}


