#' Returns a graph
#' @param list_elems
#' @param layout the type of layout: either "kk" (for a flower-like representation, the default) or "sugiyama" (for a tree-like representation)
#' @return a graph
#' @examples
#' library(flowrpowr)
#' flowr(list_elems=tibble(
#'   root="gwek",
#'   pack_or_fun="package",
#'   elems=c("pouet_boum.bam",
#'           "pouet_hop_bam",
#'           "hop_boum.bam",
#'           "hop_boum_pouet_tac",
#'           "hop_boum_pouet_toc")))


flowr=function(list_elems,
               layout="kk"){
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

  list_elems_protected=list_elems
  if(length(elems_to_protect)>0){
  for (i in 1:length(elems_to_protect)){
    list_elems_protected=list_elems_protected %>%
      mutate(elems=map_chr(elems,protect_element,elems_to_protect[i]))
  }
  }
  tib=list_elems_protected %>%
    group_by(pack_or_fun,root,elems) %>%
    nest() %>%
    mutate(data=elems) %>%
    mutate(data=stringr::str_split(data,"(_)|((?<=\\w)\\.(?=\\w))")) %>%
    mutate(data=map(data,f)) %>%
    unnest()%>%
    na.omit() %>%
    mutate(pattern=map2_chr(.$from,.$to,~str_c("(?<=",.x,")[_|\\.](?=",.y,")"))) %>%
    mutate(sep=map2_chr(.$elems,.$pattern,~str_extract(.x,.y)))
  # add root
  tib=bind_rows(tib,
                tibble(pack_or_fun=tib$pack_or_fun,
                       root=tib$root,
                       elems=tib$elems,
                       from=tib$root,
                       to=tib$from) %>%
                  unique())
  list_elems_protected=list_elems_protected %>% filter(!(elems %in% tib$elems))
  tib=bind_rows(tib,
                tibble(pack_or_fun=list_elems_protected $pack_or_fun,
                       root=list_elems_protected $root,
                       elems=list_elems_protected $elems,
                       from=list_elems_protected $root,
                       to=list_elems_protected $elems))%>%
    mutate(elems=str_replace_all(elems,"xxx","_"))%>%
    mutate(from=str_replace_all(from,"xxx","_")) %>%
    mutate(to=str_replace_all(to,"xxx","_")) %>%
    mutate(elems=str_replace_all(elems,"yyy","."))%>%
    mutate(from=str_replace_all(from,"yyy",".")) %>%
    mutate(to=str_replace_all(to,"yyy",".")) %>%
    mutate(sep=case_when((is.na(sep))&(pack_or_fun=="function")~"(...)",
                         (is.na(sep))&(pack_or_fun=="package")~"::",
                         !is.na(sep)~sep))

  g=as_tbl_graph(tib) %>%
    filter(!node_is_isolated()) %>%
    mutate(is_source=as.numeric(node_is_source())) %>%
    mutate(is_leaf=as.numeric(node_is_sink())) %>%
    mutate(nodetype=as.factor(str_c(is_source,is_leaf))) %>%
    ggraph(layout=layout)+
    geom_edge_link(aes(edge_colour=sep),
                   arrow=arrow(length=unit(3,"mm")),
                   edge_width=1,alpha=0.5)+
    geom_node_label(aes(label=name,fill=nodetype),
                    show.legend=FALSE,label.r=unit(0.5,"lines"),
                    alpha=0.3,
                    label.size=0)+
    theme_graph()+
    scale_edge_colour_manual(breaks=c("::",".","_","(...)"),
                             values=c("goldenrod","darkolivegreen4","darkolivegreen1","goldenrod2"))+
    scale_fill_manual(values=c("mediumpurple2","indianred1","gold1"))
  if(layout=="sugiyama"){
    g=g +
      scale_y_reverse(expand=expand_scale(mult=0.1))+
      coord_flip()
  }
  if(layout=="kk"){
    g=g+scale_x_continuous(expand=expand_scale(mult=0.1))
  }

  g
}


