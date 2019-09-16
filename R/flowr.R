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
  split_elems=function(x){
    y=NULL
    for (i in 2:length(x)){
      y=rbind(y,
              tibble::tibble(from=x[i-1],to=x[i]))
    }
    return(y)
  }

  elems_to_protect=list_elems %>%
    dplyr::group_by(root,elems) %>%
    tidyr::nest() %>%
    dplyr::mutate(data=elems) %>%
    dplyr::mutate(data=stringr::str_split(data,"(_)|((?<=\\w)\\.(?=\\w))")) %>%
    dplyr::mutate(data=map(data,split_elems)) %>%
    tidyr::unnest(cols=data)  %>%
    dplyr::mutate(from_is_function=root==from)%>%
    dplyr::filter(!from_is_function) %>%
    dplyr::group_by(root,from) %>%
    dplyr::mutate(n=n())%>%
    dplyr::filter(n==1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(pattern=map2_chr(.$from,.$to,~str_c(.x,"(_|\\.)",.y))) %>%
    dplyr::mutate(protect_this=map2_chr(.$elems,.$pattern,str_extract))%>%
    na.omit() %>%
    dplyr::pull(protect_this) %>%
    unique()

  list_elems_protected=list_elems
  if(length(elems_to_protect)>0){
  for (i in 1:length(elems_to_protect)){
    list_elems_protected=list_elems_protected %>%
      dplyr::mutate(elems=purrr::map_chr(elems,protect_element,elems_to_protect[i]))
  }
  }
  tib=list_elems_protected %>%
    dplyr::group_by(pack_or_fun,root,elems) %>%
    tidyr::nest() %>%
    dplyr::mutate(data=elems) %>%
    dplyr::mutate(data=stringr::str_split(data,"(_)|((?<=\\w)\\.(?=\\w))")) %>%
    dplyr::mutate(data=map(data,split_elems)) %>%
    tidyr::unnest(cols=data)%>%
    dplyr::ungroup() %>%
    na.omit() %>%
    dplyr::mutate(pattern=purrr::map2_chr(.$from,.$to,~str_c("(?<=",.x,")[_|\\.](?=",.y,")"))) %>%
    dplyr::mutate(sep=purrr::map2_chr(.$elems,.$pattern,~str_extract(.x,.y)))
  # add root
  tib=bind_rows(tib,
                tibble::tibble(pack_or_fun=tib$pack_or_fun,
                       root=tib$root,
                       elems=tib$elems,
                       from=tib$root,
                       to=tib$from) %>%
                  unique())
  list_elems_protected=list_elems_protected %>%
    dplyr::filter(!(elems %in% tib$elems))
  tib=bind_rows(tib,
                tibble::tibble(pack_or_fun=list_elems_protected $pack_or_fun,
                       root=list_elems_protected $root,
                       elems=list_elems_protected $elems,
                       from=list_elems_protected $root,
                       to=list_elems_protected $elems))%>%
    mutate(elems=stringr::str_replace_all(elems,"xxx","_"))%>%
    mutate(from=stringr::str_replace_all(from,"xxx","_")) %>%
    mutate(to=stringr::str_replace_all(to,"xxx","_")) %>%
    mutate(elems=stringr::str_replace_all(elems,"yyy","."))%>%
    mutate(from=stringr::str_replace_all(from,"yyy",".")) %>%
    mutate(to=stringr::str_replace_all(to,"yyy",".")) %>%
    mutate(sep=dplyr::case_when((is.na(sep))&(pack_or_fun=="function")~"(...)",
                         (is.na(sep))&(pack_or_fun=="package")~"::",
                         !is.na(sep)~sep))

  g=tidygraph::as_tbl_graph(tib) %>%
    tidygraph::filter(!node_is_isolated()) %>%
    tidygraph::mutate(is_source=as.numeric(node_is_source())) %>%
    tidygraph::mutate(is_leaf=as.numeric(node_is_sink())) %>%
    tidygraph::mutate(nodetype=as.factor(str_c(is_source,is_leaf))) %>%
    ggraph::ggraph(layout=layout)+
    ggraph::geom_edge_link(aes(edge_colour=sep),
                   arrow=arrow(length=unit(3,"mm")),
                   edge_width=1,alpha=0.5)+
    ggraph:: geom_node_label(aes(label=name,fill=nodetype),
                    show.legend=FALSE,label.r=unit(0.5,"lines"),
                    alpha=0.3,
                    label.size=0)+
    ggraph::theme_graph()+
    ggraph::scale_edge_colour_manual(breaks=c("::",".","_","(...)"),
                             values=c("goldenrod","darkolivegreen4","darkolivegreen1","goldenrod2"))+
    ggplot2::scale_fill_manual(values=c("mediumpurple2","indianred1","gold1"))
  if(layout=="sugiyama"){
    g=g +
      ggplot2::scale_y_reverse(expand=expand_scale(mult=0.1))+
      ggplot2::coord_flip()
  }
  if(layout=="kk"){
    g=g+
      ggplot2::scale_x_continuous(expand=expand_scale(mult=0.1))
  }

  g
}


