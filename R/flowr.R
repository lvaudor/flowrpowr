#' Returns a graph
#' @param tib_elems
#' @param layout the type of layout: either "kk" (for a flower-like representation, the default) or "sugiyama" (for a tree-like representation)
#' @return a graph
#' @examples
#' library(flowrpowr)
#' tib_elems=tibble(
#'   root="gwek",
#'   pack_or_fun="package",
#'   elems=c("pouet_boum.bam",
#'           "pouet_hop_bam",
#'           "hop_boum.bam",
#'           "hop_boum_pouet_tac",
#'           "hop_boum_pouet_tocBim",
#'           "hop_boum_pouet_tocBimPaf",
#'           "hop_boum_pouet_tocBoum"))
#'  flowr(tib_elems)


flowr=function(tib_elems,
               element=NA,
               layout="kk"){
  names_to_graph=function(data){
    graphdata=NULL
    for (i in 1:length(data$patterns)){
      graphdata=rbind(graphdata,
                      tibble::tibble(index_partial=i,
                                     from=data$parts[i],
                                     to=data$parts[i+1],
                                     sep=data$patterns[i],
                                     pair=stringr::str_c(from,sep,to)))
    }
    return(graphdata)
  }
  separation_pattern="(_)|((?<=\\w)\\.(?=\\w))|((?<=[:lower:])(?=[:upper:]))"
  tib_elems=tib_elems %>%
    dplyr::mutate(index=1:n()) %>%
    dplyr::group_by(root,pack_or_fun,elems,index) %>%
    tidyr::nest() %>%
    dplyr::mutate(data=elems) %>%
    dplyr::mutate(parts=stringr::str_split(data,separation_pattern)) %>%
    dplyr::mutate(patterns=stringr::str_extract_all(data,separation_pattern)) %>%
    dplyr::mutate(data=map2(.$parts,.$patterns,~list(parts=.x,patterns=.y))) %>%
    dplyr::mutate(data=map(data,names_to_graph)) %>%
    dplyr::select(-parts,-patterns) %>%
    tidyr::unnest(cols=data)
  tib_elems_pb=tib_elems %>%
    dplyr::mutate(from_is_function=root==from)%>%
    dplyr::filter(!from_is_function) %>%
    dplyr::group_by(root,from) %>%
    dplyr::mutate(n=n())%>%
    dplyr::filter(n==1) %>%
    dplyr::ungroup() %>%
    na.omit()
  if(!is.na(element)){
    tib_elems_pb=dplyr::bind_rows(tib_elems_pb,
                           dplyr::filter(tib_elems,pair==element))
  }
   if(dim(tib_elems_pb)[1]>0){
     for (i in 1:nrow(tib_elems_pb)){
       tib_elems=tib_elems %>%
         dplyr::filter(!(pair==tib_elems_pb$pair[i] & elems==tib_elems_pb$elems[i] & root==tib_elems_pb$root[i])) %>%
         dplyr::mutate(pb_to=(index==tib_elems_pb$index[i]) & (to==tib_elems_pb$from[i])) %>%
         dplyr::mutate(pb_from=(index==tib_elems_pb$index[i]) & (from==tib_elems_pb$to[i])) %>%
         dplyr::mutate(to=dplyr::case_when(pb_to~tib_elems_pb$pair[i],
                              !pb_to~to)) %>%
         dplyr::mutate(from=dplyr::case_when(pb_from~tib_elems_pb$pair[i],
                                             !pb_from~from)) %>%
         dplyr::mutate(pair=dplyr::case_when(pb_to|pb_from~stringr::str_c(from,sep,to),
                                             !(pb_to|pb_from)~pair)) %>%
         dplyr::select(-pb_to,-pb_from)
       }
   }
  # add root
  tib_firstpart=tib_elems %>%
    dplyr::filter(index_partial==1) %>%
    dplyr::mutate(to=from) %>%
    dplyr::mutate(from=root) %>%
    dplyr::mutate(sep=dplyr::case_when(pack_or_fun=="package"~"::",
                                       pack_or_fun=="function"~"(...)"))
  tib=dplyr::bind_rows(tib_elems,
                       tib_firstpart) %>%
    dplyr::filter(!is.na(to)) %>%
    unique()

  g=tidygraph::as_tbl_graph(tib) %>%
    tidygraph::filter(!tidygraph::node_is_isolated()) %>%
    tidygraph::mutate(is_source=as.numeric(tidygraph::node_is_source())) %>%
    tidygraph::mutate(is_leaf=as.numeric(tidygraph::node_is_sink())) %>%
    tidygraph::mutate(nodetype=as.factor(stringr::str_c(is_source,is_leaf))) %>%
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
      ggplot2::scale_y_reverse(expand=ggplot2::expand_scale(mult=0.1))+
      ggplot2::coord_flip()
  }
  if(layout=="kk"){
    g=g+
      ggplot2::scale_x_continuous(expand=ggplot2::expand_scale(mult=0.1))
  }

  g
}


