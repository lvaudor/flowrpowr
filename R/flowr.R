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
  tib_elems_tmp=tib_elems %>%
    dplyr::mutate(index=1:n()) %>%
    dplyr::group_by(root,pack_or_fun,elems,index) %>%
    tidyr::nest() %>%
    dplyr::mutate(data=purrr::map(elems,parts_and_seps)) %>%
    dplyr::mutate(data=purrr::map(data,names_to_links)) %>%
    tidyr::unnest(cols=data) %>%
    ungroup() %>%
    regather_false_pairs()
    if(!is.na(element)){
      tib_elems_tmp=dplyr::bind_rows(tib_elems_tmp,
                                     dplyr::filter(tib_elems_tmp,pair==element))
    }
  # add root
  tib_firstpart=tib_elems_tmp %>%
    dplyr::filter(index_partial==1) %>%
    dplyr::mutate(to=from) %>%
    dplyr::mutate(from=root) %>%
    dplyr::mutate(index_partial=0) %>%
    dplyr::mutate(sep=dplyr::case_when(pack_or_fun=="package"~"::",
                                       pack_or_fun=="function"~"(...)"))
  tib=dplyr::bind_rows(tib_elems_tmp,
                       tib_firstpart) %>%
    unique() %>%
    filter(!is.na(to))

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


