#' Returns a graph
#' @param tib_elems
#' @param layout the type of layout: either "kk" (for a flower-like representation, the default) or "sugiyama" (for a tree-like representation)
#' @param highlighted functions or arguments that are to be highlighted in the graph
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
               layout="kk",
               highlighted=NA){
  tib=build_edges_table(tib_elems=tib_elems,
                        element=element,
                        highlighted=highlighted)
  tibn=tibble::tibble(name=unique(c(tib$from,tib$to))) %>%
    dplyr::mutate(highlighted=TRUE)
  if(!is.na(highlighted)){
    tibn=tibn %>%
      dplyr::select(-highlighted) %>%
      dplyr::left_join(tib %>%
                         dplyr::select(from,highlighted),by=c("name"="from")) %>%
      dplyr::left_join(tib %>%
                         dplyr::select(to,highlighted),by=c("name"="to")) %>%
      dplyr::mutate(highlighted=highlighted.x|highlighted.y) %>%
      dplyr::group_by(name) %>%
      dplyr::summarise(highlighted=as.logical(sum(highlighted,na.rm=TRUE))) %>%
      dplyr::ungroup()
  }

  tibg=tidygraph::tbl_graph(nodes=tibn,edges=tib)
  tibg=tibg%>%
    tidygraph::filter(!tidygraph::node_is_isolated()) %>%
    tidygraph::mutate(is_source=as.numeric(tidygraph::node_is_source())) %>%
    tidygraph::mutate(is_leaf=as.numeric(tidygraph::node_is_sink())) %>%
    tidygraph::mutate(nodetype=as.factor(stringr::str_c(is_source,is_leaf)))
  g=tibg%>%
    ggraph::ggraph(layout=layout)+
    ggraph::geom_edge_link(ggplot2::aes(edge_colour=sep),
                   arrow=ggplot2::arrow(length=unit(3,"mm")),
                   edge_width=1,alpha=0.5)+
    ggraph:: geom_node_label(ggplot2::aes(label=name,fill=nodetype,colour=highlighted),
                    show.legend=FALSE,label.r=unit(0.5,"lines"),
                    alpha=0.3,
                    label.size=0)+
    ggraph::theme_graph()+
    ggraph::scale_edge_colour_manual(breaks=c("::",".","_","(...)"),
                                     values=c("goldenrod","darkolivegreen4","darkolivegreen1","goldenrod2"))+
    ggplot2::scale_fill_manual(values=c("mediumpurple2","indianred1","gold1"))+
    ggplot2::scale_colour_manual(values=c("black","darkgrey"), breaks=c(TRUE,FALSE))
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


