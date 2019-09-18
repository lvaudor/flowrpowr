#' Returns a graph showing all the arguments of a function or the arguments of a function with a given element as part of their name
#' @param functionname the name of a function in an installed and loaded package
#' @param element if provided, part of the name of the argument of interest
#' @param layout the type of layout: either "kk" (for a flower-like representation, the default) or "sugiyama" (for a tree-like representation)
#' @return a graph
#' @export
#' @examples
#' library(flowrpowr)
#' flowr_function("geom_edge_link")
#' flowr_function("geom_edge_link",
#'                element="label")
#' flowr_function(c("geom_violin","geom_point","geom_boxplot"))
#' flowr_function(c("geom_violin","geom_point","geom_boxplot"),layout="sugiyama")


flowr_function=function(functionnames,
                        element=NA,
                        layout="kk"){
  f=function(functionname){
    tibble::tibble(pack_or_fun="function",
           root=functionname,
           elems=formals(functionname) %>% names())
    }
  tib_elems= purrr::map_df(functionnames,f)
  if(!is.na(element)){
    tib_elems=tib_elems %>%
      dplyr::filter(stringr::str_detect(elems,element))
  }
  flowr(tib_elems,element=element, layout=layout)
}
