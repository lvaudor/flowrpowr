#' Returns a graph showing all the arguments of a function or the arguments of a function with a given element as part of their name
#' @param functionname the name of an installed and loaded package
#' @param element if provided, part of the name of the argument of interest
#' @param layout the type of layout: either "kk" (for a flower-like representation, the default) or "sugiyama" (for a tree-like representation)
#' @return a graph
#' @export
#' @examples
#' @examples
#' library(flowrpowr)
#' flowr_package("tidygraph")
#' flowr_package(packagename="tidygraph",
#'               element="node", layout="sugiyama")
#' flowr_package(packagename="tidygraph",
#'               element="node_rank")
flowr_package=function(packagename,
                       layout="kk",
                       element=NA,
                       highlighted=NA,
                       return_tibble=FALSE){
  tib_elems=tibble::tibble(
    pack_or_fun="package",
    root=packagename,
    elems=ls(paste0("package:",packagename)))
  if(return_tibble){return(tib_elems)}
  flowr(tib_elems,
        layout=layout,
        element=element,
        highlighted=highlighted)
}

