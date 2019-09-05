#' Returns a graph showing all the arguments of a function or the arguments of a function with a given element as part of their name
#' @param functionname the name of a function in an installed and loaded package
#' @param element if provided, part of the name of the argument of interest
#' @return a graph
#' @export
#' @examples
#' library(flowrpowr)
#' flowr_function("geom_edge_link")
#' flowr_function(functionname="geom_edge_link",
#'               element="edge")


flowr_function=function(functionname,element=""){
  list_elems=tibble(elems=formals(functionname) %>% names()) %>%
    mutate(elems=str_c(functionname,"()_",elems)) %>%
    filter(str_detect(elems,element)) %>%
    mutate(elems=map_chr(elems,protect_element,element)) %>%
    mutate(elems=map_chr(elems,protect_element,functionname))
  flowr(list_elems)
}
