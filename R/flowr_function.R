#' Returns a graph showing all the arguments of a function or the arguments of a function with a given element as part of their name
#' @param functionname the name of a function in an installed and loaded package
#' @param element if provided, part of the name of the argument of interest
#' @return a graph
#' @export
#' @examples
#' library(flowrpowr)
#' flowr_function("geom_edge_link")
#' flowr_function(functionnames="geom_edge_link",
#'                element="label")


flowr_function=function(functionnames,element=NA, layout="kk"){
  f=function(functionname){
    tibble(pack_or_fun="function",
           root=functionname,
           elems=formals(functionname) %>% names())
    }
  list_elems= map_df(functionnames,f)
  if(!is.na(element)){
    list_elems=list_elems %>%
      filter(str_detect(elems,element)) %>%
      mutate(elems=map_chr(elems,protect_element,element))
  }
  flowr(list_elems, layout=layout)
}
