build_edges_table=function(tib_elems, element=NA, highlighted=NA){
    tib_elems_tmp=tib_elems %>%
      dplyr::mutate(index=1:dplyr::n()) %>%
      dplyr::group_by(root,pack_or_fun,elems,index) %>%
      tidyr::nest() %>%
      dplyr::mutate(data=purrr::map(elems,parts_and_seps)) %>%
      dplyr::mutate(data=purrr::map(data,names_to_links)) %>%
      tidyr::unnest(cols=data) %>%
      dplyr::ungroup() %>%
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
      dplyr::filter(!is.na(to))
    if(length(highlighted)>1){highlighted=stringr::str_c(highlighted,collapse="|")}
    if(length(element)>1){element=stringr::str_c(element,collapse="|")}
    if(!is.na(highlighted)){
      tib=tib %>%
        dplyr::mutate(highlighted=stringr::str_detect(elems,highlighted))}
    if(!is.na(element)){
      tib=tib %>%
        dplyr::filter(stringr::str_detect(elems,element))
    }
    return(tib)
}
