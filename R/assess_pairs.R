# when one source node is linked to only one target node the pair is considered false and thus to be corrected
assess_pairs=function(tib){
  tib = tib %>%
    dplyr::mutate(from_is_function=root==from|elems==from)%>%
    dplyr::group_by(root,from)%>%
    dplyr::mutate(n=dplyr::n())%>%
    dplyr::ungroup() %>%
    dplyr::mutate(to_be_corrected=dplyr::case_when(n==1 &
                                                   !is.na(to) &
                                                   !from_is_function~TRUE,
                                                   TRUE~FALSE))
  return(tib)
}
