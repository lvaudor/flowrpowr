#' When one source node is linked only to one target node they are considered a false pair
regather_false_pairs=function(tib){
    tib=assess_pairs(tib)
    correct_these=which(tib$to_be_corrected)
    while(length(correct_these)>0){
          ind=max(which(tib$to_be_corrected))
          tib[ind,]=tib[ind,] %>%
            dplyr::mutate(from=pair,
                   to=pair,
                   sep=NA,
                   pair=NA)
          indprev=which(tib$index==tib$index[ind] & tib$index_partial==tib$index_partial[ind]-1)
          indnext=which(tib$index==tib$index[ind] & tib$index_partial==tib$index_partial[ind]+1)
          if(length(indprev)>0){
            tib[indprev,]=tib[indprev,] %>%
              dplyr::mutate(to=tib[ind,]$from,
                     pair=stringr::str_c(from,sep,to))
          }
          if(length(indnext)>0){
              tib[indnext,]=tib[indnext,] %>%
                  dplyr::mutate(from=tib[ind,]$to,
                                pair=stringr::str_c(from,sep,to)) %>%
                  dplyr::mutate(index_partial=index_partial-1)

          }
          if(length(indprev)>0){
              tib=tib%>%
                  dplyr::slice(-ind)
          }else{tib[ind,]=dplyr::mutate(tib[ind,],to=NA)}
          tib=assess_pairs(tib)
          correct_these=which(tib$to_be_corrected)
    }
    tib=tib %>%
        dplyr::select(-from_is_function,
                      -to_be_corrected)
    return(tib)
}
