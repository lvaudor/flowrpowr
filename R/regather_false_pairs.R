#' When one source node is linked only to one target node they are considered a false pair
regather_false_pairs=function(tib){
    tib=assess_pairs(tib)
    correct_these=which(tib$to_be_corrected)
    while(length(correct_these)>0){
          ind=max(which(tib$to_be_corrected))
          tib[ind,]=tib[ind,] %>%
            mutate(from=pair,
                   to=NA,
                   sep=NA,
                   pair=NA)
          if(tib[ind,]$index_partial>1){
            tib[ind-1,]=tib[ind-1,] %>%
              mutate(to=tib[ind,]$from,
                     pair=stringr::str_c(from,sep,to))
            tib=tib%>%
              slice(-n())
          }
          tib=assess_pairs(tib)
          correct_these=which(tib$to_be_corrected)
    }
    tib=tib %>%
        select(-from_is_function,
               -to_be_corrected)
    return(tib)
}
