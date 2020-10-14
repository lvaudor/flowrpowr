# Separate names into parts and separation patterns
parts_and_seps=function(elem){
  separation_pattern="(_)|((?<=\\w)\\.(?=\\w))|((?<=[:lower:])(?=[:upper:]))"
  result=list(
    parts=stringr::str_split(elem,separation_pattern) %>% unlist(),
    seps=stringr::str_extract_all(elem,separation_pattern) %>% unlist()
  )
  return(result)
}
