protect_element=function(x,element){
  if(str_detect(x,element)){
  parts=str_split_fixed(x,element,n=2)
  x=str_c(parts[1],str_replace_all(element,"_","xxx"),parts[2])
  }
  return(x)
}
