#' compare_mappings
#'
#' @param map1 First mappings list
#' @param map2 Second mappings list
#'
#' @details Compares two domain mappings lists and gives a data frame
#' with the differences.
#' @return a dataframe listing the differences
#' @importFrom dplyr data_frame setdiff intersect bind_rows
#' @importFrom purrr map_chr
compare_mappings = function(map1, map2){
  names(map1$Columns) = map_chr(map1$Columns,"Name")
  names(map2$Columns) = map_chr(map2$Columns,"Name")

  #unlist for value comparisons
  map1 = unlist(map1, recursive=T, use.names = T)
  map2 = unlist(map2, recursive=T, use.names = T)


  # get in map1 not in map2
  df1 = df2 = data_frame(Setting=character(0),
                         map1 = character(0),
                         map2=character(0)
                         )
  c1.nin.c2 = setdiff(names(map1), names(map2))
  c1.nin.c2 = map1[c1.nin.c2]
  if(length(c1.nin.c2)){
    df1 = data_frame(Setting=names(c1.nin.c2),
                     map1 = c1.nin.c2,
                     map2="."
                     )
  }

  # get in map2 not in map1
  c2.nin.c1 = setdiff(names(map2), names(map1))
  c2.nin.c1 = map2[c2.nin.c1]
  if(length(c2.nin.c1)){
    df2 = data_frame(Setting=names(c2.nin.c1),
                     map1 = ".",
                     map2=c2.nin.c1
    )
  }

  # get common names
  cols = intersect(names(map1), names(map2))
  map1=map1[cols]
  map2=map2[cols]

  # get differences
  m1.ne.m2 = map1[sapply(names(map1), function(x) !identical(map1[[x]], map2[[x]]))]
  m2.ne.m1 = map2[sapply(names(map2), function(x) !identical(map2[[x]], map1[[x]]))]

  df = data_frame(Setting=names(m1.ne.m2), map1=m1.ne.m2, map2=m2.ne.m1)
  bind_rows(df1,df2,df)
}
