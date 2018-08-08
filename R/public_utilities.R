
#' Check if a file is writable
#'
#' This function can test if a file is writable.  A file will not be writable if another application has it open
#' and locked, or if the file permissions don't support writing
#'
#' @param fname The path to the file
#'
#' @return logical TRUE if the file is writable, FALSE if not
#' @export
#'
is_file_writable = function(fname){
  # need to see if the file exists.  If so, this check will have to try to create it and then delete it
  had.file=F
  if(file.exists(fname)) had.file=T
  tryCatch({
    con=file(fname, open="a") #it's not enought to just use file, since it can't tell if another app has it open.
    # opened for append checks our ability to write the file, without wiping it clean (like "w" would do)
    close(con)
    if(!had.file & file.exists(fname)) file.remove(fname) #if we created the file as part of this check, remove it now
    return(T)},
    error=function(x){return(F)},
    warning=function(x){return(F)})
}
