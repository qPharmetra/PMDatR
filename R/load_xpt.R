

#' Load a sas xport file into a domain object
#'
#' @param path The fully qualified filename
#' @param tbl If more than one table is present, which table to load

#'
#' @return a data.frame compatible object
#' @export
#' @importFrom foreign read.xport
#' @importFrom dplyr as_data_frame
#'

load_xpt = function(path, tbl=1){
  df = foreign::read.xport(path)
  if(!inherits(df,"data.frame")) df = ret[[tbl]]
  dplyr::as_data_frame(df)
}
