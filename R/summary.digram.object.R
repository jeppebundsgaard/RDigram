#' @title Summarize DIGRAM object
#' @name summary.digram.object
#' @description Summarizes information about a DIGRAM object
#' @param do a DIGRAM object
#' @export
#' @return Returns nothing
#' @author Jeppe Bundsgaard <jebu@@edu.au.dk>
#' @examples summary(DHP)
#' @references
#' Kreiner, S. (2003). *Introduction to DIGRAM*. Dept. of Biostatistics, University of Copenhagen.
summary.digram.object <- function(do=NULL) {
  print(do)
  score.information(do = do,items = 1:do$recursive.structure[1],accept.na = sum(apply(do$recoded[1:do$recursive.structure[1]],1,sum),na.rm = T)==0)
}
