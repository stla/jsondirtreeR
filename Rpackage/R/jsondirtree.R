.dirToJSONtree <- function(dir, depth){
  .C("dirToJSONtreeR", depth=as.integer(depth), dir=dir,
     result=character(0L))$result
}

#' @title JSON representation of a directory
#' @description Returns the hiearchical structure of a folder as a JSON string.
#' @export
#' @return A string.
#' @param dir the path of the directory
#' @param depth a nonnegative integer, the desired depth, or \code{NULL}
dirToJSONtree <- function(dir, depth=NULL){
  if(!dir.exists(dir)){
    stop(sprintf("Folder %s not found.", dir))
  }
  .dirToJSONtree(dir, ifelse(is.null(depth), -1L, depth))
}
