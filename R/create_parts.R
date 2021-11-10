
#' @title make_parts
#' @param part_num number of points
#' @param groups number of groups to assign points to at random
#' @return data frame of part x and y coordinates and integer group assignment
#' @export
#' @import magrittr
#' @example
#' parts<-make_parts(part_num=10,groups=3)
make_parts<-function(part_num=10,groups=3,seed=1){

  set.seed(seed)
  parts<-data.frame(x=rnorm(part_num),y=rnorm(part_num))
  parts$group<-sample(1:groups,part_num,replace=TRUE)
  parts
}

#' @title get_centers
#' @param parts object created by \link[randomviz]{make_parts}
#' @return adds centers as a data frame x and y coordinates and integer group assignment to parts list
#' @details centers are calculated as the mean between all part x and y coordinates seperately for each group
#' @export
#' @example
#' parts<-make_parts(part_num=10,groups=3)
#' obj<-get_centers(parts)
get_centers<-function(parts){

  tmp<-split(parts, parts$group)

  centers<-lapply(tmp, function(x){

    data.frame(x=mean(x$x),y=mean(x$y), x %>%
                .[1,colnames(.)[!colnames(.) %in% c('x','y')] ,drop=FALSE])

  })

  list(parts=tmp,centers=centers)

}
