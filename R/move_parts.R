#' @title get_centers
#' @param parts object created by \link[randomviz]{make_parts}
#' @param center object calculated by \link[randomviz]{get_centers}
#' @param attract boolean move parts toward center (TRUE) or away from the center (FALSE)
#' @param increment the distance each point is moved as a percent of the total distance to the center. For example increment=0.5 corresponds to a step of half the distance to the center for each step.
#' @return list of parts and data frame of part centers x and y coordinates and integer group assignment
#' @export
#' @example
#' parts<-make_parts(part_num=10,groups=3)
#' obj<-get_centers(parts)
#' get_linear_forces(obj$parts,obj$centers)
get_linear_forces <- function(parts, center, attract=T,increment = .5) {

  #move points to center at
  #increment of total distance
  diffx<-parts$x - center$x
  diffy<-parts$y - center$y

  if(!attract){

    parts$x<-parts$x - diffx*increment
    parts$y<-parts$y - diffy*increment

  } else {

    parts$x<-parts$x + diffx*increment
    parts$y<-parts$y + diffy*increment

  }

  parts

}

#' @title points_linear_attractor
#' @param obj object created by \link[randomviz]{make_parts} followed by \link[randomviz]{get_centers}
#' @param iter integer of iterations to calculate attraction or repulsion based on \link[randomviz]{get_linear_forces}
#' @return list of iteration results for parts
#' @export
#' @importFrom purrr flatten
#' @example
#' parts<-make_parts(part_num=10,groups=3)
#' obj<-get_centers(parts)
#' get_linear_forces(obj$parts,obj$centers)
points_linear_attractor<-function(obj,iter=10,...){

  results<-list()
  .parts<-lapply(1:length(obj$parts),function(i){

    .parts<-new<-obj$parts[[i]]
    .center<-obj$centers[[i]]

    .results<-list()

    for(j in 1:iter){

      .results[[j]]<-new<-get_linear_forces(new,.center,...)
    } %>%
      do.call('rbind',.)


    results[[i]]<-.results

  })

  .parts<-flatten(.parts)

  obj$parts<-c(.parts,results)
  obj
}


#rotate points
rotate_parts <- function(parts, angle, center, group) {

  co <- cos(-angle * pi / 180)
  si <- sin(-angle * pi / 180)
  tmp<-list()
  tmp$x<-parts$x-center$x
  tmp$y<-parts$y-center$y
  .tmp<-list(x = co * tmp$x - si * tmp$y, y = si * tmp$x + co *tmp$y)
  .tmp$x<-.tmp$x - tmp$x
  .tmp$y<-.tmp$y - tmp$y

  data.frame(x=.tmp$x, y=.tmp$y,group=group)
}

rotate_obj<-function(obj,angle=45){

  .parts<-lapply(1:length(obj$parts),function(i){

    rotate_parts(obj$parts[[i]][,1:2,drop=F],angle,obj$centers[[i]][,1:2,drop=F],group = obj$centers[[i]]$group )

  })

  obj$parts<-.parts
  obj
}

#' @title rotate_obj_angles
#' @param obj list of parts and centers
#' @param angles integer values for angles of rotation 1 to 360
#' @export
rotate_obj_angles<-function(obj,angles){

  #calculate all parts rotation
  res<-list()
  #calculate
  for (i in 1:length(angles)){

    res[[i]]<-rotate_obj(obj,angles[i])$parts

  }

  obj$parts<-res  %>%  flatten()

  obj

}
