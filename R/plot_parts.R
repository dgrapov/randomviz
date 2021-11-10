#' @title obj_dims
#' @param obj list of parts
#' @param zoom decimal and percent to zoom (e.g. zoom out, 0 to 1; zoom in, -1 to 0)
#' @return adds dims or x and y limits for plotting
#' @export
#' @example
#' parts<-make_parts(part_num=10,groups=3)
#' obj<-get_centers(parts)
#' obj<- obj %>%
#' obj_dims()
obj_dims<-function(obj, zoom=NULL){

  tmp<-obj$parts %>% do.call('rbind',.)
  dims<-list(x=range(tmp$x),y=range(tmp$y)) %>%
    data.frame()

  obj$dims<-dims

  if(!is.null(zoom)){
    obj$dims<-obj$dims - obj$dims*zoom
  }

  obj

}

#' @title random_colors
#' @param id lintege specifying index to choose from colors()
#' @param seed random seed to scramble colors()
#' @importFrom  grDevices adjustcolor
#' @export
#set colors based on group
random_colors<-function(id=NULL,alpha=1,seed=123){

  #seed to scramble colors() so id is also rnadom but reproducible
  set.seed(seed)
  .col<-colors() %>%
    .[sample(1:length(.),length(.))]

  if(is.null(id)) {

    col<-.col %>%
      .[sample(1:length(.),1)]

  } else {
    col <- .col[id]
  }

  adjustcolor(col,alpha.f=alpha)

}

#' @title get_colors
#' @export
#' @param obj containing a list of parts
#' @param fun string name of function to generate color for each part
get_colors<-function(obj,color_fun='random_colors',...){

  .col<-lapply(obj$parts, function(x){
    id<-as.numeric(x$group) %>% unique()
    do.call(color_fun,list(id=id,...))

  })

  obj$colors<-.col
  obj

}
#' @title init_plot
#' @export
#' @details initialize empty plot based on parts$dims limits
init_plot<-function(obj,...){

  plot(
    obj$dims$x,
    obj$dims$y,
    type='n',
    ann=F,
    frame.plot = FALSE,
    axes = FALSE,
    ...
  )
}

#' @title plot_obj_fun
#' @param obj list containing list of parts
#' @param fun function string to call one of c('points','lines','polygons')
#' @param index of which part to plot if NULL all will be printed
#' @param sleep pause between plotting parts (E.g. use for animation timing)
#' @param alpha transparency
#' @seed random seed for color generation
#' @param ... passed to fun
#' @export
#' @details plot all parts based on supplied function c('points','lines','polygons')
plot_obj_fun<-function(obj,fun='points',index=NULL,sleep=.1,seed=123,...){

  if (is.null(index)){

    index<-1:length(obj$parts)

  }

  for(i in index){

    tmp<-obj$parts[[i]]

    col <- obj$colors[[i]]

    do.call(fun,list(
      tmp$x,
      tmp$y,
      pch = 21,
      col = col,
      ...
    ))

    Sys.sleep(sleep)

  }

}
