

test_<-function(){

  library(randomviz)

  part_n<-10
  group_n<-2
  seed<-777
  iter<-3
  increment<-.5
  alpha<-.5

  parts<-make_parts(part_n,group_n,seed = seed) %>%
    get_centers()  %>%
    obj_dims() %>%
    get_colors(alpha=alpha,seed=seed)

  obj<-parts

  init_plot(obj)
  plot_obj_fun(obj,fun='polygon',lwd=4,border=NA)
  plot_obj_fun(obj,fun='points',lwd=4,)
  plot_obj_fun(obj,fun='lines',lwd=4)


  #linear attractor
  obj <- parts %>%
    points_linear_attractor(.,
                            attract = F,
                            increment = increment,
                            iter = iter) %>%
    obj_dims() %>%
    get_colors(alpha=alpha,seed=seed)


    init_plot(obj)
    plot_obj_fun(obj,fun='polygon',lwd=4,alpha=.5,border=NA)
    plot_obj_fun(obj,fun='points',lwd=4,alpha=.5)
    plot_obj_fun(obj,fun='lines',lwd=4,alpha=.5)

  #rotate parts
  angles<-seq(0,360, by=90)
  obj<- parts %>%
    rotate_obj_angles(.,angles) %>%
    obj_dims() %>%
    get_colors(alpha=alpha,seed=seed)

  init_plot(obj)
  for(i in 1:length(obj$parts)){
    plot_obj_fun(obj,index=i,fun='polygon',cex=1,border=NA)
    plot_obj_fun(obj,index=i,fun='points',cex=1)
    plot_obj_fun(obj,index=i,fun='lines', lwd=1)
  }

  # animations --------------------------------------------------------------

  animations<-function(){

    #seems only works when a new plot is called not when parts are added

    #animate
    # install.packages('animation')
    # install.packages('gifski')
    library(gifski)
    # library(animation)


    #need re plot all old layers when adding a new one for w gif

    plot_fun<-function(index){
      init_plot(obj)
      for(i in 1:index){
        plot_obj_fun(obj,index=i,fun='polygon',cex=1,border=NA)
        plot_obj_fun(obj,index=i,fun='points',cex=1)
        plot_obj_fun(obj,index=i,fun='lines', lwd=1)
      }
    }

    f<-function(obj){
      par(bg='black')
      for(i in 1:length(obj$parts)){

        plot_fun(i)
      }

    }

    save_gif(f(), gif_file= "./vignettes/imgs/plot.gif", 1280, 720, res = 144)
    utils::browseURL("./vignettes/imgs/plot.gif")

    #line plot with variable zoom
    plot_fun<-function(index){
      init_plot(obj,bg='black')
      for(i in 1:index){
        plot_obj_fun(obj,index=i,fun='lines',cex=1,lwd=4,bg='black')
      }
    }


    part_n<-100
    group_n<-10
    seed<-777
    iter<-5
    increment<-.5
    alpha<-.8
    zoom<-.7

    obj<-make_parts(part_n,group_n,seed = seed) %>%
      get_centers()  %>%
      obj_dims(zoom=zoom) %>%
      get_colors(alpha=alpha,seed=seed)

    save_gif(f(obj), gif_file= "./vignettes/imgs/plot2.gif", delay = 0.3, bg='black')



# rotations ---------------------------------------------------------------

    obj<-obj %>% rotate_obj_angles(.,angles) %>%
      obj_dims(zoom=.6) %>%
      get_colors(alpha=alpha,seed=seed)

    plot_fun<-function(index){
      init_plot(obj,bg='black')
      for(i in 1:index){
        plot_obj_fun(obj,index=i,fun='polygon')
        plot_obj_fun(obj,index=i,fun='lines',cex=1,lwd=4)
      }
    }

    invisible(save_gif(f(obj), gif_file= "vignettes/imgs/plot4.gif", delay = 0.4, bg='black'))


  }

  #iterative rotations
  angles<-seq(0,360, by=30)
  speed_up<-T
  obj<-points_linear_attractor(parts,attract=F,increment = .5,iter=5) %>%
    obj_dims()
  init_plot(obj)
  for (i in 1:length(angles)){
    # init_plot(obj)
    if(speed_up){
      obj$parts<-list(do.call('rbind',obj$parts))
    }

    .obj<-rotate_obj(obj,angles[i]) %>% obj_dims()
    # plot_obj_fun(.obj,type='lines',lwd=1,alpha=.5,sleep=.5)
    plot_obj_fun(.obj,fun='polygon',cex=1,border=NA)
    Sys.sleep(1)

  }




  #one object rotation
  angles<-seq(0,360, by=30)
  speed_up<-F
  obj<-points_linear_attractor(parts,attract=F,increment = .5,iter=1) %>%
    obj_dims()

  #simple test
  obj<-make_parts(50,1) %>%
    get_center() %>%
    obj_dims()


  init_plot(obj)
  for (i in 1:length(angles)){
    # init_plot(obj)
    if(speed_up){
      obj$parts<-list(do.call('rbind',obj$parts))
    }

    .obj<-rotate_obj(obj,angles[i]) %>% obj_dims()
    # plot_obj_fun(.obj,type='lines',lwd=1,alpha=.5,sleep=.5)
    plot_obj_fun(.obj,fun='polygon',cex=1,border=NA)
    # plot_obj_fun(.obj,fun='lines',cex=1,border=NA)
    # plot_obj_fun(.obj,fun='points',cex=.5,border=NA)
    Sys.sleep(.25)

  }

  #simple test
  obj<-make_parts(50,1) %>%
    get_center() %>%
    obj_dims()


  #calculate all parts rotation
  res<-list()
  #calculate
  for (i in 1:length(angles)){

    res[[i]]<-rotate_obj(obj,angles[i])$parts

  }

  obj$parts<-res  %>%  flatten()
  #dims
  obj<-obj %>%  obj_dims()



  #render each part
  x11() # to show each iteretaion
  .obj<-obj %>%  obj_dims(zoom=.5)

  plot_obj_fun(.obj,fun='lines',cex=1,border=NA, sleep=.1,alpha=1, lwd=3)
  plot_obj_fun(.obj,fun='points',cex=1,border=NA, sleep=.1,alpha=1, lwd=3)

  #movie
  #-----
  # points
  #lines
  #polygons
  #rotate
  #attract

  #interleave ploters
  x11() # to show each iteration
  #simple test
  seed = 1
  obj<-make_parts(50,1) %>%
    get_center() %>%
    obj_dims()

  angles<-seq(0,360, by=30)
  obj<- rotate_obj_angles(obj,angles)
  .obj<-obj %>%  obj_dims(zoom=.5)
  init_plot(.obj)
  for(i in 1:length(.obj$parts)){
    .seed<-seed+i

    plot_obj_fun(.obj,index=i,fun='polygon',cex=1,border=NA, sleep=.1,alpha=.5, lwd=3,seed=.seed)
    plot_obj_fun(.obj,index=i,fun='points',cex=1,border=NA, sleep=.1,alpha=1, lwd=3,seed=.seed)
    plot_obj_fun(.obj,index=i,fun='lines',cex=1,border=NA, sleep=.1,alpha=1, lwd=1,seed=.seed)
  }

  #change zoom in sequence

  #convert to polar coordinates
  toPolar = function(x, y){
    t1 = atan2(y,x)
    rP = sqrt(x^2+y^2)
    return(c(t1 = t1,rP = rP))
  }

  lapply(1:nrow(),function(i){toPolar(obj$parts[[1]][i,]$x,obj$parts[[1]][i,]$y)})



  # OLDER
  #-----------
  # library(purrr)
  #
  # #make random points and groups
  # make_parts<-function(part_num=10,groups=3){
  #   parts<-data.frame(x=rnorm(part_num),y=rnorm(part_num))
  #   parts$group<-sample(1:groups,part_num,replace=TRUE)
  #   parts
  # }
  #
  # #center
  # get_centers<-function(parts){
  #
  #   tmp<-split(parts, parts$group)
  #
  #   centers<-lapply(tmp, function(x){
  #
  #     data.frame(x=mean(x$x),y=mean(x$y),x %>%
  #                  select(-x,-y) %>% .[1, ,drop=FALSE])
  #
  #   })
  #
  #   list(parts=tmp,centers=centers)
  #
  # }
  #
  # #attract to point
  # get_linear_forces <- function(parts, center, attract=T,increment = .5) {
  #
  #   #move points to center at
  #   #increment of total distance
  #   diffx<-parts$x - center$x
  #   diffy<-parts$y - center$y
  #
  #   if(attract){
  #
  #     parts$x<-parts$x - diffx*increment
  #     parts$y<-parts$y - diffy*increment
  #
  #   } else {
  #
  #     parts$x<-parts$x + diffx*increment
  #     parts$y<-parts$y + diffy*increment
  #
  #   }
  #
  #   parts
  #
  # }
  #
  # #attract or repulse point from center along  aline
  # points_linear_attractor<-function(parts,iter=10,...){
  #
  #   results<-list()
  #   .parts<-lapply(1:length(parts$parts),function(i){
  #
  #     .parts<-new<-parts$parts[[i]]
  #     .center<-parts$centers[[i]]
  #
  #     .results<-list()
  #
  #     for(j in 1:iter){
  #
  #       .results[[j]]<-new<-get_linear_forces(new,.center,...)
  #     } %>%
  #       do.call('rbind',.)
  #
  #
  #     results[[i]]<-.results
  #
  #   })
  #
  #   .parts<-flatten(.parts)
  #
  #   list(parts=c(.parts,results) )
  # }
  #
  # #plot parts list with sleep
  # #get plot dims
  # obj_dims<-function(obj, zoom=NULL){
  #
  #   tmp<-obj$parts %>% do.call('rbind',.)
  #   dims<-list(x=range(tmp$x),y=range(tmp$y)) %>%
  #     data.frame()
  #
  #   obj$dims<-dims
  #
  #   if(!is.null(zoom)){
  #     obj$dims<-obj$dims - obj$dims*zoom
  #   }
  #
  #   obj
  #
  # }
  #
  # init_plot<-function(obj,...){
  #
  #   plot(
  #     obj$dims$x,
  #     obj$dims$y,
  #     type='n',
  #     ann=F,
  #     frame.plot = FALSE,
  #     axes = FALSE,
  #     ...
  #   )
  # }
  #
  # plot_obj_fun<-function(obj,sleep=.1,fun='points',alpha=.5,seed=123,...){
  #
  #
  #   for(i in 1:length(obj$parts)){
  #
  #     tmp<-obj$parts[[i]]
  #
  #     col <- get_color(tmp$group[1],alpha = alpha,seed=seed+i)
  #     do.call(fun,list(
  #       tmp$x,
  #       tmp$y,
  #       bg = tmp$group,
  #       pch = 21,
  #       col = col,
  #       ...
  #     ))
  #
  #     Sys.sleep(sleep)
  #
  #   }
  #
  # }
  #
  # plot_obj_fun_index<-function(obj,index=1,sleep=.1,fun='points',alpha=.5,seed=123,...){
  #
  #
  #   tmp<-obj$parts[[index]]
  #
  #   col <- get_color(tmp$group[1],alpha = alpha,seed=seed)
  #   do.call(fun,list(
  #     tmp$x,
  #     tmp$y,
  #     bg = tmp$group,
  #     pch = 21,
  #     col = col,
  #     ...
  #   ))
  #
  #   Sys.sleep(sleep)
  #
  # }
  #
  # #make color pallet
  # get_color<-function(id=1,random=TRUE,alpha=.5, seed=123){
  #
  #   set.seed(seed)
  #   if(random){
  #     .col<-colors()[sample(1:length(colors()),1)]
  #
  #   } else {
  #     .col<-colors()[id]
  #   }
  #
  #   adjustcolor(.col,alpha.f=alpha)
  #
  # }
  #
  # #rotate points
  # rotate_parts <- function(parts, angle, center, group) {
  #
  #   co <- cos(-angle * pi / 180)
  #   si <- sin(-angle * pi / 180)
  #   tmp<-list()
  #   tmp$x<-parts$x-center$x
  #   tmp$y<-parts$y-center$y
  #   .tmp<-list(x = co * tmp$x - si * tmp$y, y = si * tmp$x + co *tmp$y)
  #   .tmp$x<-.tmp$x - tmp$x
  #   .tmp$y<-.tmp$y - tmp$y
  #
  #   data.frame(x=.tmp$x, y=.tmp$y,group=group)
  # }
  #
  # rotate_obj<-function(obj,angle=45){
  #
  #   .parts<-lapply(1:length(obj$parts),function(i){
  #
  #     rotate_parts(obj$parts[[i]][,1:2,drop=F],angle,obj$centers[[i]][,1:2,drop=F],group = obj$centers[[i]]$group )
  #
  #   })
  #
  #   obj$parts<-.parts
  #   obj
  # }
  #
  # rotate_obj_angles<-function(obj,angles){
  #
  #   #calculate all parts rotation
  #   res<-list()
  #   #calculate
  #   for (i in 1:length(angles)){
  #
  #     res[[i]]<-rotate_obj(obj,angles[i])$parts
  #
  #   }
  #
  #   obj$parts<-res  %>%  flatten()
  #   #dims
  #   obj<-obj %>%  obj_dims()
  #
  #   obj
  #
  # }
}
