## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  echo = TRUE,
  message = FALSE,
  warning = FALSE,
  fig.width = 7,
  fig.height = 7
)

## ---- echo=FALSE--------------------------------------------------------------
library(emo)
library(gifski)
library(knitr)

part_n<-10
group_n<-2
seed<-777
iter<-3
increment<-.5
alpha<-.5



## -----------------------------------------------------------------------------
library(randomviz)

parts<-obj<-make_parts(part_n,group_n,seed = seed) %>%
  get_centers()  %>%
  obj_dims() %>%
  get_colors(alpha=alpha,seed=seed)

str(obj)


## -----------------------------------------------------------------------------
init_plot(obj)
plot_obj_fun(obj,fun='points',lwd=4,)

## -----------------------------------------------------------------------------
init_plot(obj)
plot_obj_fun(obj,fun='lines',lwd=4)

## -----------------------------------------------------------------------------
init_plot(obj)
plot_obj_fun(obj,fun='polygon',lwd=4,border=NA)

## -----------------------------------------------------------------------------
init_plot(obj)
plot_obj_fun(obj,fun='lines',lwd=4)
plot_obj_fun(obj,fun='polygon',lwd=4,border=NA)
plot_obj_fun(obj,fun='points',lwd=4,)

## -----------------------------------------------------------------------------
obj <- parts %>%
points_linear_attractor(.,
                        attract = T,
                        increment = increment,
                        iter = iter) %>%
obj_dims() %>%
get_colors(alpha=alpha,seed=seed)


init_plot(obj)
plot_obj_fun(obj,fun='polygon',lwd=4,alpha=.5,border=NA)
plot_obj_fun(obj,fun='points',lwd=4,alpha=.5)
plot_obj_fun(obj,fun='lines',lwd=4,alpha=.5)


## -----------------------------------------------------------------------------
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

## -----------------------------------------------------------------------------
angles<-seq(0,360, by=90)
obj<- parts %>%
  rotate_obj_angles(.,angles) %>%
  obj_dims() %>%
  get_colors(alpha=alpha,seed=seed)


plot_fun<-function(index){
      init_plot(obj)
      for(i in 1:index){
        plot_obj_fun(obj,index=i,fun='polygon',cex=1,border=NA)
        plot_obj_fun(obj,index=i,fun='points',cex=1)
        plot_obj_fun(obj,index=i,fun='lines', lwd=1)
      }
    }

f<-function(parts){

      for(i in 1:length(obj$parts)){

        plot_fun(i)
      }

}

## ----cache=TRUE,eval=FALSE----------------------------------------------------
#  
#  invisible(save_gif(f(), "imgs/plot1.gif", delay = 0.3, progress = FALSE))
#  

## -----------------------------------------------------------------------------
include_graphics("imgs/plot1.gif")  

## -----------------------------------------------------------------------------
part_n<-250
group_n<-25
seed<-777
iter<-5
increment<-.5
alpha<-.8
zoom<-.7

obj<-make_parts(part_n,group_n,seed = seed) %>%
  get_centers()  %>%
  obj_dims(zoom=zoom) %>%
  get_colors(alpha=alpha,seed=seed)

plot_fun<-function(index){
  init_plot(obj,bg='black')
  for(i in 1:index){
    plot_obj_fun(obj,index=i,fun='lines',cex=1,lwd=4)
  }
}
  

## ----eval=FALSE---------------------------------------------------------------
#  invisible(save_gif(f(obj), gif_file= "imgs/plot2.gif", delay = 0.3, bg='black'))

## -----------------------------------------------------------------------------
include_graphics("imgs/plot2.gif")  

## ----cache=TRUE,eval=FALSE----------------------------------------------------
#  
#  plot_fun<-function(index){
#    init_plot(obj,bg='black')
#    for(i in 1:index){
#      plot_obj_fun(obj,index=i,fun='polygon')
#      plot_obj_fun(obj,index=i,fun='lines',cex=1,lwd=4)
#    }
#  }
#  
#  invisible(save_gif(f(obj), gif_file= "imgs/plot3.gif", delay = 0.3, bg='black'))
#  

## -----------------------------------------------------------------------------
include_graphics("imgs/plot3.gif")  

## ----eval=FALSE---------------------------------------------------------------
#  obj<-obj %>% rotate_obj_angles(.,angles) %>%
#        obj_dims(zoom=.7) %>%
#        get_colors(alpha=alpha,seed=seed)
#  
#  
#  invisible(save_gif(f(obj), gif_file= "imgs/plot4.gif", delay = 0.2, bg='black'))
#  

## -----------------------------------------------------------------------------
include_graphics("imgs/plot4.gif")  

