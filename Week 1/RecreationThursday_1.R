# Recreation Thursday Week 1 - Alfredo Hilto's Curves and Straight Series
# https://twitter.com/_natalie_oshea/status/1400526686848958466


library(ggplot2)
library(ragg)
library(here)


yellow = "#d9c757"
green = "#78a621"
red = "#dd4626"
blue = "#034095"
bgcol = "#ebe4d8"


# make a vector for movement, which will move my entire drawing by .5 
# in each image, and stitch them together in a gif.
movement <- c(seq(from=0, to=10.5, by=.5), rev(seq(from=.5,to=11, by=.5)))

# loop over the movement vector to save out one ggplot at each step
for(i in 1:length(movement)){
  ynudge = movement[i]
  xnudge = movement[i]
  
  
  
  
  {
    # set all parameters
    curves<- list()
    
    # left set of lines
    curves$lines_left <- list(x = 2 -xnudge,
                              y = c(2, 4.7, 5.0, 5.3, 6)-ynudge,
                              xend= 3-xnudge,
                              yend =  c(2, 4.7, 5.0, 5.3, 6)-ynudge,
                              color = c(red,"black",blue,red,green)
    )
    
    # yellow curve
    curves$yellowcurve <- list(x = 3-xnudge,
                               xend = 4-xnudge,
                               y = 3-ynudge, 
                               yend = 10 -ynudge,
                               color = yellow,
                               curvature = .4)
    
    # blue curve
    curves$bluecurve <- list(x =  5-xnudge,
                             xend = 5-xnudge,
                             y = 1.5-ynudge,
                             yend =  5-ynudge,
                             color = blue,
                             curvature =  -.4)
    # diagonal black line
    curves$blackline <- list(x =  4-xnudge,
                             xend = 7-xnudge,
                             y = 6-ynudge,
                             yend =  3-ynudge,
                             color = "black")
    
    # right set of lines
    curves$lines_right <- list(x = 7-xnudge,
                               y = c(5.5, 7.3, 7.6,7.9)-ynudge,
                               xend= 8-xnudge,
                               yend = c(5.5, 7.3, 7.6, 7.9)-ynudge,
                               color = c(red,green,blue,"black")
    )
  }
  
  
  #theme_set(theme_bw())
  
  # First test this with only one plot, i.e. recreate the exact painting
  ## start plot --------------------------------------------------------------
  #ggplot() +
  #  
  #  # add segments on left
  #  geom_segment(aes(
  #    x = curves$lines_left$x,
  #    xend = curves$lines_left$xend,
  #    y = curves$lines_left$y,
  #    yend = curves$lines_left$yend,
  #    color = I(curves$lines_left$color)
  #  ),
  #  size=1) +
  #  
  #  
  #  # add yellow curves
  #  geom_curve(aes(
  #    x = curves$yellowcurve$x,
  #    xend = curves$yellowcurve$xend,
  #    y = curves$yellowcurve$y,
  #    yend = curves$yellowcurve$yend,
  #    color = curves$yellowcurve$color,
  #    ),
  #    curvature = curves$yellowcurve$curvature, 
  #    size=1) +
  #  
  #  # add bluecurve
  #  geom_curve(aes(
  #    x = curves$bluecurve$x,
  #    xend = curves$bluecurve$xend,
  #    y = curves$bluecurve$y,
  #    yend = curves$bluecurve$yend,
  #    color = curves$bluecurve$color,
  #  ), 
  #  curvature = curves$bluecurve$curvature, 
  #  size=1) +
  #  
  #  # add blackline
  #  geom_segment(aes(
  #    x = curves$blackline$x,
  #    xend = curves$blackline$xend,
  #    y = curves$blackline$y,
  #    yend = curves$blackline$yend,
  #    color = curves$blackline$color,
  #  ), 
  #  size=1) +
  #  
  #  # add segments on right
  #  geom_segment(aes(
  #    x = curves$lines_right$x,
  #    xend = curves$lines_right$xend,
  #    y = curves$lines_right$y,
  #    yend = curves$lines_right$yend,
  #    color = I(curves$lines_right$color)
  #  ),
  #  size=1) +
  #  
  #  coord_equal(xlim = c(-5,5),
  #              ylim = c(0,10),
  #              expand = F)  +
  #  
  #  
  #  theme_void() +
  #  theme(panel.background = element_rect(fill = bgcol))
  ##------
  
  
  
  # Now try to replicate this plot 4 times, mirrored across the x and y axes
  # try rotating plot -------------------------------------------------------
  ggplot() +
    
    
    # first plot  - top right quadrant
    #---------------------------------
  
  # add segments on left
  geom_segment(aes(
    x = curves$lines_left$x,
    xend = curves$lines_left$xend,
    y = curves$lines_left$y,
    yend = curves$lines_left$yend,
    color = I(curves$lines_left$color)
  ),
  size=1) +
    
    
    # add yellow curves
    geom_curve(aes(
      x = curves$yellowcurve$x,
      xend = curves$yellowcurve$xend,
      y = curves$yellowcurve$y,
      yend = curves$yellowcurve$yend,
      color = curves$yellowcurve$color,
    ),
    curvature = curves$yellowcurve$curvature, 
    size=1) +
    
    # add bluecurve
    geom_curve(aes(
      x = curves$bluecurve$x,
      xend = curves$bluecurve$xend,
      y = curves$bluecurve$y,
      yend = curves$bluecurve$yend,
      color = curves$bluecurve$color,
    ), 
    curvature = curves$bluecurve$curvature, 
    size=1) +
    
    # add blackline
    geom_segment(aes(
      x = curves$blackline$x,
      xend = curves$blackline$xend,
      y = curves$blackline$y,
      yend = curves$blackline$yend,
      color = curves$blackline$color,
    ), 
    size=1) +
    
    # add segments on right
    geom_segment(aes(
      x = curves$lines_right$x,
      xend = curves$lines_right$xend,
      y = curves$lines_right$y,
      yend = curves$lines_right$yend,
      color = I(curves$lines_right$color)
    ),
    size=1) +
    
    
    # second plot -- mirror y
    # ----------------------------------------
  # turn all y coords negative
  
  
  # add segments on left
  geom_segment(aes(
    x = -curves$lines_left$x,
    xend = -curves$lines_left$xend,
    y = -curves$lines_left$y,
    yend = -curves$lines_left$yend,
    color = I(curves$lines_left$color)
  ),
  size=1) +
    
    
    # add yellow curves
    geom_curve(aes(
      x = -curves$yellowcurve$x,
      xend = -curves$yellowcurve$xend,
      y = -curves$yellowcurve$y,
      yend = -curves$yellowcurve$yend,
      color = curves$yellowcurve$color,
    ),
    curvature = curves$yellowcurve$curvature, 
    size=1) +
    
    # add bluecurve
    geom_curve(aes(
      x = -curves$bluecurve$x,
      xend = -curves$bluecurve$xend,
      y = -curves$bluecurve$y,
      yend = -curves$bluecurve$yend,
      color = curves$bluecurve$color,
    ), 
    curvature = curves$bluecurve$curvature, 
    size=1) +
    
    # add blackline
    geom_segment(aes(
      x = -curves$blackline$x,
      xend = -curves$blackline$xend,
      y = -curves$blackline$y,
      yend = -curves$blackline$yend,
      color = curves$blackline$color,
    ), 
    size=1) +
    
    # add segments on right
    geom_segment(aes(
      x = -curves$lines_right$x,
      xend = -curves$lines_right$xend,
      y = -curves$lines_right$y,
      yend = -curves$lines_right$yend,
      color = I(curves$lines_right$color)
    ),
    size=1) +
    
    
    # third plot -- mirror x and y
    # ----------------------------------------
  # flip y and x
  
  
  # add segments on left
  geom_segment(aes(
    y = -curves$lines_left$x,
    yend = -curves$lines_left$xend,
    x = curves$lines_left$y,
    xend = curves$lines_left$yend,
    color = I(curves$lines_left$color)
  ),
  size=1) +
    
    
    # add yellow curves
    geom_curve(aes(
      y = -curves$yellowcurve$x,
      yend = -curves$yellowcurve$xend,
      x = curves$yellowcurve$y,
      xend = curves$yellowcurve$yend,
      color = curves$yellowcurve$color,
    ),
    curvature = curves$yellowcurve$curvature, 
    size=1) +
    
    # add bluecurve
    geom_curve(aes(
      y = -curves$bluecurve$x,
      yend = -curves$bluecurve$xend,
      x = curves$bluecurve$y,
      xend = curves$bluecurve$yend,
      color = curves$bluecurve$color,
    ), 
    curvature = curves$bluecurve$curvature, 
    size=1) +
    
    # add blackline
    geom_segment(aes(
      y = -curves$blackline$x,
      yend = -curves$blackline$xend,
      x = curves$blackline$y,
      xend = curves$blackline$yend,
      color = curves$blackline$color,
    ), 
    size=1) +
    
    # add segments on right
    geom_segment(aes(
      y = -curves$lines_right$x,
      yend = -curves$lines_right$xend,
      x = curves$lines_right$y,
      xend = curves$lines_right$yend,
      color = I(curves$lines_right$color)
    ),
    size=1) +
    
    
    # fourth plot  - mirror x
    # ----------------------------------------
  # flip y and x
  
  
  # add segments on left
  geom_segment(aes(
    y = curves$lines_left$x,
    yend = curves$lines_left$xend,
    x = -curves$lines_left$y,
    xend = -curves$lines_left$yend,
    color = I(curves$lines_left$color)
  ),
  size=1) +
    
    
    # add yellow curves
    geom_curve(aes(
      y = curves$yellowcurve$x,
      yend = curves$yellowcurve$xend,
      x = -curves$yellowcurve$y,
      xend = -curves$yellowcurve$yend,
      color = curves$yellowcurve$color,
    ),
    curvature = curves$yellowcurve$curvature, 
    size=1) +
    
    # add bluecurve
    geom_curve(aes(
      y = curves$bluecurve$x,
      yend = curves$bluecurve$xend,
      x = -curves$bluecurve$y,
      xend = -curves$bluecurve$yend,
      color = curves$bluecurve$color,
    ), 
    curvature = curves$bluecurve$curvature, 
    size=1) +
    
    # add blackline
    geom_segment(aes(
      y = curves$blackline$x,
      yend = curves$blackline$xend,
      x = -curves$blackline$y,
      xend = -curves$blackline$yend,
      color = curves$blackline$color,
    ), 
    size=1) +
    
    # add segments on right
    geom_segment(aes(
      y = curves$lines_right$x,
      yend = curves$lines_right$xend,
      x = -curves$lines_right$y,
      xend = -curves$lines_right$yend,
      color = I(curves$lines_right$color)
    ),
    size=1) +
    
    
    # theme stuff
    # ----------------------------------------
  coord_equal(xlim = c(-10,10),
              ylim = c(-10,10),
              expand = F)  +
    
    
    theme_void() +
    theme(panel.background = element_rect(fill = bgcol)) -> plot
  
  
  ggsave(plot,
         file = here::here("Week 1",paste0("curves_",5000+i,".png")), # save as plot 5001, 5002, etc, so they end up in the right order
         width = 1000, height = 1000, res = 175,     
         limitsize = F,
         device = agg_png)
  
  # print each plot 
  print(i)
}


# stitch into gif and save
#--- --- --- --- --- --- --- --- --- ---
# do run this command, on mac - at least, you need to have imageMagik installed
# (on the computer, it's not an R package)
setwd(here::here("Week 1")) # can I do this without setting wd?? not sure
system(command = "convert -delay 20 *.png curves.gif")



# Part 5. Delete all the .pngs that made the gif
#--- --- --- --- --- --- --- --- --- ---
here::here()
for (i in 1:length(movement)){
  unlink(here::here("Week 1",paste0("curves_",5000+i,".png")))
}
