# week 3: Sol Lewitt, Color Bands;
# see tweet here: https://twitter.com/Jake_Lawlor1/status/1410665757772443651/photo/1



# For now, I'll try to recreate pane 3


# Libraries 
library(tidyverse)
library(ggforce)
library(patchwork)
library(ragg)




# set themes --------------------------------------------------------------
theme_set(theme_void())
theme_update(
  plot.margin = margin(15,15,15,15,unit="pt")
)


# Make colors and assignment function -------------------------------------


# add colors radonly but without repeat
colorpal <- c("#37b73a", # green
           "#e90d19", # red
           "#5099e6", # blue
           "#fc8012", # orange
           "#faeb05", # yellow
           "#834ea1" # purple
          ) 

# create a function to assign random colors from the palette
# not repeating, and also not repeating with only one line between
randomcols <- function(df){
  colorvect <- c()
  for(i in 1:nrow(df)){
    colorvect[i] <- sample(colorpal,1)
  # if same as previous, try again
  if (i > 1){
    while (colorvect[i] == colorvect[i-1]){
      colorvect[i] <- sample(colorpal,1)
    }
  }
    if (i > 2){
      while (colorvect[i] == colorvect[i-2] | colorvect[i] == colorvect[i-1] ){
        colorvect[i] <- sample(colorpal,1)
      }
    }
    if (i > 3){
      while (colorvect[i] == colorvect[i-3] | colorvect[i] == colorvect[i-2] | colorvect[i] == colorvect[i-1] ){
        colorvect[i] <- sample(colorpal,1)
      }
    }
    
  }
  return(colorvect)
}


# Panel 1 -----------------------------------------------------------------

## Lets make some data
plot1_arcs <- data.frame(
  start = -pi/2,
  end = pi/2,
  r = seq(from=0, to=10, by = .25),
  x0 = 0,
  y0 = 0,
  color=NA)
plot1_arcs$color <- randomcols(plot1_arcs)

plot1_left <- data.frame(
  x = 0,
  y =  seq(from=0, to=10, length.out=30),
  xend = c(5),
  yend = seq(from = 5, to = 15, length.out=30)
)
plot1_left$color <- randomcols(plot1_left)

plot1_right <- data.frame(
  x = seq(from=5, to=10, length.out=22),
  y =  10,
  xend = seq(from=5, to=10, length.out=22),
  yend = seq(from=5, to=0, length.out=22)
)
plot1_right$color <- randomcols(plot1_right)

plot1_outlines <- data.frame(
  x = c(0, 5, 5),
  y = c(0, 10, 5),
  xend = c(5, 5, 10),
  yend = c(5, 5, 0)
)

allplots_box <- geom_rect(
 # data = c(xmin = 0,
 #          ymin= 0,
 #          xmax = 10,
 #          ymax = 10),
  aes(xmin = 0,
        ymin= 0,
        xmax = 10,
        ymax = 10),
  color = "black",
  size=4,
  fill="transparent"
)
  
plot1_linesize <- 2.5
  
### start plot ----
plot1 <- ggplot() +
  geom_arc(
    data = plot1_arcs,
    aes(x0 = 5, 
        y0 = 5, 
        r = -r, 
        start = start,
        end = end,
        color=I(color)),
    size=plot1_linesize)+
  
  geom_segment(data = plot1_left,
               aes(x=x,
                   y=y,
                   xend=xend,
                   yend=yend,
                   color=I(color)),
               size=plot1_linesize)+
  
  geom_segment(data = plot1_right,
               aes(x=x,
                   y=y,
                   xend=xend,
                   yend=yend,
                   color=I(color)),
               size=plot1_linesize)+
  
  geom_segment(data = plot1_outlines,
               aes(x=x,
                   y=y,
                   xend=xend,
                   yend=yend),
               color = "black",
               size=2.2)+
  
  allplots_box +
  
  
  coord_equal(xlim = c(0,10),
              ylim = c(0,10),
              expand = F) 

#plot1




# Panel 2 -----------------------------------------------------------------

## Lets make some data
plot2_rightarcs <- data.frame(
  start = pi/2,
  end = 0,
  r = seq(from=0, to=7, length.out=28),
  x0 = 10,
  y0 = 10,
  color=NA)
plot2_rightarcs$color <- randomcols(plot2_rightarcs) 

## Lets make some data
plot2_leftarcs <- data.frame(
  start = pi,
  end = pi/2,
  r = seq(from=0, to=7, length.out=28),
  x0 = 5,
  y0 = 0,
  color=NA)
plot2_leftarcs$color <- randomcols(plot2_leftarcs) 



plot2_leftlines <- data.frame(
  x = seq(from=-5, to=5, length.out=36),
  y =  5,
  xend = c(5),
  yend = rev(seq(from=5, to=15, length.out=36))
)
plot2_leftlines$color <- randomcols(plot2_leftlines)

plot2_rightlines <- data.frame(
  x = 5,
  y =   seq(from=0, to=5, length.out=22),
  xend =10,
  yend =   seq(from=0, to=5, length.out=22)
)
plot2_rightlines$color <- randomcols(plot2_rightlines)

plot2_outlines <- data.frame(
  x = c(0, 5),
  y = c(5, 0),
  xend = c(10, 5),
  yend = c(5, 10)
)

plot2_linesize <- 2.5

### start plot -----
plot2 <- ggplot() +
  geom_arc(
    data = plot2_rightarcs,
    aes(x0 = x0, 
        y0 = y0, 
        r = -r, 
        start = start,
        end = end,
        color=I(color)),
    size=plot2_linesize)+
  
  geom_arc(
    data = plot2_leftarcs,
    aes(x0 = x0, 
        y0 = y0, 
        r = -r, 
        start = start,
        end = end,
        color=I(color)),
    size=plot2_linesize)+
  
  geom_segment(data = plot2_leftlines,
               aes(x=x,
                   y=y,
                   xend=xend,
                   yend=yend,
                   color=I(color)),
               size=plot2_linesize)+
  
  geom_segment(data = plot2_rightlines,
               aes(x=x,
                   y=y,
                   xend=xend,
                   yend=yend,
                   color=I(color)),
               size=plot2_linesize)+
  
  geom_segment(data = plot2_outlines,
               aes(x=x,
                   y=y,
                   xend=xend,
                   yend=yend),
               color = "black",
               size=2.2)+

  allplots_box +
  
  coord_equal(xlim = c(0,10),
              ylim = c(0,10),
              expand = F) 

#plot2




# panel 3 ------------------------------------------------------------------



## data right arcs
plot3_rightarcs <- data.frame(
  start = pi/2,
  end = 0,
  r = seq(from=0, to=8, length.out=38),
  x0 = 10,
  y0 = 10,
  color=NA)
plot3_rightarcs$color <- randomcols(plot3_rightarcs) 
plot3_rightarcs$color[nrow(plot3_rightarcs)] <- "black"


## data left arcs
plot3_leftarcs <- data.frame(
  start = -pi/2,
  end = 0,
  r = seq(from=0, to=8, length.out=38),
  x0 = 0,
  y0 = 10,
  color=NA)
plot3_leftarcs$color <- randomcols(plot3_leftarcs) 
plot3_leftarcs$color[nrow(plot3_leftarcs)] <- "black"


## data bottom arcs
plot3_bottomarcs <- data.frame(
  start = pi/2,
  end = pi + pi/2,
  r = seq(from=0, to=8, length.out=38),
  x0 = 5,
  y0 = 0,
  color=NA)
plot3_bottomarcs$color <- randomcols(plot3_bottomarcs) 
plot3_bottomarcs$color[nrow(plot3_bottomarcs)] <- "black"

plot3_linesize <- 2.2

### start plot----
plot3 <- ggplot() +
  
  # bottom arcs
  geom_arc(
    data = plot3_bottomarcs,
    aes(x0 = x0, 
        y0 = y0, 
        r = -r, 
        start = start,
        end = end,
        color=I(color)),
    size=plot3_linesize)+
  
  # left arcs
  geom_arc(
    data = plot3_leftarcs,
    aes(x0 = x0, 
        y0 = y0, 
        r = -r, 
        start = start,
        end = end,
        color=I(color)),
    size=plot3_linesize) +
  
  # right arcs
  geom_arc(
    data = plot3_rightarcs,
    aes(x0 = x0, 
        y0 = y0, 
        r = -r, 
        start = start,
        end = end,
        color=I(color)),
    size=plot3_linesize)+

  allplots_box +
  
  coord_equal(xlim = c(0,10),
              ylim = c(0,10),
              expand = F) 

#plot3





# Panel 4 -----------------------------------------------------------------

## data right arcs
plot4_rightarcs <- data.frame(
  start = -pi,
  end = 0,
  r = seq(from=0, to=8, length.out=34),
  x0 = 5,
  y0 = 5,
  color=NA)
plot4_rightarcs$color <- randomcols(plot4_rightarcs) 


## data top left
plot4_topleft <- data.frame(
  start = -pi/2,
  end = 0,
  r = seq(from=0, to=7, length.out=30),
  x0 = 0,
  y0 = 10,
  color=NA)
plot4_topleft$color <- randomcols(plot4_topleft) 

## data bottomleft
plot4_bottomleft <- data.frame(
  start = c(rep(pi,times=21),
            c(rep(pi, times=9) - c(.14,.34,.45,.53,.6,.65,.7,.73,.79))),
  end = pi/2,
  r = seq(from=0, to=7, length.out=30),
  x0 = 5,
  y0 = 0,
  color=NA)
plot4_bottomleft$color <- randomcols(plot4_bottomleft) 


plot4_outlines <- data.frame(
  x = c(0, 5),
  y = c(5, 0),
  xend = c(5, 5),
  yend = c(5, 10)
)



plot4_linesize <- 2.5

### start plot ----
plot4 <-  ggplot() +
  geom_arc(
    data = plot4_topleft,
    aes(x0 = x0, 
        y0 = y0, 
        r = -r, 
        start = start,
        end = end,
        color=I(color)),
    size=plot4_linesize) +
  
  
  
  geom_arc(
    data = plot4_rightarcs,
    aes(x0 = x0, 
        y0 = y0, 
        r = -r, 
        start = start,
        end = end,
        color=I(color)),
    size=plot4_linesize) +
  
  geom_arc(
    data = plot4_bottomleft,
    aes(x0 = x0, 
        y0 = y0, 
        r = -r, 
        start = start,
        end = end,
        color=I(color)),
    size=plot4_linesize) +
  

  geom_segment(data = plot4_outlines,
               aes(x=x,
                   y=y,
                   xend=xend,
                   yend=yend),
               color = "black",
               size=2.2)+
  
  allplots_box +
  
  coord_equal(xlim = c(0,10),
              ylim = c(0,10),
              expand = F) 

#plot4


# Panel 5 -----------------------------------------------------------------

## data left arcs
plot5_bottomleft <- data.frame(
  start = pi/2,
  end = 0,
  r = seq(from=0, to=8, length.out=34),
  x0 = 5,
  y0 = 6,
  color=NA)
plot5_bottomleft$color <- randomcols(plot5_bottomleft) 

## data bottom right arcs
plot5_bottomright <- data.frame(
  start = pi + pi/2,
  end = pi,
  r = seq(from=0, to=8, length.out=34),
  x0 = 5,
  y0 = 0,
  color=NA)
plot5_bottomright$color <- randomcols(plot5_bottomright) 

## data for top lines
plot5_top <- data.frame(
  x = seq(from = 0, to = 10, length.out=45),
  xend = seq(from = 0, to = 10, length.out=45), 
  y = 10,
  yend = 6)
plot5_top$color <- randomcols(plot5_top) 




plot5_outlines <- data.frame(
  x = c(0, 5),
  y = c(6, 0),
  xend = c(10, 5),
  yend = c(6, 6)
)



plot5_linesize <- 2.2

### start plot ----
plot5 <- ggplot() +
   geom_arc(
     data = plot5_bottomleft,
     aes(x0 = x0, 
         y0 = y0, 
         r = -r, 
         start = start,
         end = end,
         color=I(color)),
     size=plot5_linesize*1.05) +
  
  
  geom_arc(
    data = plot5_bottomright,
    aes(x0 = x0, 
        y0 = y0, 
        r = -r, 
        start = start,
        end = end,
        color=I(color)),
    size=plot5_linesize*1.05) +
  
  
  geom_segment(
    data = plot5_top,
    aes(x = x,
        y = y,
        xend = xend,
        yend=yend,
        color=I(color)),
    size=plot5_linesize) +
  

  
  geom_segment(data = plot5_outlines,
               aes(x=x,
                   y=y,
                   xend=xend,
                   yend=yend),
               color = "black",
               size=2.2) +
  
  allplots_box +
  
  coord_equal(xlim = c(0,10),
              ylim = c(0,10),
              expand = F) 

#plot5







# Panel 6 -----------------------------------------------------------------
n_triangles <- 36
triangles_topleft <- data.frame(
  x = seq(from=4.5, to = -6, length.out=n_triangles),
  y = seq(from=7, to = 14, length.out=n_triangles),
  group = c(1:n_triangles)
)

triangles_topright <- data.frame(
  x = seq(from=5.5, to = 16, length.out=n_triangles),
  y = seq(from=7, to = 14, length.out=n_triangles),
  group = c(1:n_triangles)
)

triangles_bottom <- data.frame(
  x = 5,
  y = seq(from=6, to = -10, length.out=n_triangles),
  group = c(1:n_triangles)
)



## data triangles
plot6_triangles <- data.frame(
  x = c(t(triangles_topleft)[1,],
        t(triangles_topright)[1,],
        t(triangles_bottom)[1,],
        t(triangles_topleft)[1,]),
  y = c(t(triangles_topleft)[2,],
        t(triangles_topright)[2,],
        t(triangles_bottom)[2,],
        t(triangles_topleft)[2,]),
  group = c(t(triangles_topleft)[3,],
        t(triangles_topright)[3,],
        t(triangles_bottom)[3,],
        t(triangles_topleft)[3,])
)
plot6_triangles$color <- randomcols(plot6_triangles[1:n_triangles,])


plot6_size <- 2.2

### start plot ----
plot6 <- ggplot() +
  
  geom_path(data = plot6_triangles,
               aes(x=x,y=y,
                   group=factor(group),
                   color=I(color)),
               size=plot6_size,
            lineend = "square",
            linejoin = "mitre") + 
  
 #geom_polygon(data = plot6_triangles,
 #             aes(x=x,y=y,
 #                 group=factor(group),
 #                 color=I(color)),
 #                 fill="transparent",
 #             size=plot6_size) + 

  allplots_box +
  
  coord_equal(xlim = c(0,10),
              ylim = c(0,10),
              expand = F) +
  
  theme(panel.background = element_rect(fill = "#F0F048"))

#plot6



# Panel 7 -----------------------------------------------------------------

plot7_leftlines <- data.frame(
  x= 0,
  xend = 3.3,
  y = seq(from=-3, to=10, length.out=46),
  yend = seq(from = 0, to = 13, length.out=46)
)
plot7_leftlines$color <- randomcols(plot7_leftlines) 

plot7_midlines <- data.frame(
  x= seq(from = 3.3, to = 6.6, length.out=16),
  xend = seq(from = 3.3, to = 6.6, length.out=16),
  y = 0,
  yend = 10
)
plot7_midlines$color <- randomcols(plot7_midlines) 


## data bottomright
plot7_bottomright <- data.frame(
 start = pi + pi/2,
  end = pi,
  r = seq(from=0, to=6, length.out=30),
  x0 = 6.67,
  y0 = 0,
  color=NA)
plot7_bottomright$color <- randomcols(plot7_bottomright) 

## data topright
plot7_topright <- data.frame(
 end = c(rep(0,times=25),
              c(rep(0, times=5) + c(.3,.4,.49,.55,.6))),
  start = pi/2,
#  end = 0,
  r = seq(from=0, to=6, length.out=30),
  x0 = 10,
  y0 = 10,
  color=NA)
plot7_topright$color <- randomcols(plot7_topright) 



plot7_outlines <- 
  data.frame(
    x = c(3.3, 6.6, 6.6),
    xend = c(3.3, 6.6, 10),
    y = c(0, 0, 5),
    yend = c(10, 10, 5)
  )


plot7_size <- 2.2

### start plot ----
plot7 <-  ggplot() +
  
  # diagonal left lines
  geom_segment(data = plot7_leftlines,
               aes(x=x,
                   y=y,
                   xend=xend,
                   yend=yend,
                   color = I(color)),
               size=plot7_size) +

  geom_arc(
    data = plot7_bottomright,
    aes(x0 = x0, 
        y0 = y0, 
        r = -r, 
        start = start,
        end = end,
        color=I(color)),
    size=plot7_size) +
  
  
  geom_arc(
    data = plot7_topright,
    aes(x0 = x0, 
        y0 = y0, 
        r = -r, 
        start = start,
        end = end,
        color=I(color)),
    size=plot7_size) +
  
  
  # middle lines
  geom_segment(data = plot7_midlines,
               aes(x=x,
                   y=y,
                   xend=xend,
                   yend=yend,
                   color = I(color)),
               size=plot7_size) +
  
  
  geom_segment(data = plot7_outlines,
               aes(x=x,
                   y=y,
                   xend=xend,
                   yend=yend),
               color = "black",
               size=2.2) +
  
  allplots_box +
  
  coord_equal(xlim = c(0,10),
              ylim = c(0,10),
              expand = F) 
  
#plot7


# Panel 8 -----------------------------------------------------------------

# data horizontal lines
plot8_leftlines <- data.frame(
  x= 0,
  xend = 3.3,
  y = seq(from=0, to=10, length.out=45),
  yend = seq(from=0, to=10, length.out=45)
)
plot8_leftlines$color <- randomcols(plot8_leftlines) 

# data vertical lines
plot8_topright <- data.frame(
  x= seq(from = 3.33, to = 10, length.out=34),
  xend = seq(from = 3.33, to = 10, length.out=34),
  y = 4,
  yend = 10
)
plot8_topright$color <- randomcols(plot8_topright) 


## data bottomright arcs
plot8_bottomright <- data.frame(
  start = pi + pi/2,
  end = pi,
  r = seq(from=0, to=8, length.out=38),
  x0 = 3.33,
  y0 = 0,
  color=NA)
plot8_bottomright$color <- randomcols(plot8_bottomright) 



plot8_outlines <- 
  data.frame(
    x = c(3.3, 3.3),
    xend = c(3.3, 10),
    y = c(0, 4),
    yend = c(10, 4)
  )


plot8_size <- 2.2

### start plot ----
plot8 <- ggplot() +
  
  # bottom right arcs
  geom_arc(
    data = plot8_bottomright,
    aes(x0 = x0, 
        y0 = y0, 
        r = -r, 
        start = start,
        end = end,
        color=I(color)),
    size=plot8_size) +
  
  # horizontal left lines
  geom_segment(data = plot8_leftlines,
               aes(x=x,
                   y=y,
                   xend=xend,
                   yend=yend,
                   color = I(color)),
               size=plot8_size) +
  
  # vertical right lines
  geom_segment(data = plot8_topright,
               aes(x=x,
                   y=y,
                   xend=xend,
                   yend=yend,
                   color = I(color)),
               size=plot8_size) +

  
  
  geom_segment(data = plot8_outlines,
               aes(x=x,
                   y=y,
                   xend=xend,
                   yend=yend),
               color = "black",
               size=2.2) +
  
  allplots_box +
  
  coord_equal(xlim = c(0,10),
              ylim = c(0,10),
              expand = F) 

#plot8



# merge and save ---------------------------------------------------------
layout <- "
ABCD
EFGH
"
plots_arranged <- plot1 + plot2 + plot3 + plot4  +
  plot5 + plot6 + plot7 + plot8 +
  plot_layout(design = layout)

plots_arranged



# save plot ---------------------------------------------------------------

ggsave(plots_arranged,
       file = here::here("Week3_Lewitt","plot.png"), # save as plot 5001, 5002, etc, so they end up in the right order
       width = 14, 
       height = 7, 
       unit = "in",
       dpi=300,
       device = cairo_pdf())



