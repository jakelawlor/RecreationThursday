# Recreation Thursday Week 2

# Replicating Edna Andrade's Twilight Wave
# see tweet: https://twitter.com/Mayacelium/status/1405503706653597698


# I am going to do this with a hacky set of geom_ribbons, i think.



# test making a hacky half-circle wave. I think I'm going to do this by creating code for circles
# make a hacky circles curve 

# make df of circle centers
df <- data.frame(x=c(1,5),y=c(0,0),group=c(1,2))
diag <- .707
#
df %>%
  # separate each circle into 8 points
  slice(rep(1:n(), each=8)) %>%
  
  # make two anchors for each flower pedal
  mutate(circles_x =   x + c(-1, -diag,  0,  diag,  1, diag, 0, -diag ),
         circles_y =   y + c(0, diag,  1,  diag,  0, -diag, -1, -diag )) %>%
  
  # if y is positive, add 1 to x (shift the top half of the circle over by one diameter)
  mutate(circles_x_shifted = case_when(circles_y > 0 ~ (circles_x+2),
                                       TRUE ~ circles_x))  %>%
  
  # rearrange in order of x 
  arrange(circles_x_shifted) %>%
  
  # test plot
  ggplot(aes(x=circles_x_shifted,y=circles_y, color=group, group=group)) +
  geom_point() +
  coord_equal() +
  geom_bspline()

# nice! ok, we made a curve.



# Make a long curve across a page  ----------------------------------------
# make a curve with ~ 4 peaks
rm(df)
df <- data.frame(x=seq(from=0, to=16, by=4),y=0) %>%
  mutate(group = c(1:nrow(.)))


df %>%
  # separate each circle into 8 points
  slice(rep(1:n(), each=8)) %>%
  
  # make two anchors for each flower pedal
  mutate(circles_x =   x + c(-1, -diag,  0,  diag,  1, diag, 0, -diag ),
         circles_y =   y + c(0, diag,  1,  diag,  0, -diag, -1, -diag )) %>%
  
  # if y is positive, add 1 to x (shift the top half of the circle over by one diameter)
  mutate(circles_x_shifted = case_when(circles_y > 0 ~ (circles_x+2),
                                       TRUE ~ circles_x))  %>%
  
  # rearrange in order of x 
  arrange(circles_x_shifted) %>%
  
  # test plot
  ggplot(aes(x=circles_x_shifted,y=circles_y, color=group)) +
  geom_point() +
  coord_equal() +
  geom_bspline()

  


