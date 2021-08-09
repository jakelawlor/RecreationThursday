# week 4 - Phantom's Shadow - https://www.stevenson.info/exhibition/3088/work/3


# libraries
# ----------------------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)
library(PNWColors)

# set colors
# ----------------------------------------------------
pal <- pnw_palette("Starfish")


# try to make just one square of the shapes first
polygon <- data.frame(
  #group = c(1,1,1,1,      2,2,2,2,       3,3,3,3,         4,4,4,4),
  x =     c(0,1,1,.33,  1,2,2,1.33,   1, 1.66, 2, 1,      0, 1, .66, 0  ),
  y =     c(0,0,1,.66,  1,1,0,.33,    1, 1.33, 2, 2,       1, 1, 1.66, 2)
)

polygons <- data.frame(group = rep (1:16, each=16))
polygons$shape <- rep(1:64, each= 4)
polygons$x = rep(polygon$x, times=16)
polygons$y = rep(polygon$y, times=16)


#set.seed(7)
set.seed(13)
#set.seed(19)
#set.seed(40)



# rotate and add colors ---------------------------------------------------
set.seed(21)
# good seeds: 5, 14, 20, 21, 29
polygonsdf <- polygons %>%
  
  # rotate some groups randomly
  group_by(group) %>%
  nest() %>%
  ungroup() %>%
  mutate(rotate = sample(c("y","n"), 16, replace = T)) %>%
  unnest(cols = c(data)) %>%
  mutate(y = case_when(rotate == "y" ~ -y+2,
                       TRUE ~ y)) %>%
  select(-rotate) %>%
  
  # spread groups out
  group_by(group) %>%
  nest() %>%
  ungroup() %>%
  mutate(bumpx = rep(c(0,2,4,6), times=4),
         bumpy = rep(c(0,2,4,6), each =4)) %>%
  unnest(cols = c(data)) %>%
  mutate(x=x+bumpx,
         y = y+bumpy) %>%
  select(-bumpx, -bumpy) %>%
  
  # add colors
  group_by(group,shape) %>%
  nest() %>%
  group_by(group) %>%
  mutate(color = sample(pal,4))  %>%
  #mutate(color = pal)
  unnest(cols = c(data))  %>%
  mutate(color = colorspace::lighten(color, amount = .18)) 
  



# start plot --------------------------------------------------------------
plot <- 
polygonsdf %>%  

  # start plot
  ggplot() +
  geom_polygon(aes(x=x,y=y, group= shape, fill = I(color))) +
  coord_equal(xlim = c(0,8),
              ylim=c(0,8),
              expand = F) +
  theme_void() +
  theme(plot.margin = margin(10,10,10,10,"pt"),
        plot.background = element_rect(fill = "transparent",
                                       color = "grey20",
                                       size= 2))+
#  scale_fill_manual(values = sample(pnw_palette("Bay",16),16,replace=F)) +
  theme(panel.background = element_rect(fill = "#f5f5e4", color="transparent"))


plot



# save --------------------------------------------------------------------

ggsave(plot,
       file = here::here("week4_phantoms_shadow","plot.png"), 
       width =6,
       height = 6,
       dpi=600,
       units = "in",
       limitsize = F,
       device = 'png')
