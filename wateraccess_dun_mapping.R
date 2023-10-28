# VISUALIZATION

# inspired by @karim_douieb

# load packages
library(tidyverse)


# set strings as factors to false
options(stringsAsFactors = FALSE)


#########################
# MAPPING

library(sf) # processing spatial vector data
library(raster) # processing spatial raster data. !!!overwrites dplyr::select!!!
#library(rgdal) # read shapefile #will be retired in 2023, replace


# map country and coordinate data

## shapefile
result_shp <- read_sf("C:/Users/User/Documents/GIS/DOSM/msia_dun_pt.shp")

state_shp <- read_sf("C:/Users/User/Documents/GIS/Boundary/state_msia_dosm.shp")


#DOSM
msia_dun_shp <- read_sf("C:/Users/User/Documents/GIS/DOSM/dosm_msia_dun_desktop.geojson")


crs(result_shp)
crs(state_shp)
crs(msia_dun_shp)




# layout map (read_sf)

map <- ggplot() + 
  geom_sf(data = msia_dun_shp) + #read_sf
  geom_sf(data = state_shp, color = 'red', size = 1, fill = NA) +
  geom_point(data = as.data.frame(result_shp), aes(x = Long, y = Lat),
              color = 'blue', size = 1, alpha = 0.5) +
  coord_sf(xlim = c(99, 120), ylim = c(0.5, 8)) +
  theme_void()

map


#########################
# POLYGON MAP


###import data
dun_data = read.csv("C:/Users/User/Documents/GIS/DOSM/census_dun.csv", 
                   header = T, sep = ",")


### set format
str(dun_data)

### select columns
dun_data <- dun_data[, c(1:10, 51:55)]


##initial dummy value
x_pop <- 100


### join data to shp
dun_data_shp <- msia_dun_shp %>% 
  full_join(dun_data, by = c("code_state_dun", "state", "parlimen", "dun", 
                             "code_state", "code_parlimen", "code_dun")) %>% 
  mutate(xutilities_pipedwater_home = 100 - utilities_pipedwater_home) %>% 
  mutate(pop_xwater = population_total*xutilities_pipedwater_home/100) %>% 
  mutate(pop_xwater_ratio = pop_xwater/x_pop*0.1) %>% 
  mutate(Perc_class = cut(xutilities_pipedwater_home, breaks = c(-1, 0, 1, 10, 20, 50, 100),
                           labels = c("0", "0.1-1", "1.1-10", "11-20", "21-50", ">50")))
  #mutate(votes_ratio = (votes/min(votes)*0.1)) %>%   #create ratio for size of dorling cartogram
  #mutate(lon = map_dbl(geometry, ~st_point_on_surface(.x)[[1]]),
  #       lat = map_dbl(geometry, ~st_point_on_surface(.x)[[2]]))

  

## second lowest value after 0
### find 2nd lowest population with no water for ratio calculation
### lowest is 0
#min(dun_data_shp$pop_xwater[dun_data_shp$pop_xwater != min(dun_data_shp$pop_xwater)])
# Rank the column without skipping number
df_ranked <- dense_rank(dun_data_shp$pop_xwater)
  
# Find the row index of the given rank
row_index <- which(df_ranked == 2)
  
# Return the value of the column at the given row index
x_pop <- dun_data_shp$pop_xwater[row_index]
#rerun join data to shp!!


### remove na (wilayah no dun)
dun_data_shp <- filter(dun_data_shp, !is.na(code_dun))


### reformat field
#dun_data_shp$xutilities_pipedwater_home <- as.numeric(as.character(dun_data_shp$xutilities_pipedwater_home))


### add field ID for animation sequence
#result_shp$ID <- 1:nrow(result_shp) #by parlimen
#dun_data_shp$rank <- rank(dun_data_shp$lon, ties.method = "first") #by longitude


# CARTOGRAM
# using dorling (non overlapping circles)

library(cartogram)

class(dun_data_shp)
crs(dun_data_shp)

dun_data_shp2 <- st_as_sf(dun_data_shp)
dun_data_shp2 <-st_transform(dun_data_shp2, crs = 32648)  #need projected coordinate system, eg WGS 84 / UTM zone 48N

crs(dun_data_shp2)

# select data != 0 as cartogram cannot process 0
# don't remove 0 if not polygon with 0 will be empty, change 0 to 0.01
#dun_data_shp4 <- dun_data_shp2 %>% 
#  filter(pop_xwater_ratio != 0)
dun_data_shp2$pop_xwater_ratio[dun_data_shp2$pop_xwater_ratio == 0] <- 0.01

crs(dun_data_shp2)

# set cartogram
dun_data_dorling <- cartogram_dorling(dun_data_shp2, k = 0.1, weight = "pop_xwater_ratio") #adjust k for size


### remove rank column from data so no confusion in animating layers with same field
#dun_data_shp3 <- subset(dun_data_shp, select = -result)




### set colors by classification

col_class <- c(
  "0" = "#4083ee", 
  "0.1-1" = "#6aaaf4", 
  "1.1-10" = "#82c2ff", 
  "11-20" = "#BEF7FF", 
  "21-50" = "#ffb860", 
  ">50" = "#ee0000")




# FONT

library(extrafont)

## import font
font_import(pattern="Roboto", prompt=FALSE)
## set 'sans' font as Roboto
windowsFonts(sans="Roboto Light") 
## load fonts
loadfonts(device="win")
loadfonts(device="postscript")


## mapping

library(ggrepel)
library(plotly)

map_polygon <- ggplot() + 
  geom_sf(data = dun_data_shp, 
          mapping = aes(fill = as.factor(Perc_class), group = as.factor(Perc_class)), 
          color = "white", linewidth = 0.05, #alpha = 0.2,
          show.legend = T) +
  geom_sf(data = dun_data_dorling, mapping = aes(fill = Perc_class), 
          color = NA, #alpha = 0.5,
          show.legend = FALSE) +
  geom_sf(data = state_shp, color = 'red', size = 1, fill = NA) +
  scale_fill_manual(name = "% no piped water access", values = col_class) +
  coord_sf(xlim = c(103.5, 119), ylim = c(1.1, 7.3)) +
  theme_void() +
  theme(plot.title = element_text(family = "sans", 
                                  size = 15, hjust = 0.45, vjust = 2),
        legend.title = element_text(size = 10), 
        plot.caption = element_text(size = 8, color = "grey50", hjust = 0.1),
        legend.position = "bottom") +
  annotate("text", x = 111, y = 6.5, size = 4,
           label = "Percentage of population with\nno access to piped water by DUN") +
  #geom_text_repel(data = label_party, aes(x = lon, y = lat, label = Ratio_class),
                  #min.segment.length = 0, seed = 42, box.padding = 0.5,
  #                size = 2, color = "grey30") +
  labs(caption = "Data source: DOSM") +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))


map_polygon


### print last plot to file
ggsave(paste0("msia_dun_xwater_map-dorling1.jpg"), dpi = 300,
       width = 12, height = 5, units = "in")



## mapping choropleth


map_choro <- ggplot() + 
  geom_sf(data = dun_data_shp, 
          mapping = aes(fill = as.factor(Perc_class), 
                        group = as.factor(Perc_class)), 
          color = "white", alpha = 1, show.legend = T) +
  geom_sf(data = state_shp, color = 'red', size = 1, fill = NA) +
  scale_fill_manual(name = "% no piped water access", values = col_class) +
  coord_sf(xlim = c(103.5, 119), ylim = c(1.1, 7.3)) +
  theme_void() +
  theme(plot.title = element_text(family = "sans", 
                                  size = 15, hjust = 0.45, vjust = 2),
        legend.title = element_text(size = 10), 
        plot.caption = element_text(size = 8, color = "grey50", hjust = 0),
        plot.margin = margin(r = 10, l = 10),
        legend.position = "bottom") +
  annotate("text", x = 111, y = 6.5, size = 4,
           label = "Percentage of population with\nno access to piped water by DUN") +
  labs(caption = "Data source: DOSM") +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))


map_choro

### print last plot to file
ggsave(paste0("msia_dun_xwater_map-choro.jpg"), dpi = 300,
       width = 12, height = 5, units = "in")


## mapping cartogram

#dun_data_dorling2 <- cartogram_dorling(dun_data_shp2, k = 0.1, weight = "votes_ratio") #dunno why k=0.1 works

map_carto <- ggplot() + 
  geom_sf(data = dun_data_shp, 
          #mapping = aes(fill = as.factor(Perc_class), group = as.factor(Perc_class)), 
          color = "grey", fill = NA, linewidth = 0.05, #alpha = 0.2,
          show.legend = F) +
  geom_sf(data = dun_data_dorling, 
          mapping = aes(fill = as.factor(Perc_class), group = as.factor(Perc_class),
                        text = paste0(dun, "<br>", pop_xwater)), 
          color = NA, alpha = 1,
          show.legend = T) +
  #geom_sf(data = dun_data_dorling, mapping = aes(fill = Perc_class, group = Perc_class), 
  #        color = NA, #alpha = 0.5,
  #        show.legend = T) +
  geom_sf(data = state_shp, color = 'red', size = 1, fill = NA) +
  scale_fill_manual(name = "% no piped water access", values = col_class) +
  coord_sf(xlim = c(103.5, 119), ylim = c(1.1, 7.3)) +
  theme_void() +
  theme(plot.title = element_text(family = "sans", 
                                  size = 15, hjust = 0.45, vjust = 2),
        legend.title = element_text(size = 10), 
        plot.caption = element_text(size = 8, color = "grey50", hjust = 0),
        plot.margin = margin(r = 10, l = 10),
        legend.position = "bottom") +
  annotate("text", x = 111, y = 6.5, size = 4,
           label = "Percentage of population with\nno access to piped water by DUN") +
  annotate("text", x = 106.4, y = 7.3, size = 2, hjust = 0,
           label = "31015 (N.21 Pantai Irama)") +
  annotate("text", x = 107.0, y = 5.6, size = 2, hjust = 0,
           label = "2339 (N.26 Rantau Abang)") +
  annotate("text", x = 118.5, y = 5.6, size = 2, hjust = 0,
           label = "40513\n(N.59 Sukau)") +
  annotate("text", x = 112.0, y = 3.6, size = 2, hjust = 0,
           label = "10393 (N.70 Samalaju)") +
  labs(caption = "Data source: DOSM") +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))


map_carto


ggplotly(map_carto, tooltip = "text",
         width = 1000, height = 500) 



### print last plot to file
ggsave(paste0("msia_dun_xwater_map-carto_lbl.jpg"), dpi = 300,
       width = 12, height = 5, units = "in")



#########################

# ANIMATION CARTOGRAM

#http://r-graph-gallery.com/a-smooth-transition-between-chloropleth-and-cartogram

library(tweenr)
library(gganimate)
library(sfheaders)
library(transformr)


crs(dun_data_dorling)
crs(dun_data_shp2)


### change projection to WGS84 (original UTM) so both spatial object have same proj
class(dun_data_dorling)
dun_data_dorling2 <- st_transform(dun_data_dorling, 
                                 crs = "+proj=longlat +ellps=WGS84 +datum=WGS84")

dun_data_shp3 <- st_transform(dun_data_shp2, 
                               crs = "+proj=longlat +ellps=WGS84 +datum=WGS84")

crs(dun_data_dorling2)
crs(dun_data_shp3)


class(dun_data_shp3)
class(dun_data_dorling2)


## Give an id to every row (every DUN)
dun_data_dorling2$dun_no <- seq(1, nrow(dun_data_dorling2))
dun_data_shp3$dun_no <- seq(1, nrow(dun_data_shp3))


## Transform these 2 objects in dataframe, plotable with ggplot2
dun_data_dorling2_df <- sf_to_df(dun_data_dorling2) 
dun_data_shp3_df <- sf_to_df(dun_data_shp3)



## join fields to connect df with data (DUN)
dun_data_dorling2_df2 <- left_join(dun_data_dorling2_df, 
                                 dun_data_dorling2 %>% dplyr::select(c(4, 9, 19, 21, 22)), 
                                 by = c("polygon_id" = "dun_no")) 
dun_data_shp3_df2 <- left_join(dun_data_shp3_df, 
                               dun_data_shp3 %>% dplyr::select(c(4, 9, 19, 21, 22)), 
                                 by = c("multipolygon_id" = "dun_no")) 


## set time
#dun_data_shp3_df2$time <- 1
#dun_data_dorling2_df2$time <- 30


## remove geometry (for tweenr to work somehow), multipolygon_id for dun_data_shp3
dun_data_shp3_df2 <- subset(dun_data_shp3_df2, select = -c(polygon_id, geometry))
dun_data_dorling2_df2 <- subset(dun_data_dorling2_df2, select = -geometry)
dun_data_shp3_df2 <- dun_data_shp3_df2 %>% 
  rename(polygon_id = multipolygon_id)

### cannot use geom_sf if no geometry


## Set transformation type + time
#dun_data_df$ease <- "cubic-in-out"


## Calculate the transition between these 2 objects
dtw <- tween_polygon(dun_data_shp3_df2, dun_data_dorling2_df2, 
                     ease = 'cubic-in-out', 
                     nframe = 30, match = F,
                     id = polygon_id) %>% 
  keep_state(10)


## check a few frame
ggplot() + 
  geom_polygon(data = dun_data_shp3_df2, 
               aes(fill = polygon_id, x = x, y = y, group = polygon_id), size = 0, alpha = 0.9
  )
ggplot() + 
  geom_polygon(data = dtw  %>% filter(.frame == 30), 
               aes(fill = Perc_class, x = x, y = y, group = polygon_id), size = 0, alpha = 0.9
  )




# Plot animated map - testing

map_anim_try1 <- ggplot() + 
  geom_polygon(data = dtw,  #%>% filter(.frame == 30), #try out frame by frame
          mapping = aes(fill = Perc_class, group = polygon_id, 
                        x = x, y = y), 
          color = "white", alpha = 1, size = 0.05,
          show.legend = FALSE) +
  scale_fill_manual(name = "% no piped water access", values = col_class) +
  coord_sf(xlim = c(103.5, 119), ylim = c(1.1, 7.3)) +
  theme_void() +
  theme(plot.title = element_text(family = "Times New Roman", 
                                  size = 15, hjust = 0.45, vjust = 2),
        legend.title = element_text(size = 10), 
        plot.caption = element_text(size = 8, color = "grey50", hjust = 0.1),
        legend.position = "bottom") +
  labs(title = paste0("Percentage of population with\nno access to piped water by DUN"), 
       caption = "Inspired by @karim_douieb\nData source: DOSM") +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))


# Final map

map_anim_try1 <- ggplot() + 
  geom_sf(data = dun_data_shp, 
          #mapping = aes(fill = as.factor(Perc_class), group = as.factor(Perc_class)), 
          color = "grey", fill = NA, linewidth = 0.05, #alpha = 0.2,
          show.legend = F) +
  geom_polygon(data = dtw, # %>% filter(.frame == 5), #try out frame by frame
               mapping = aes(fill = Perc_class, group = polygon_id, 
                             x = x, y = y), 
               color = "white", alpha = 1, linewidth = 0.05,
               show.legend = T) +
  geom_sf(data = state_shp, color = 'red', size = 1, fill = NA) +
  scale_fill_manual(name = "% no piped water access", values = col_class) +
  coord_sf(xlim = c(103.5, 119), ylim = c(1.1, 7.3)) +
  theme_void() +
  theme(plot.title = element_text(family = "sans", 
                                  size = 15, hjust = 0.45, vjust = 2),
        legend.title = element_text(size = 4), 
        legend.text = element_text(size = 3),
        legend.key.size = unit(0.2, 'cm'),
        plot.caption = element_text(size = 4, color = "grey50", hjust = 0),
        plot.margin = margin(r = 10, l = 10),
        legend.position = "bottom") +
  annotate("text", x = 111, y = 6.5, size = 2,
           label = "Percentage of population with\nno access to piped water by DUN") +
  labs(caption = "Data source: DOSM") +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))


map_anim_try1



# Make the animation


### mapping animation

map_anim <- map_anim_try1 +
  #shadow_mark() +
  transition_manual(frames = .frame)


gganimate::animate(map_anim, fps = 30, res = 300, #nframes = num_dun, 
                   start_pause = 10, end_pause = 10,
                   width = 1000, height = 500
                   )

anim_save("dun_xwater_map_anim1.gif")




