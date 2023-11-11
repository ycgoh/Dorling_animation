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
point_shp <- read_sf("C:/Users/User/Documents/GIS/DOSM/msia_parlimen_dosm_pt.shp")

state_shp <- read_sf("C:/Users/User/Documents/GIS/Boundary/state_msia_dosm.shp")


#DOSM
msia_parl_shp <- read_sf("C:/Users/User/Documents/GIS/DOSM/parlimen_desktop.json")


crs(point_shp)
crs(state_shp)
crs(msia_parl_shp)




# layout map (read_sf)

map <- ggplot() + 
  geom_sf(data = msia_parl_shp) + #read_sf
  geom_sf(data = state_shp, color = 'red', size = 1, fill = NA) +
  geom_point(data = as.data.frame(point_shp), aes(x = Long, y = Lat),
              color = 'blue', size = 1, alpha = 0.5) +
  coord_sf(xlim = c(99, 120), ylim = c(0.5, 8)) +
  theme_void()

map


#########################
# POLYGON MAP


###import data
parl_data = read.csv("C:/Users/User/Documents/GIS/DOSM/census_parlimen.csv", 
                   header = T, sep = ",")


### set format
str(parl_data)

### select columns
parl_data <- parl_data[, c(1:7, 48)]


##initial dummy value
x_pop <- min(parl_data$population_total)
max(parl_data$population_total)


### join data to shp
parl_data_shp <- msia_parl_shp %>% 
  full_join(parl_data, by = c("state", "parlimen",  
                             "code_state", "code_parlimen")) %>% 
  mutate(pop_ratio = population_total/x_pop*0.1) %>% 
  mutate(Perc_class = cut(population_total, breaks = c(20000, 100000, 200000, 300000, 400000, 500000, 600000, 700000),
                           labels = c("20k-100k", "100k-200k", "200k-300k", "300k-400k", "400k-500k", "500k-600k", "600k-700k")))


  

# CARTOGRAM
# using dorling (non overlapping circles)

library(cartogram)

class(parl_data_shp)
crs(parl_data_shp)

parl_data_shp2 <- st_as_sf(parl_data_shp)
parl_data_shp2 <-st_transform(parl_data_shp2, crs = 32648)  #need projected coordinate system, eg WGS 84 / UTM zone 48N

crs(parl_data_shp2)

# select data != 0 as cartogram cannot process 0
# don't remove 0 if not polygon with 0 will be empty, change 0 to 0.01


# set cartogram
parl_data_dorling <- cartogram_dorling(parl_data_shp2, k = 0.12, weight = "pop_ratio") #adjust k for size


### remove rank column from data so no confusion in animating layers with same field
#parl_data_shp3 <- subset(parl_data_shp, select = -result)



### set colors by classification
library(RColorBrewer)

col_pal <- brewer.pal(n = 7, name = "YlGnBu")

col_class <- c(
  "20k-100k" = "#FFF300", 
  "100k-200k" = "#C7E9B4", 
  "200k-300k" = "#7FCDBB", 
  "300k-400k" = "#41B6C4", 
  "400k-500k" = "#1D91C0", 
  "500k-600k" = "#225EA8", 
  "600k-700k" = "#0C2C84")




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
  geom_sf(data = parl_data_shp, 
          mapping = aes(fill = as.factor(Perc_class), group = as.factor(Perc_class)), 
          color = "white", linewidth = 0.05, #alpha = 0.2,
          show.legend = T) +
  geom_sf(data = parl_data_dorling, mapping = aes(fill = Perc_class), 
          color = NA, #alpha = 0.5,
          show.legend = FALSE) +
  geom_sf(data = state_shp, color = 'red', size = 1, fill = NA) +
  scale_fill_manual(name = "Population", values = col_class) +
  coord_sf(xlim = c(103.5, 119), ylim = c(1.1, 7.3)) +
  theme_void() +
  theme(plot.title = element_text(family = "sans", 
                                  size = 15, hjust = 0.45, vjust = 2),
        legend.title = element_text(size = 10), 
        plot.caption = element_text(size = 8, color = "grey50", hjust = 0.1),
        legend.position = "bottom") +
  annotate("text", x = 111, y = 6.5, size = 4,
           label = "Malaysia\nPopulation by parlimen (2020)") +
  labs(caption = "Data source: DOSM") +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))


map_polygon


### print last plot to file
ggsave(paste0("msia_parl_pop_map-dorling1.jpg"), dpi = 300,
       width = 12, height = 5, units = "in")



## mapping choropleth


map_choro <- ggplot() + 
  geom_sf(data = parl_data_shp, 
          mapping = aes(fill = as.factor(Perc_class), 
                        group = as.factor(Perc_class)), 
          color = "white", alpha = 1, show.legend = T) +
  geom_sf(data = state_shp, color = 'red', size = 1, fill = NA) +
  scale_fill_manual(name = "Population", values = col_class) +
  coord_sf(xlim = c(103.5, 119), ylim = c(1.1, 7.3)) +
  theme_void() +
  theme(plot.title = element_text(family = "sans", 
                                  size = 15, hjust = 0.45, vjust = 2),
        legend.title = element_text(size = 10), 
        plot.caption = element_text(size = 8, color = "grey50", hjust = 0),
        plot.margin = margin(r = 10, l = 10),
        legend.position = "bottom") +
  annotate("text", x = 111, y = 6.5, size = 4,
           label = "Malaysia\nPopulation by parlimen (2020)") +
  labs(caption = "Data source: DOSM") +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))


map_choro

### print last plot to file
ggsave(paste0("msia_parl_pop_map-choro.jpg"), dpi = 300,
       width = 12, height = 5, units = "in")


## mapping cartogram

map_carto <- ggplot() + 
  geom_sf(data = parl_data_shp, 
          #mapping = aes(fill = as.factor(Perc_class), group = as.factor(Perc_class)), 
          color = "grey", fill = NA, linewidth = 0.05, #alpha = 0.2,
          show.legend = F) +
  geom_sf(data = parl_data_dorling, 
          mapping = aes(fill = as.factor(Perc_class), group = as.factor(Perc_class),
                        text = paste0(parlimen, "<br>", population_total)), 
          color = NA, alpha = 1,
          show.legend = T) +
  geom_sf(data = state_shp, color = 'red', size = 1, fill = NA, alpha = 0.7) +
  scale_fill_manual(name = "Population", values = col_class) +
  coord_sf(xlim = c(103.5, 119), ylim = c(1.1, 7.3)) +
  theme_void() +
  theme(plot.title = element_text(family = "sans", 
                                  size = 15, hjust = 0.45, vjust = 2),
        legend.title = element_text(size = 10), 
        plot.caption = element_text(size = 8, color = "grey50", hjust = 0),
        plot.margin = margin(r = 10, l = 10),
        legend.position = "bottom") +
  annotate("text", x = 111, y = 6.5, size = 4,
           label = "Malaysia\nPopulation by parlimen (2020)") +
  annotate("text", x = 103.4, y = 1.5, size = 2, hjust = 0,
           label = "Largest population: 687,609 (P.102 Bangi, Selangor)") +
  annotate("text", x = 103.4, y = 1.2, size = 2, hjust = 0,
           label = "Smallest population: 24,700 (P.210 Kanowit, Sarawak)") +
  labs(caption = "Data source: DOSM") +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))


map_carto


ggplotly(map_carto, tooltip = "text",
         width = 1000, height = 500) 



### print last plot to file
ggsave(paste0("msia_parl_pop_map-carto_lbl.jpg"), dpi = 300,
       width = 12, height = 5, units = "in")



#########################

# ANIMATION CARTOGRAM

#http://r-graph-gallery.com/a-smooth-transition-between-chloropleth-and-cartogram

library(tweenr)
library(gganimate)
library(sfheaders)
library(transformr)


crs(parl_data_dorling)
crs(parl_data_shp2)


### change projection to WGS84 (original UTM) so both spatial object have same proj
class(parl_data_dorling)
parl_data_dorling2 <- st_transform(parl_data_dorling, 
                                 crs = "+proj=longlat +ellps=WGS84 +datum=WGS84")

parl_data_shp3 <- st_transform(parl_data_shp2, 
                               crs = "+proj=longlat +ellps=WGS84 +datum=WGS84")

crs(parl_data_dorling2)
crs(parl_data_shp3)


class(parl_data_shp3)
class(parl_data_dorling2)


## Give an id to every row (every parlimen)
parl_data_dorling2$parl_no <- seq(1, nrow(parl_data_dorling2))
parl_data_shp3$parl_no <- seq(1, nrow(parl_data_shp3))


## Transform these 2 objects in dataframe, plotable with ggplot2
parl_data_dorling2_df <- sf_to_df(parl_data_dorling2) 
parl_data_shp3_df <- sf_to_df(parl_data_shp3)



## join fields to connect df with data (parlimen)
parl_data_dorling2_df2 <- left_join(parl_data_dorling2_df, 
                                 parl_data_dorling2 %>% dplyr::select(c(5 , 6 , 9 , 11:13)), 
                                 by = c("polygon_id" = "parl_no")) 
parl_data_shp3_df2 <- left_join(parl_data_shp3_df, 
                               parl_data_shp3 %>% dplyr::select(c(5 , 6 , 9 , 11:13)), 
                                 by = c("multipolygon_id" = "parl_no")) 


## set time
#parl_data_shp3_df2$time <- 1
#parl_data_dorling2_df2$time <- 30


## remove geometry (for tweenr to work somehow), multipolygon_id for parl_data_shp3
parl_data_shp3_df2 <- subset(parl_data_shp3_df2, select = -c(polygon_id, geometry))
parl_data_dorling2_df2 <- subset(parl_data_dorling2_df2, select = -geometry)
parl_data_shp3_df2 <- parl_data_shp3_df2 %>% 
  rename(polygon_id = multipolygon_id)

### cannot use geom_sf if no geometry


## Set transformation type + time
#parl_data_df$ease <- "cubic-in-out"


## Calculate the transition between these 2 objects
dtw <- tween_polygon(parl_data_shp3_df2, parl_data_dorling2_df2, 
                     ease = 'cubic-in-out', 
                     nframe = 30, match = F,
                     id = polygon_id) %>% 
  keep_state(10)


## check a few frame
ggplot() + 
  geom_polygon(data = parl_data_shp3_df2, 
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
  scale_fill_manual(name = "Population", values = col_class) +
  coord_sf(xlim = c(103.5, 119), ylim = c(1.1, 7.3)) +
  theme_void() +
  theme(plot.title = element_text(family = "Times New Roman", 
                                  size = 15, hjust = 0.45, vjust = 2),
        legend.title = element_text(size = 10), 
        plot.caption = element_text(size = 8, color = "grey50", hjust = 0.1),
        legend.position = "bottom") +
  labs(title = paste0("Malaysia\nPopulation by parlimen"), 
       caption = "Inspired by @karim_douieb\nData source: DOSM") +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))


# Final map

map_anim_try1 <- ggplot() + 
  geom_sf(data = parl_data_shp, 
          #mapping = aes(fill = as.factor(Perc_class), group = as.factor(Perc_class)), 
          color = "grey", fill = NA, linewidth = 0.05, #alpha = 0.2,
          show.legend = F) +
  geom_polygon(data = dtw, # %>% filter(.frame == 5), #try out frame by frame
               mapping = aes(fill = Perc_class, group = polygon_id, 
                             x = x, y = y), 
               color = "white", alpha = 1, linewidth = 0.05,
               show.legend = T) +
  geom_sf(data = state_shp, color = 'red', size = 1, fill = NA, alpha = 0.7) +
  scale_fill_manual(name = "Population", values = col_class) +
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
           label = "Malaysia\nPopulation by parlimen (2020)") +
  annotate("text", x = 103.4, y = 1.5, size = 1, hjust = 0,
           label = "Largest population: 687,609 (P.102 Bangi, Selangor)") +
  annotate("text", x = 103.4, y = 1.2, size = 1, hjust = 0,
           label = "Smallest population: 24,700 (P.210 Kanowit, Sarawak)") +
  labs(caption = "Data source: DOSM") +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))


map_anim_try1



# Make the animation


### mapping animation

map_anim <- map_anim_try1 +
  #shadow_mark() +
  transition_manual(frames = .frame)


gganimate::animate(map_anim, fps = 30, res = 300,
                   start_pause = 10, end_pause = 10,
                   width = 1000, height = 500
                   )

anim_save("parlimen_pop_map_anim1.gif")




