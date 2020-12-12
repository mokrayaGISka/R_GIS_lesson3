# Libraries ----
library(sf)
library(sp)
library(spdep)
library(tidyverse)
library(osrm)
library(osmdata)
library(tmap)



# Download Data from OSM 1.0 ----

# Districts
admin_osm = opq(bbox = 'Saint-Petersburg') %>%
  add_osm_feature(key = 'admin_level', value = '5') %>%
  osmdata_sf()

spb_admin_center = admin_osm$osm_multipolygons %>% dplyr::select(osm_id, name, admin_level, name) %>% filter (name  %in% c('Адмиралтейский район','Центральный район'))

# Defining frame 
frame = spb_admin_center %>% st_bbox() %>% st_as_sfc() %>% st_as_sf()
frame_UTM = frame %>% st_transform(crs = 32636)



# Download Data from OSM 2.0 ----

 osm_sf_Utm  = function(key_1, value_1, type) {
   osm = opq(bbox = frame) %>%
     add_osm_feature(key = key_1, value = value_1) %>%
     osmdata_sf()
   type_osm = get(type, osm)
   final = type_osm %>% dplyr::select(osm_id, name) %>% st_transform(crs = 32636) 
   return(final)
 }
 
spb_okrugs = osm_sf_Utm('admin_level', '8', 'osm_multipolygons') %>% st_crop(frame_UTM)
spb_roads = osm_sf_Utm('highway', c('motorway', 'trunk', 'primary', 'secondary', 'tertiary', 'residential'), 'osm_lines')
spb_food = osm_sf_Utm('amenity', c('restaurant', 'bar', 'cafe', 'pub', 'fast_food'), 'osm_points')
spb_water = osm_sf_Utm('natural', 'water', 'osm_multipolygons') %>% st_crop(frame_UTM)
spb_metro = osm_sf_Utm('railway', 'subway_entrance', 'osm_points') %>% filter(name>0)


# Making a basemap ----

spb_center_basemap = tm_shape(spb_water) +
  tm_polygons(col = "lightskyblue1",
              border.alpha = 0) +
  tm_shape(spb_roads)+
  tm_lines(col = "grey",
           lwd = 0.4)+
  tm_scale_bar(breaks = c(0,1), position = c('left', 'bottom'), color.dark = 'grey')

# Visualizing food places ----

spb_center_basemap + tm_shape(spb_food) +
  tm_dots(col = 'orange')
  

# Counting number of food places on each street  ----

spb_food_snapped = sf::st_snap(spb_food, spb_roads, tolerance = 20) %>% mutate (count = 1)
spb_streets_food = aggregate(spb_food_snapped %>% select(count), 
                 by = spb_roads, 
                 FUN = sum, 
                 drop=FALSE)

spb_roads2 = spb_roads %>% group_by(name) %>% summarise()
spb_streets_food2 = aggregate(spb_food_snapped %>% select(count), by = spb_roads2, FUN = sum, drop=FALSE) %>% filter(count>0)
                              

spb_center_basemap +
tm_shape(spb_streets_food2) +
  tm_lines(lwd = 'count', col = 'orange', title.lwd = "Amount", legend.lwd.is.portrait = TRUE)+
  tm_layout(title = "Food in Spb", title.bg.color = 'white', legend.bg.color = 'white')


# Counting number of food places in okrugs ----

spb_okrug_food = aggregate(spb_food_snapped %>% select(count), 
                              by = spb_okrugs, 
                              FUN = sum, 
                              drop=FALSE) %>% cbind(spb_okrugs$name) %>% mutate(name = gsub('округ','',spb_okrugs.name )) %>% filter(count>50)


spb_center_basemap +
  tm_shape(spb_okrug_food) +
  tm_polygons(col = 'count', alpha = 0.6, border.col = 'white')+
  tm_text('name', size = 0.5)+
  tm_layout(title = "Food places in St Petersburg")

spb_okrug_food2 = spb_okrug_food %>% mutate(density = count/(st_area(.)/1000000))

spb_center_basemap +
  tm_shape(spb_okrug_food2) +
  tm_polygons(col = 'density', alpha = 0.6, border.col = 'white', title = "Food places per sq.km")+
  tm_text('name', size = 0.5)+
  tm_layout(title = "Food places in St Petersburg", 
            fontfamily = 'Muller',
            legend.format = list(text.separator = "-"), 
            title.bg.color = 'white', 
            legend.bg.color = 'white',
            legend.title.size = 0.8)


# How to get to Bushe? ----

bushe = spb_food %>% filter(name == 'Буше')

bushe_WGS =  bushe %>% st_transform(crs = 4326) 
metro_WGS = spb_metro %>% st_transform(crs = 4326)

distancetable = osrmTable(src = metro_WGS, dst = bushe_WGS)

mindistances = bind_cols(distancetable$sources, mintime = apply(distancetable$durations, 1, min))
mindistances_sf =  st_as_sf(mindistances, coords = c("lon", "lat"), crs = 4326) %>% st_transform(crs = 32636)

icon_bushe = tmap_icons('bushe.png')

spb_center_basemap +
  tm_shape(mindistances_sf) +
  tm_symbols(col = 'mintime', border.alpha = 0.3, size = 0.7, palette = '-viridis')+
  tm_shape(bushe) + 
  tm_symbols(shape = icon_bushe, size = 0.3, border.alpha = 0)+
  tm_layout(title = 'Буше', 
            fontfamily = 'Muller',
            legend.format = list(text.separator = "-"), 
            title.bg.color = 'white', 
            legend.bg.color = 'white',
            legend.title.size = 0.8) + tm_shape(spb_metro)+
  tm_text('name', size = 0.5)

  

  
destinations = colnames(distancetable$durations)

distancetable_durations = distancetable$durations %>% cbind(rownames(distancetable$durations)) %>% as.data.frame() %>% pivot_longer(cols = all_of(destinations), names_to = 'destination', values_to = 'time')

distancetable_durations$time = as.numeric(as.character(distancetable_durations$time))
distancetable_destination = as.data.frame(distancetable$destination) %>%  mutate(destination = rownames(distancetable$destinations))
distancetable_durations2 = distancetable_durations %>% group_by(V10) %>% mutate(mintime = min(time)) %>% filter(time==mintime) %>% bind_cols(distancetable$sources) %>% merge(distancetable_destination, by = "destination")

from  = distancetable_durations2 %>% st_as_sf(coords = c("lon.x", "lat.x"), crs = 4326)
to = distancetable_durations2 %>% st_as_sf(coords = c("lon.y", "lat.y"), crs = 4326)


osrmRoute_f  = osrmRoute(src = from[1,], dst = to[1,],returnclass="sf")

for (i in 2:31) {
  osrmRoute_x = osrmRoute(src = from[i,], dst = to[i,],returnclass="sf")
  osrmRoute_f = rbind(osrmRoute_f, osrmRoute_x)
}


spb_center_basemap +
  tm_shape(osrmRoute_f) + 
  tm_lines(lwd = 1.5, lty= "dotted") +
  tm_shape(mindistances_sf) +
  tm_symbols(col = 'mintime', border.alpha = 0.3, size = 0.7, palette = '-viridis')+
  tm_shape(bushe) + 
  tm_symbols(shape = icon_bushe, size = 0.3, border.alpha = 0)+
  tm_layout(title = "Буше", 
            fontfamily = 'Muller',
            legend.format = list(text.separator = "-"), 
            title.bg.color = 'white', 
            legend.bg.color = 'white',
            legend.title.size = 0.8)

# Shortest travel around the city ----
random_points = spsample(frame_UTM %>% as_Spatial(),n=20,"random") %>% st_as_sf() %>% st_transform(crs = 4326)
spb_center_basemap + tm_shape(random_points) +tm_dots()

trip_raw = osrmTrip(random_points,returnclass="sf" )
trip = trip_raw[[1]]$trip %>% dplyr::mutate(row = 1:nrow(.))

spb_center_basemap + 
  tm_shape(trip) + 
  tm_lines(col = 'red', lty= "dashed", lwd = 2) + 
  tm_shape(random_points) + 
  tm_bubbles(size = 0.4, col = 'brown2') + 
  tm_shape(trip) +  
  tm_text('row', size = 0.8, bg.color = 'white', col = 'brown') +
  tm_layout(fontfamily = 'Muller') +
  tm_credits(c(paste("Время в пути - ", round(sum(trip$duration)), 'мин.', '\n' ,"Общая протяженность - ", '\n',round(sum(trip$distance)), 'км.')))

