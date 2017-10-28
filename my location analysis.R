#----Preparing Data----
library(jsonlite)
library(tidyverse)
library(ggmap)
library(tibbletime)

x <- fromJSON(("~/R Works/my location analysis/Riwayat Lokasi.json"))

#tweaking dataset
loc <- x$locations %>%
  mutate(time = as.POSIXct(as.numeric(timestampMs) / 1000,
                         origin = "1970-01-01"),
         lat = latitudeE7 / 1e7,
         lon = longitudeE7 / 1e7) %>%
  as_tbl_time(time)
View(head(loc))

#----plotting touring----
#sifting data from date 2017-10-16 to 2017-10-17
tour <- as_tbl_time(loc,
                    time) %>%
  time_filter(2017-10-16 ~ 2017-10-17)
View(head(tour))

#plotting locations
JawaTimur <- get_map(location = 'Jawa Timur', 
                     zoom = 8)

#options(stringsAsFactors = T)
ggmap(JawaTimur) + 
  stat_summary_2d(geom = "tile", 
                  bins = 100, 
                  data = tour, 
                  aes(x = lon, 
                      y = lat, 
                      z = accuracy), 
                  alpha = 0.5) + 
  scale_fill_gradient(low = "blue", 
                      high = "red", 
                      guide = guide_legend(title = "Accuracy")) +
  labs(
    x = "Longitude", 
    y = "Latitude", 
    title = "Lokasi perjalanan touring Magetan",
    subtitle = "Warna poin menunjukkan skala akurasi (rendah: biru, tinggi: merah)",
    caption = "\nThis bin plot shows recorded positions 
    and their accuracy in and around Jawa Timur")

#plotting velocity
tour2 <- tour[which(!is.na(loc$velocity)), ] 

ggmap(JawaTimur) + 
  geom_point(data = tour2, 
             aes(x = lon, 
                 y = lat, 
                 color = velocity), 
             alpha = 0.3) + 
  theme(legend.position = "right") + 
  labs(x = "Longitude", y = "Latitude", 
       title = "Kecepatan selama touring",
       subtitle = "Touring Surabaya-Magetan",
       caption = "\n Tingkat kecepatan selama touring") +
  scale_colour_gradient(low = "blue", 
                        high = "red", 
                        guide = guide_legend(title = "Kecepatan"))

#----lokasi tertinggi----
View(tour3 <- filter(tour, 
                     accuracy <= 20,
                     altitude ==  max(altitude,
                                      na.rm = T) | 
                       altitude <= mean(altitude,
                                       na.rm = T)))
ggmap(JawaTimur) + 
  geom_point(data = tour3, 
             aes(x = lon, 
                 y = lat, 
                 color = velocity), 
             alpha = 0.3) + 
  theme(legend.position = "right") + 
  labs(x = "Longitude", y = "Latitude", 
       title = "Kecepatan selama touring",
       subtitle = "Touring Surabaya-Magetan",
       caption = "\n Tingkat kecepatan selama touring") +
  scale_colour_gradient(low = "blue", 
                        high = "red", 
                        guide = guide_legend(title = "Kecepatan"))
  
#----plotting data to map (Indonesia)----
Indonesia <- get_map(location = "Indonesia",
                     zoom = 5)
ggmap(Indonesia) + 
  geom_point(data = loc, 
             aes(x = lon, 
                 y = lat), 
             alpha = 0.5, 
             color = "red") + 
  theme(legend.position = "right") + 
  labs(
    x = "Longitude", 
    y = "Latitude", 
    title = "Location history data points in Indonesia",
    caption = "\nA simple point plot shows recorded positions.")

#----Lokasi Jawa Timur----
JawaTimur <- get_map(location = 'Jawa Timur', 
                     zoom = 8)

options(stringsAsFactors = T)
ggmap(JawaTimur) + 
  stat_summary_2d(geom = "tile", 
                  bins = 100, 
                  data = loc, 
                  aes(x = lon, 
                      y = lat, 
                      z = accuracy), 
                  alpha = 0.5) + 
  scale_fill_gradient(low = "blue", 
                      high = "red", 
                      guide = guide_legend(title = "Accuracy")) +
  labs(
    x = "Longitude", 
    y = "Latitude", 
    title = "Location history data points around Jawa Timur",
    subtitle = "Color scale shows accuracy (low: blue, high: red)",
    caption = "\nThis bin plot shows recorded positions 
    and their accuracy in and around Jawa Timur")
#----Lokasi Jawa Tengah----
JawaTengah <- get_map(location = 'Jawa Tengah',
                      zoom = 8)

options(stringsAsFactors = T)
ggmap(JawaTengah) + 
  stat_summary_2d(geom = "tile", 
                  bins = 100, 
                  data = loc, 
                  aes(x = lon, 
                      y = lat, 
                      z = accuracy), 
                  alpha = 0.5) + 
  scale_fill_gradient(low = "blue", 
                      high = "red", 
                      guide = guide_legend(title = "Accuracy")) +
  labs(
    x = "Longitude", 
    y = "Latitude", 
    title = "Location history data points around Jawa Tengah",
    subtitle = "Color scale shows accuracy (low: blue, high: red)",
    caption = "\nThis bin plot shows recorded positions 
    and their accuracy in and around Jawa Tengah")
#----Lokasi Jakarta----
Jakarta <- get_map(location = 'Jakarta', 
                     zoom = 10)

options(stringsAsFactors = T)
ggmap(Jakarta) + 
  stat_summary_2d(geom = "tile", 
                  bins = 100, 
                  data = loc, 
                  aes(x = lon, 
                      y = lat, 
                      z = accuracy), 
                  alpha = 0.5) + 
  scale_fill_gradient(low = "blue", 
                      high = "red", 
                      guide = guide_legend(title = "Accuracy")) +
  labs(
    x = "Longitude", 
    y = "Latitude", 
    title = "Location history data points around Jakarta",
    subtitle = "Color scale shows accuracy (low: blue, high: red)",
    caption = "\nThis bin plot shows recorded positions 
    and their accuracy in and around Jakarta")
#----Lokasi Surabaya----
Surabaya <- get_map(location = 'Surabaya', 
                     zoom = 10)

options(stringsAsFactors = T)
ggmap(Surabaya) + 
  stat_summary_2d(geom = "tile", 
                  bins = 100, 
                  data = loc, 
                  aes(x = lon, 
                      y = lat, 
                      z = accuracy), 
                  alpha = 0.5) + 
  scale_fill_gradient(low = "blue", 
                      high = "red", 
                      guide = guide_legend(title = "Accuracy")) +
  labs(
    x = "Longitude", 
    y = "Latitude", 
    title = "Location history data points around Surabaya",
    subtitle = "Color scale shows accuracy (low: blue, high: red)",
    caption = "\nThis bin plot shows recorded positions 
    and their accuracy in and around Surabaya")
#----Lokasi Sidoarjo----
Sidoarjo <- get_map(location = 'Sidoarjo', 
                     zoom = 10)

options(stringsAsFactors = T)
ggmap(Sidoarjo) + 
  stat_summary_2d(geom = "tile", 
                  bins = 100, 
                  data = loc, 
                  aes(x = lon, 
                      y = lat, 
                      z = accuracy), 
                  alpha = 0.5) + 
  scale_fill_gradient(low = "blue", 
                      high = "red", 
                      guide = guide_legend(title = "Accuracy")) +
  labs(
    x = "Longitude", 
    y = "Latitude", 
    title = "Location history data points around Sidoarjo",
    subtitle = "Color scale shows accuracy (low: blue, high: red)",
    caption = "\nThis bin plot shows recorded positions 
    and their accuracy in and around Sidoarjo")
#----Kecepatan pindah di Surabaya----
loc_2 <- loc[which(!is.na(loc$velocity)), ]

Surabaya <- get_map(location = 'Surabaya', 
                   zoom = 12)

ggmap(Surabaya) + 
  geom_point(data = loc_2, 
             aes(x = lon, 
                 y = lat, 
                 color = velocity), 
             alpha = 0.3) + 
  theme(legend.position = "right") + 
  labs(x = "Longitude", y = "Latitude", 
       title = "Location history data points in Surabaya",
       subtitle = "Color scale shows velocity measured for location",
       caption = "\nA Tingkat kecepatan di Kota Surabaya") +
  scale_colour_gradient(low = "blue", 
                        high = "red", 
                        guide = guide_legend(title = "Velocity"))

#----Kecepatan pindah di Sidoarjo----
loc_2 <- loc[which(!is.na(loc$velocity)), ]

Sidoarjo <- get_map(location = 'Sidoarjo', 
                    zoom = 12)

ggmap(Sidoarjo) + 
  geom_point(data = loc_2, 
             aes(x = lon, 
                 y = lat, 
                 color = velocity), 
             alpha = 0.3) + 
  theme(legend.position = "right") + 
  labs(x = "Longitude", y = "Latitude", 
       title = "Location history data points in Sidoarjo",
       subtitle = "Color scale shows velocity measured for location",
       caption = "\nA Tingkat kecepatan di Kota Surabaya") +
  scale_colour_gradient(low = "blue", 
                        high = "red", 
                        guide = guide_legend(title = "Velocity"))
#----Kecepatan pindah di Malang----
loc_2 <- loc[which(!is.na(loc$velocity)), ]

Malang <- get_map(location = 'Malang', 
                    zoom = 12)

ggmap(Malang) + 
  geom_point(data = loc_2, 
             aes(x = lon, 
                 y = lat, 
                 color = velocity), 
             alpha = 0.3) + 
  theme(legend.position = "right") + 
  labs(x = "Longitude", y = "Latitude", 
       title = "Location history data points in Malang",
       subtitle = "Color scale shows velocity measured for location",
       caption = "\nA Tingkat kecepatan di Kota Surabaya") +
  scale_colour_gradient(low = "blue", 
                        high = "red", 
                        guide = guide_legend(title = "Velocity"))
#loc3 <- with(loc, 
#             subset(loc, 
#                    loc$time > as.POSIXct('2016-01-01 0:00:01')))
#loc3 <- with(loc, 
#             subset(loc3, 
#                    loc$time < as.POSIXct('2016-12-22 23:59:59')))

# Shifting vectors for latitude and longitude to include end position
#shift.vec <- function(vec, shift){
#  if (length(vec) <= abs(shift)){
#    rep(NA ,length(vec))
#  } else {
#    if (shift >= 0) {
#      c(rep(NA, shift), vec[1:(length(vec) - shift)]) }
#    else {
#      c(vec[(abs(shift) + 1):length(vec)], rep(NA, abs(shift)))
#    }
#  }
#}

#loc3$lat.p1 <- shift.vec(loc3$lat, -1)
#loc3$lon.p1 <- shift.vec(loc3$lon, -1)

# Calculating distances between points (in metres) with the function pointDistance from the 'raster' package.
#library(raster)
#loc3$dist.to.prev <- apply(loc3, 1, 
#                          FUN = function(row) {
#  pointDistance(c(as.numeric(as.character(row["lat.p1"])),
#                  as.numeric(as.character(row["lon.p1"]))),
#                c(as.numeric(as.character(row["lat"])),
#                  as.numeric(as.character(row["lon"]))),
#                lonlat = T) # Parameter 'lonlat' has to be TRUE!
#})
# distance in km
#round(sum(as.numeric(as.character(loc3$dist.to.prev)), 
#          na.rm = TRUE)*0.001, 
#      digits = 2)

#distance_p_month <- aggregate(loc3$dist.to.prev, 
#                              by = list(month_year = as.factor(loc3$month_year)), 
#                              FUN = sum)
#distance_p_month$x <- distance_p_month$x*0.001
#ggplot(distance_p_month[-1, ], 
#       aes(x = month_year, 
#           y = x,  
#           fill = month_year)) + 
#  geom_bar(stat = "identity")  + 
#  guides(fill = FALSE) +
#  my_theme() +
#  labs(
#    x = "",
#    y = "Distance in km",
#    title = "Distance traveled per month in 2016",
#    caption = "This barplot shows the sum of distances between recorded 
#    positions for 2016. In September we went to the US and Canada."
#  )

#activities <- loc3$activitys

#list.condition <- sapply(activities, function(x) !is.null(x[[1]]))
#activities  <- activities[list.condition]

#df <- do.call("rbind", activities)
#main_activity <- sapply(df$activities, 
#                        function(x) x[[1]][1][[1]][1])

#activities_2 <- data.frame(main_activity = main_activity, 
#                           time = as.POSIXct(as.numeric(df$timestampMs)/1000, 
#                                             origin = "1970-01-01"))

#head(activities_2)

#ggplot(activities_2, 
#       aes(x = main_activity, 
#           group = main_activity, 
#           fill = main_activity)) + 
#  geom_bar()  + 
#  guides(fill = FALSE) +
#  my_theme() +
#  labs(
#    x = "",
#    y = "Count",
#    title = "Main activities in 2016",
#    caption = "Associated activity for recorded positions in 2016. 
#    Because Google records activity probabilities for each position, 
#    only the activity with highest likelihood were chosen for each position."
#  )
