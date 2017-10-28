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
rm(x)
#----plotting touring 1----
#sifting data from date 2017-10-16 and 2017-10-17
tour1 <- loc[~2017-10-16]
tour2 <- loc[~2017-10-17]

#plotting Jatim map 
JawaTimur <- get_map(location = 'Jawa Timur', 
                     zoom = 8)
JawaTimur2 <- get_map(location = "Jawa Timur",
                      zoom = 9)

#plotting Sby-Mgt berdasar koordinat yg tercatat
##sinyal hilang setelah mojokerto ke jombang
ggmap(JawaTimur) +
  geom_point(data = tour1,
             mapping = aes(x = lon,
                           y = lat,
                           colour = accuracy),
             alpha = 0.5) +
  labs(x = "Longitude",
       y = "Latitude",
       title = "Koordinat yang terekam Google selama touring Sby-Mgt",
       caption = "Nilai akurasi semakin kecil berarti semakin akurat")
  
#Plotting tour Sby-Mgt 2017-10-16 berdasar akurasi
ggmap(JawaTimur) + 
  stat_summary_2d(geom = "tile", 
                  bins = 100, 
                  data = tour1, 
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
    title = "Lokasi perjalanan touring Surabaya-Magetan",
    subtitle = "Warna poin menunjukkan skala akurasi (rendah: biru, tinggi: merah)",
    caption = "\nThis bin plot shows recorded positions 
    and their accuracy in and around Jawa Timur")

#plotting velocity Sby-Mgt 2017-10-16
tour1v <- filter(tour1,
                !is.na(velocity))

max(tour1v$velocity) #kec maks cuma 25? salah besar ini

ggmap(JawaTimur) + 
  geom_point(data = tour1v, 
             aes(x = lon, 
                 y = lat, 
                 color = velocity), 
             alpha = 0.5) + 
  theme(legend.position = "right") + 
  labs(x = "Longitude", y = "Latitude", 
       title = "Kecepatan selama touring",
       subtitle = "Touring Surabaya-Magetan",
       caption = "\n Tingkat kecepatan selama touring") +
  scale_colour_gradient(low = "blue", 
                        high = "red", 
                        guide = guide_legend(title = "Kecepatan"))

#----plotting touring 2----

#plotting Mgt-Sby berdasar koordinat yg tercatat
##Koordinat lengkap sepanjang perjalanan
ggmap(JawaTimur) +
  geom_point(data = tour2,
             mapping = aes(x = lon,
                           y = lat,
                           colour = accuracy),
             alpha = 0.5)  

#Plotting tour Mgt-Sby 2017-10-17 berdasar akurasi
ggmap(JawaTimur) + 
  stat_summary_2d(geom = "tile", 
                  bins = 100, 
                  data = tour2, 
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
    title = "Lokasi perjalanan touring Magetan-Surabaya",
    subtitle = "Warna poin menunjukkan skala akurasi (rendah: biru, tinggi: merah)",
    caption = "\nKoordinat tercatat lengkap sepanjang perjalanan")

#plotting velocity Mgt-Sby 2017-10-17
tour2v <- filter(tour2,
                 !is.na(velocity))

max(tour2v$velocity) #kec maks cuma 28? salah besar ini

ggmap(JawaTimur) + 
  geom_point(data = tour2v, 
             aes(x = lon, 
                 y = lat, 
                 color = velocity), 
             alpha = 0.5) + 
  theme(legend.position = "right") + 
  labs(x = "Longitude", y = "Latitude", 
       title = "Kecepatan selama touring",
       subtitle = "Touring Magetan-Surabaya",
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

#----Aktivitas selama touring----
activities <- tour1$activity
(act <- filter(tour1,
              !is.null(activity)))
(act2 <- which(!is.null(tour1$activity[[1]])))

list.condition <- sapply(activities, function(x) !is.null(x[[1]]))
activities  <- activities[list.condition]

df <- do.call("rbind", activities)
main_activity <- sapply(df$activity, function(x) x[[1]][[1]][[1]][1])

activities_2 <- data.frame(main_activity = main_activity,
                           time = as.POSIXct(as.numeric(df$timestampMs)/1000, origin = "1970-01-01"))

head(activities_2)

summary(activities_2)
