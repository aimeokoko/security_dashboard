#Load libraries

library(acled.api)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(readxl)
library(sf)
library(tmap)
library(lubridate)
library(forcats)


#############################
#Import data via Acled API
#############################

# my.data.frame1 <- acled.api(
#   email.address = "aimokoko@yahoo.fr",
#   access.key = "********************",
#   country = "Burkina Faso")
# 
# my.data.frame2 <- acled.api(
#   email.address = "aimokoko@yahoo.fr",
#   access.key = "********************",
#   region = 1)


#############################
#Store as csv and use data imported
# write.csv(my.data.frame1, "acled.csv")
# write.csv(my.data.frame2, "acled_west.csv")

#############################
#Data
#############################


#Burkina data
my.data.frame1 <- read.csv("C:/Users/okoko/Documents/R_train/EMC/acled.csv")[,-1]

#West Africa data
my.data.frame2 <- read.csv("C:/Users/okoko/Documents/R_train/EMC/acled_west.csv")[,-1]


#To find the most represented actor----

#Get groups and theirs numbers
inter_code <- readxl::read_xlsx("C:/Users/okoko/Documents/R_train/conflict_data/inter_code.xlsx")

names(inter_code$group) <-  inter_code$code

#Séparate groupes
my.data.frame2 <- my.data.frame2 %>% 
  mutate (group1 = str_sub(interaction, 1, 1),
          group2 = str_sub(interaction, 2), 
          group1 = inter_code$group[group1],
          group2 = inter_code$group[group2]) %>% 
  unite("inter", group1:group2, sep = " & \n")

#Keep the modified dataframe for further analysis
write.csv(my.data.frame2, "acled_west_inter.csv")

##############################
#Plot are first made for Burkina and then, adapted in the app for other countries
##############################

#####################
#Plots
####################

#Most attacked region in Burkina
my.data.frame1 %>% 
  group_by(admin1) %>% count %>% ungroup %>% 
  arrange(desc(n)) %>%   
  slice(1:10) %>% 
  ggplot() + geom_col(aes(reorder(admin1, n), n)) + 
  theme(axis.text.x = element_text(angle = 45))

#Most interaction in Burkina
my.data.frame1 %>% 
  group_by(interaction) %>% count %>% ungroup %>% 
  arrange(desc(n)) %>% 
  slice(1:5) %>% 
  ggplot() + geom_col(aes(reorder(interaction, n), n)) + 
  theme(axis.text.x = element_text(angle = 45))


#Most common event type
my.data.frame1 %>% 
  group_by(event_type) %>% count %>% ungroup %>% 
  arrange(desc(n)) %>% 
  slice(1:5) %>% 
  ggplot() + geom_col(aes(reorder(event_type, n), n)) + 
  theme(axis.text.x = element_text(angle = 10))

#Most common Sub Event type
my.data.frame1 %>% 
  group_by(sub_event_type) %>% count %>% ungroup %>% 
  arrange(desc(n)) %>% 
  slice(1:5) %>% 
  ggplot() + geom_col(aes(reorder(sub_event_type, n), n)) + 
  theme(axis.text.x = element_text(angle = 10))


#Events evolution by year
my.data.frame1 %>% 
  group_by(year) %>% count %>% ungroup %>% 
  ggplot() + geom_col(aes(year, n)) + 
  theme(axis.text.x = element_text(angle = 10)) +
  theme_classic()

#Fatalities evolution by year
my.data.frame1 %>% 
  group_by(year) %>% summarise(n = mean(fatalities)) %>% 
  ungroup %>% 
  ggplot() + geom_col(aes(year, n)) + 
  theme(axis.text.x = element_text(angle = 10))


#Evolution by month
my.data.frame1 %>% 
  mutate(event_date = as.Date(event_date), 
         data_m = format(event_date, "%y_%m")) %>% 
  group_by(data_m) %>% summarise(n = n()) %>% 
  ggplot()+ geom_line(aes(x = data_m, y = n, group = 1))+ 
  geom_col(aes(x = data_m, y = n), alpha = .5) + 
  theme(axis.text.x = element_text(angle = 40))

#Most attacked countries
my.data.frame2 %>% filter(country %in% c("Mali", "Burkina Faso", "Nigeria", "Niger")) %>%
  mutate(event_date = as.Date(event_date), 
         data_m = format(event_date, "%y_%m")) %>% 
  group_by(data_m, country) %>% summarise(n = n()) %>% 
  ggplot()+ 
  geom_line(aes(x = data_m, y = n, group = country, color = country)) +
  theme(axis.text.x = element_text(angle = 40))


############################
#Geographic data (from the Humanitarian Data Exchange)
############################


#Import boundaries
geo_data <- st_read("C:/Users/okoko/Documents/R_train/conflict_data/wca_admbnda_adm2_ocha/wca_admbnda_adm2_ocha.shp")

#Keep Burkina Faso
geo_data.b <- geo_data %>% 
  filter(admin0Pcod == "BF") %>% 
  select(admin2Name,13:15)

##Find admin2 with différent names 
# admin2_conf <- my.data.frame1 %>% pull(admin2) %>% unique() %>% sort
# admin2_geo <- geo_data %>% filter(admin0Pcod == "BF") %>% pull(admin2Name) %>% unique %>% sort
# setdiff(admin2_geo, admin2_conf)

#Komandjari and Kouritenga have not the same names across the two databases

#Rename provinces to match names
my.data.frame1_geo$admin2 <- ifelse(my.data.frame1_geo$admin2 == "Komandjari", 
                                    "Komonjdjari", my.data.frame1_geo$admin2)
my.data.frame1_geo$admin2 <- ifelse(my.data.frame1_geo$admin2 == "Kourittenga", 
                                    "Kouritenga", my.data.frame1_geo$admin2)

# my.data.frame1_geo[45,] <- list("Leraba", 0, 0)

#Create Leraba because it doesn't appear in the conflict database
my.data.frame1_geo[146,] <-list("Leraba", 2019, 0)
my.data.frame1_geo[147,] <-list("Leraba", 2020, 0)
my.data.frame1_geo[148,] <-list("Leraba", 2021, 0)
my.data.frame1_geo[149,] <-list("Leraba", 2022, 0)

#Summarise events by provinces 
my.data.frame1_geo <- my.data.frame1 %>% 
  group_by(admin2, year) %>% 
  summarise(n = n())

########################################
#Join geographic data to conflict data
########################################

my.data.frame1_geo <- my.data.frame1_geo %>% 
  pivot_wider(names_from = year, values_from = n, values_fill = 0) %>% 
  pivot_longer(2:5, names_to = "year", values_to = "n") %>% 
  left_join(geo_data.b, by = c("admin2" = "admin2Name")) %>% 
  st_as_sf()


#Plot map of events by regions

my.data.frame1_geo %>% 
  group_by(admin2) %>% 
  summarise(n = sum(n)) %>%  ggplot +
  geom_sf(aes(fill = n)) + 
  geom_sf_label(aes(label = admin2), size = 3, 
                label.padding =unit(.5, "mm")) +
  theme_classic() + 
  theme(axis.text = element_blank(), 
        axis.line = element_blank(), 
        axis.ticks = element_blank()) +
  scale_fill_gradient2(
    low = "grey", 
    mid = "white", 
    high = "red")
########################################"
#Conflicts with locations (With another ACLED set of data)
##########################################

bf_conflict <- read_excel("burkina-faso_conflict.xlsx")
bf_points <- bf_conflict %>% 
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

#Tmap
my.data.frame1_geo %>%
  group_by(admin2) %>%
  summarise(N = sum(n)) %>% 
  tm_shape() + 
  tm_polygons("N", alpha = .3) + 
  tm_shape(filter(bf_points, FATALITIES >= 1)) + 
  tm_dots()
