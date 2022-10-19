~#Données du 29-09

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

# my.data.frame1 <- acled.api(
#    email.address = "okokoaime@gmail.com",
#    access.key = "izHHnDCinodsWHUHsEf0",
#    region = c(1),
#    start.date = "2022-08-01",
#    end.date = "2022-18-31")

# my.data.frame1 <- acled.api(
#   email.address = "aimokoko@yahoo.fr",
#   access.key = "xKbWUF2tzAMjIa37p!yD",
#   country = "Burkina Faso")
# 
# my.data.frame2 <- acled.api(
#   email.address = "aimokoko@yahoo.fr",
#   access.key = "xKbWUF2tzAMjIa37p!yD",
#   region = 1)

my.data.frame1 <- read.csv("C:/Users/okoko/Documents/R_train/EMC/acled.csv")[,-1]
my.data.frame2 <- read.csv("C:/Users/okoko/Documents/R_train/EMC/acled_west.csv")[,-1]

#Pays les plus attaqués par année
my.data.frame2 %>% 
  group_by(country, year) %>% count %>% 
  arrange(year, desc(n)) %>% 
  group_by(year) %>%  
  slice(1:10) %>% 
  ggplot() + geom_col(aes(fct_reorder2(country, year, n), n)) + 
  facet_wrap(~year) +
  theme(axis.text.x = element_text(angle = 45))

#Meilleures sources d'info
#Récupérer les sources
sources <- my.data.frame1 %>% 
  pull(source) %>% 
  str_split("; ")

max_sources <- sapply(sources, length) %>% sort() %>% tail(1)

source_var <- paste0("source", 1:max_sources)

source.data <- separate(my.data.frame1, source, into = source_var, sep = "; ", fill = "right") %>% 
  pivot_longer(5:(5+max_sources-1), names_to = "num_source", values_to = "Source") %>% 
  filter(!is.na(Source)) 

#plot sources
source.data %>% filter(Source != "Undisclosed Source") %>% 
  group_by(Source) %>% count %>% ungroup %>% 
  arrange(desc(n)) %>%   
  slice(1:5) %>% 
  ggplot() + geom_col(aes(reorder(Source, n), n)) + 
  theme(axis.text.x = element_text(angle = 45))

#Villes les plus attaquées
my.data.frame1 %>% 
  group_by(admin1) %>% count %>% ungroup %>% 
  arrange(desc(n)) %>%   
  slice(1:10) %>% 
  ggplot() + geom_col(aes(reorder(admin1, n), n)) + 
  theme(axis.text.x = element_text(angle = 45))




#Plus d'interaction
my.data.frame1 %>% 
  group_by(interaction) %>% count %>% ungroup %>% 
  arrange(desc(n)) %>% 
  slice(1:5) %>% 
  ggplot() + geom_col(aes(reorder(interaction, n), n)) + 
  theme(axis.text.x = element_text(angle = 45))

#Groupe le plus représenté----

#Récupérer groupes
inter_code <- readxl::read_xlsx("C:/Users/okoko/Documents/R_train/conflict_data/inter_code.xlsx")

names(inter_code$group) <-  inter_code$code

#Séparer groupes
my.data.frame2 <- my.data.frame2 %>% 
  mutate (group1 = str_sub(interaction, 1, 1),
          group2 = str_sub(interaction, 2), 
          group1 = inter_code$group[group1],
          group2 = inter_code$group[group2]) %>% 
  unite("inter", group1:group2, sep = " & \n")

write.csv(my.data.frame2, "acled_west_inter.csv")

group_var <- paste0("group", 1:2)

group.data <- my.data.frame1 %>% 
  pivot_longer(15:16, names_to = "num_group", values_to = "group") %>% 
  filter(!is.na(group)) 

#Plotting
group.data %>%  
  group_by(group) %>% count %>% ungroup %>% 
  arrange(desc(n)) %>%   
  slice(1:5) %>% 
  ggplot() + geom_col(aes(reorder(group, n), n)) + 
  theme(axis.text.x = element_text(angle = 45))

#Event type le plus courant
my.data.frame1 %>% 
  group_by(event_type) %>% count %>% ungroup %>% 
  arrange(desc(n)) %>% 
  slice(1:5) %>% 
  ggplot() + geom_col(aes(reorder(event_type, n), n)) + 
  theme(axis.text.x = element_text(angle = 10))

#Sub Event type le plus courant
my.data.frame1 %>% 
  group_by(sub_event_type) %>% count %>% ungroup %>% 
  arrange(desc(n)) %>% 
  slice(1:5) %>% 
  ggplot() + geom_col(aes(reorder(sub_event_type, n), n)) + 
  theme(axis.text.x = element_text(angle = 10))


#Evolution du nombre d'évènements par année
my.data.frame1 %>% 
  group_by(year) %>% count %>% ungroup %>% 
  ggplot() + geom_col(aes(year, n)) + 
  theme(axis.text.x = element_text(angle = 10)) +
  theme_classic()

#Evolution de la moyenne des victimes par année
my.data.frame1 %>% 
  group_by(year) %>% summarise(n = mean(fatalities)) %>% 
  ungroup %>% 
  ggplot() + geom_col(aes(year, n)) + 
  theme(axis.text.x = element_text(angle = 10))

#Evolution du nombre d'interaction (27, Rebel-civil)
  my.data.frame1 %>% 
  filter(str_detect(as.character(interaction), "2")) %>% 
  mutate(event_date = as.Date(event_date), 
         data_m = format(event_date, "%y_%m")) %>% 
  group_by(data_m) %>% summarise(n = n()) %>% 
  ggplot()+ 
  # geom_line(aes(x = data_m, y = n, group = 1))+ 
  geom_col(aes(x = data_m, y = n), alpha = .5) + 
  theme(axis.text.x = element_text(angle = 40))


#Evolution par mois par année
my.data.frame1 %>% 
  mutate(event_date = as.Date(event_date), month = month(event_date)) %>% 
  group_by(month, year) %>% summarise(n = n()) %>% 
  ggplot()+ geom_col(aes(x = month, y = n)) + 
  facet_wrap(~year) + 
  scale_x_continuous(breaks = 1:12, labels = month.name)

#Evolution par mois sur toute la période
my.data.frame1 %>% 
  mutate(event_date = as.Date(event_date), 
         data_m = format(event_date, "%y_%m")) %>% 
  group_by(data_m) %>% summarise(n = n()) %>% 
  ggplot()+ geom_line(aes(x = data_m, y = n, group = 1))+ 
  geom_col(aes(x = data_m, y = n), alpha = .5) + 
  theme(axis.text.x = element_text(angle = 40))

#Comparaison des pays les plus touchés
my.data.frame2 %>% filter(country %in% c("Mali", "Burkina Faso", "Nigeria", "Niger")) %>%
  mutate(event_date = as.Date(event_date), 
         data_m = format(event_date, "%y_%m")) %>% 
  group_by(data_m, country) %>% summarise(n = n()) %>% 
  ggplot()+ 
  geom_line(aes(x = data_m, y = n, group = country, color = country)) +
  theme(axis.text.x = element_text(angle = 40))

#Somme sur toute la zone
my.data.frame2 %>% 
  mutate(event_date = as.Date(event_date), 
         data_m = format(event_date, "%y_%m")) %>% 
  group_by(data_m, country) %>% summarise(n = n()) %>% 
  ggplot()+ 
  geom_col(aes(x = data_m, y = n, fill = country), alpha = .5) + 
  theme(axis.text.x = element_text(angle = 40))

#date, lieu, type d'event, interaction, source

#Source la plus utilisée
#Région, chef lieu ou département le plus touché
#Type ou sous type d'évènement le plus courant
# Interaction la plus courante pour chaque région

##------------------------------------------Evolution (year)
#event type
#admin

#Import boundaries
geo_data <- st_read("C:/Users/okoko/Documents/R_train/conflict_data/wca_admbnda_adm2_ocha/wca_admbnda_adm2_ocha.shp")

#Garder Burkina
geo_data.b <- geo_data %>% filter(admin0Pcod == "BF") %>% select(admin2Name,13:15)

##Détecter admin2 avec noms différents
# admin2_conf <- my.data.frame1 %>% pull(admin2) %>% unique() %>% sort
# admin2_geo <- geo_data %>% filter(admin0Pcod == "BF") %>% pull(admin2Name) %>% unique %>% sort
# setdiff(admin2_geo, admin2_conf)


#Grouper par admin2
my.data.frame1_geo <- my.data.frame1 %>% 
  group_by(admin2, year) %>% 
  summarise(n = n())

#Renommer
my.data.frame1_geo$admin2 <- ifelse(my.data.frame1_geo$admin2 == "Komandjari", 
                                "Komonjdjari", my.data.frame1_geo$admin2)
my.data.frame1_geo$admin2 <- ifelse(my.data.frame1_geo$admin2 == "Kourittenga", 
                                "Kouritenga", my.data.frame1_geo$admin2)

# my.data.frame1_geo[45,] <- list("Leraba", 0, 0)

my.data.frame1_geo[146,] <-list("Leraba", 2019, 0)
my.data.frame1_geo[147,] <-list("Leraba", 2020, 0)
my.data.frame1_geo[148,] <-list("Leraba", 2021, 0)
my.data.frame1_geo[149,] <-list("Leraba", 2022, 0)


#Nombre d'éléments par provinces
my.data.frame1_geo <- my.data.frame1_geo %>% 
  pivot_wider(names_from = year, values_from = n, values_fill = 0) %>% 
  pivot_longer(2:5, names_to = "year", values_to = "n") %>% 
  left_join(geo_data.b, by = c("admin2" = "admin2Name")) %>% 
  st_as_sf()


#plot map pour toutes années
ggplot(my.data.frame1_geo) + 
  geom_sf(aes(fill = n)) + theme_classic() + 
  facet_wrap(~year) +
  theme(axis.text = element_blank(), 
        axis.line = element_blank(), 
        axis.ticks = element_blank()) +
  scale_fill_viridis_b()

#Map Données totales
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
  scale_fill_viridis_b(breaks = c(0, 50, 100, 150))

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

