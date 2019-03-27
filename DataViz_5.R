library(ggplot2) # graphs
library(dplyr) # data manipulation
library(tidyr) # data formatting
library(readxl) # excel file handling
library(stringi) # string processing
library(scales) # graph scales
library(ggpubr) # graph layouts
library(ggspectra) # identify graph peaks

traffic_summary <- read.csv('TrafficCounts.csv')
eastbound_detailed <- read_excel('TrafficCounts_Detailed.xlsx', sheet = 1)
westbound_detailed <- read_excel('TrafficCounts_Detailed.xlsx', sheet = 2)


# Pre-Processing
traffic_summary <- traffic_summary %>%
  select(Date:Location) %>%
  na.omit() %>%
  mutate(OGV1 = rowSums(.[6:7])) %>% #OGV1 is both Rigid 2 Axel & Rigid 3 Axle vehicles
  rename(OGV2 = Rigid.4.Axle,
         OGV3 = X3.Axle.HGv,
         OGV4 = X4.Axle.HGV,
         OGV5 = X5.Axle.HGV) %>%
  select(-c(Rigid.2.Axle, Rigid.3.Axle)) %>%
  mutate(Date = as.Date(Date),
         Time = as.character(Time),
         Location = as.character(Location),
         OGV1 = as.integer(OGV1)) %>%
  gather(key = VehicleType, value = Count, -c("Date", "Total", "Time", "Location")) %>%
  rename(TotalVehicles = Total)

stri_sub(eastbound_detailed$Time, 3, 2) <- ":" # separate times with a semi-colon
eastbound_detailed <- eastbound_detailed %>%
  mutate(OGV1 = rowSums(.[34:35]),
         Time = as.POSIXct(strptime(Time, format="%H:%M"))) %>% 
  rename(OGV2 = Rigid.4.Axle,
         OGV3 = X3.Axle.HGv,
         OGV4 = X4.Axle.HGV,
         OGV5 = X5.Axle.HGV) %>%
  select(-c(Rigid.2.Axle, Rigid.3.Axle)) %>%
  gather(key = VehicleType, value = Count, c('Cyclist', 'M.Cycle', 'Car', 
                                             'Van', 'OGV2', 'OGV3', 
                                             'OGV4', 'OGV5', 'Bus', 'OGV1')) %>%
  rename(TotalVehicles = Total)

stri_sub(westbound_detailed$Time, 3, 2) <- ":"
westbound_detailed <- westbound_detailed %>%
  mutate(OGV1 = rowSums(.[34:35]),
         Time = as.POSIXct(strptime(Time, format="%H:%M"))) %>% 
  rename(OGV2 = Rigid.4.Axle,
         OGV3 = X3.Axle.HGv,
         OGV4 = X4.Axle.HGV,
         OGV5 = X5.Axle.HGV) %>%
  select(-c(Rigid.2.Axle, Rigid.3.Axle)) %>%
  gather(key = VehicleType, value = Count, c('Cyclist', 'M.Cycle', 'Car', 
                                           'Van', 'OGV2', 'OGV3', 
                                           'OGV4', 'OGV5', 'Bus', 'OGV1')) %>%
  rename(TotalVehicles = Total)


# the periods of traffic congestion
westbound_congestion <- westbound_detailed %>%
  filter(VehicleType == "Bus" | VehicleType == "Van" | VehicleType == "Cyclist" |
           VehicleType == "Car" | VehicleType == "OGV1") %>%
  group_by(Time, VehicleType) %>%
  summarise(Average = sum(Count) / 7) # sum vehicle counts and divide by 7 for average

eastbound_congestion <- eastbound_detailed %>%
  filter(VehicleType == "Bus" | VehicleType == "Van" | VehicleType == "Cyclist" |
           VehicleType == "Car" | VehicleType == "OGV1") %>%
  group_by(Time, VehicleType) %>%
  summarise(Average = sum(Count) / 7)

f1.1 <- ggplot(westbound_congestion, aes(x = Time, y = Average)) +
  geom_line(aes(color = VehicleType)) +
  scale_x_datetime(breaks = date_breaks("2 hour"), labels = date_format("%H:%M")) +
  labs(title = "Figure 1.1 | Westbound Traffic - Periods of Congestion",
       subtitle = "7 Day Data | Coloured By Vehicle Type",
       color = "Vehicle Type") +
  ylab("Average Number Vehicles") +
  stat_peaks(span = 101, shape = 21, size = 1.5, color = "black", fill = "white", stroke = 1) + # label peaks in traffic
  theme_linedraw() +
  scale_color_brewer(palette = "Set1")

f1.2 <- ggplot(eastbound_congestion, aes(x = Time, y = Average)) +
  geom_line(aes(color = VehicleType)) +
  scale_x_datetime(breaks = date_breaks("2 hour"), labels = date_format("%H:%M")) +
  labs(title = "Figure 1.2 | Eastbound Traffic - Periods of Congestion",
       subtitle = "7 Day Data | Coloured By Vehicle Type",
       color = "Vehicle Type",
       caption = "Galway City Council Traffic Data") +
  ylab("Average Number Vehicles") +
  stat_peaks(span = 101, shape = 21, size = 1.5, color = "black", fill = "white", stroke = 1) +
  theme_linedraw() +
  scale_color_brewer(palette = "Set1")

ggarrange(f1.1, f1.2, ncol = 1, nrow = 2, common.legend = T, legend = "right")




# the distributions of vehicle types contributing to daily traffic
total_traffic <- traffic_summary %>%
  mutate(Location = factor(Location, levels = c("Westbound", "Eastbound"))) %>%
  filter(VehicleType == "Bus" | VehicleType == "Van" | VehicleType == "Cyclist" |
          VehicleType == "Car" | VehicleType == "OGV1")

ggplot(total_traffic, aes(x = Date, y = Count, fill = VehicleType)) +
  geom_col(position = "fill", width = 0.9) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_date(breaks = date_breaks("1 day"), labels = date_format("%a\n%b %d")) +
  facet_grid(~Location) +
  labs(title = "Figure 2.1 | Distribution of Daily Traffic",
       subtitle = "7 Day Data | Coloured By Vehicle Type",
       fill = "Vehicle Type",
       caption = "Galway City Council Traffic Data") +
  ylab("Percentage Vehicles Per Day") +
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = 5, size=8, face="bold"),
        strip.text.x = element_text(size = 10, face = "bold"),
        axis.line.y= element_line(colour = "grey", size = 0.1),
        axis.text.y = element_text(size=8)) +
  scale_fill_brewer(palette = "Set1")
  







turning <- read.csv('TurningCounts.csv')


eastbound_turning <- turning %>%
  filter(Route == "B => A" | Route == "B => D" | Route == "B => C") %>%
  mutate(Bus = rowSums(.[9:11]),
         Time = as.POSIXct(strptime(TIME, format="%H:%M"))) %>%
  select(-c("TIME", "PCU", "TOT", "CDB", "BEB", "OB")) %>%
  gather(key = VehicleType, value = Count, -c("Route", "Time")) %>%
  filter(VehicleType == "Bus" | VehicleType == "Van" | VehicleType == "Cyclist" |
           VehicleType == "Car" | VehicleType == "OGV1") %>%
  group_by(Time, Route) %>%
  summarise(TotalVehicles = sum(Count))

# reorder factors levels in route based on values in TotalVehicles from smallest to largest
# required to have smallest area chart at the "front"
eastbound_turning$Route <- reorder(eastbound_turning$Route, eastbound_turning$TotalVehicles, function(x) -max(x) )


f3.1 <- ggplot(eastbound_turning, aes(x = Time, y = TotalVehicles, fill = Route)) +
  geom_area(position = "identity", color = "black", size = 0.3) +
  scale_x_datetime(breaks = date_breaks("2 hour"), labels = date_format("%H:%M")) +
  labs(title = "Figure 3.1 | Traffic Counts Turning from N59 ",
       subtitle = "22nd November 2016",
       fill = "Destination") +
  ylab("Vehicle Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2",
                    labels = c("Northern Campus/City",
                               "Central Campus/City",
                               "IDA Business Park"))

eastbound_turning_distribution <- turning %>%
  filter(Route == "B => A" | Route == "B => D" | Route == "B => C") %>%
  mutate(Bus = rowSums(.[9:11]),
         Time = as.POSIXct(strptime(TIME, format="%H:%M"))) %>%
  select(-c("TIME", "PCU", "TOT", "CDB", "BEB", "OB")) %>%
  gather(key = VehicleType, value = Count, -c("Route", "Time")) %>%
  filter(VehicleType == "Bus" | VehicleType == "Van" | VehicleType == "Cyclist" |
           VehicleType == "Car" | VehicleType == "OGV1") %>%
  group_by(Time, VehicleType) %>%
  summarise(TotalVehicles = sum(Count))

f3.2 <- ggplot(eastbound_turning_distribution, aes(x = Time, y = TotalVehicles, fill = VehicleType)) +
  geom_area(position = "fill", color = "black", size = 0.3) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_datetime(breaks = date_breaks("2 hour"), labels = date_format("%H:%M")) +
  labs(title = "Figure 3.2 | Distribution Of Traffic Turning from N59",
       subtitle = "22nd November 2016",
       fill = "Vehicle Type",
       caption = "Galway City Council Traffic Data") +
  ylab("Percentage Vehicles") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

ggarrange(f3.1, f3.2, ncol = 1, nrow = 2, common.legend = F, legend = "top")




westbound_turning <- turning %>%
  filter(Route == "A => B" | Route == "D => B" | Route == "C => B") %>%
  mutate(Bus = rowSums(.[9:11]),
         Time = as.POSIXct(strptime(TIME, format="%H:%M"))) %>%
  select(-c("TIME", "PCU", "TOT", "CDB", "BEB", "OB")) %>%
  gather(key = VehicleType, value = Count, -c("Route", "Time")) %>%
  filter(VehicleType == "Bus" | VehicleType == "Van" | VehicleType == "Cyclist" |
           VehicleType == "Car" | VehicleType == "OGV1") %>%
  group_by(Time, Route) %>%
  summarise(TotalVehicles = sum(Count))

westbound_turning$Route <- reorder(westbound_turning$Route, westbound_turning$TotalVehicles, function(x) -max(x) )

f3.3 <- ggplot(westbound_turning, aes(x = Time, y = TotalVehicles, fill = Route)) +
  geom_area(position = "identity", color = "black", size = 0.3) +
  scale_x_datetime(breaks = date_breaks("2 hour"), labels = date_format("%H:%M")) +
  labs(title = "Figure 3.3 | Traffic Counts Turning To N59",
       subtitle = "22nd November 2016",
       fill = "Origin") +
  ylab("Vehicle Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2",
                     labels = c("Central Campus/City",
                                "Northern Campus/City",
                                "IDA Business Park"))

westbound_turning_distribution <- turning %>%
  filter(Route == "A => B" | Route == "D => B" | Route == "C => B") %>%
  mutate(Bus = rowSums(.[9:11]),
         Time = as.POSIXct(strptime(TIME, format="%H:%M"))) %>%
  select(-c("TIME", "PCU", "TOT", "CDB", "BEB", "OB")) %>%
  gather(key = VehicleType, value = Count, -c("Route", "Time")) %>%
  filter(VehicleType == "Bus" | VehicleType == "Van" | VehicleType == "Cyclist" |
           VehicleType == "Car" | VehicleType == "OGV1") %>%
  group_by(Time, VehicleType) %>%
  summarise(TotalVehicles = sum(Count))


f3.4 <- ggplot(westbound_turning_distribution, aes(x = Time, y = TotalVehicles, fill = VehicleType)) +
  geom_area(position = "fill", color = "black", size = 0.3) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_datetime(breaks = date_breaks("2 hour"), labels = date_format("%H:%M")) +
  labs(title = "Figure 3.4 | Distribution Of Traffic Turning to N59",
       subtitle = "22nd November 2016",
       fill = "Vehicle Type",
       caption = "Galway City Council Traffic Data") +
  ylab("Percentage Vehicles") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

ggarrange(f3.3, f3.4, ncol = 1, nrow = 2, common.legend = F, legend = "top")


# ook at traffic turning from B to A in the and eastbound congestion to show people coming from 
#moycullen to insight and how it could remove impact on morning traffic

# do same for westbound congestion and tunring A to B and how people going home
# on the new bike path could redoce the westbound congestion in the evening

# look at the amount of people turning into the business park and suggest a bus
# could alleviate pressure because there is enough people going in
# since there s this number of people there should be a decent demand for a bus