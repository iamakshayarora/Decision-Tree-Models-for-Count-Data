#install.packages("csodata")

library(csodata)

toc <- cso_get_toc()
head(toc)

road_safety_toc <- toc[grepl("^ROA", toc$id) & !grepl("DRIV", toupper(toc$title)),]
road_safety_toc <- road_safety_toc[road_safety_toc$id != "ROA10", ]

road_safety_toc

cso_disp_meta("ROA11")

test_tbl <- cso_get_data("ROA11")

RoadFatalities <- read.csv("../Datasets/CSO Road Safety/RoadFatalities.csv")
Drivers_in_Collisions <- read.csv("../Datasets/CSO Road Safety/Drivers of Cars Involved in Fatal and Injury Collisions.csv")
Number_of_Casualties <- read.csv("../Datasets/CSO Road Safety/Number of Casualties.csv")
Car_Users_in_Collisions <- read.csv("../Datasets/CSO Road Safety/Users of Cars Involved in Fatal and Injury Collisions.csv")
Vehicles_in_Collisions <- read.csv("../Datasets/CSO Road Safety/Vehicles Involved in Fatal and Injury Collisions.csv")
Traffic_Collisions_Casualties_bycounty <- read.csv("../Datasets/CSO Road Safety/Traffic Collisions and Casualties_county.csv")
Traffic_Collisions_Casualties_byhour <- read.csv("../Datasets/CSO Road Safety/Traffic Collisions and Casualties_hour.csv")
Traffic_Collisions_Casualties_bymonth <- read.csv("../Datasets/CSO Road Safety/Traffic Collisions and Casualties_month.csv")
Traffic_Collisions_Casualties_byweek <- read.csv("../Datasets/CSO Road Safety/Traffic Collisions and Casualties_week.csv")


head(RoadFatalities)
summary(RoadFatalities)

head(Drivers_in_Collisions)
summary(Drivers_in_Collisions)

head(Number_of_Casualties)
summary(Number_of_Casualties)

head(Car_Users_in_Collisions)
summary(Car_Users_in_Collisions)

head(Vehicles_in_Collisions)
summary(Vehicles_in_Collisions)

head(Traffic_Collisions_Casualties_bycounty)
summary(Traffic_Collisions_Casualties_bycounty)

head(Traffic_Collisions_Casualties_byhour)
summary(Traffic_Collisions_Casualties_byhour)

head(Traffic_Collisions_Casualties_bymonth)
summary(Traffic_Collisions_Casualties_bymonth)

head(Traffic_Collisions_Casualties_byweek)
summary(Traffic_Collisions_Casualties_byweek)



Traffic_Collisions_Casualties_bymonth %>%
  group_by(Statistic.Label) %>%
  summarise(Total_Value = sum(VALUE, na.rm = TRUE)) %>%
  arrange(desc(Total_Value))

Traffic_Collisions_Casualties_bymonth %>%
  group_by(Month.of.Year) %>%
  summarise(Total_Value = sum(VALUE, na.rm = TRUE)) %>%
  arrange(desc(Total_Value))




Number_of_Casualties %>%
  group_by(Statistic.Label) %>%
  summarise(Total_Value = sum(VALUE, na.rm = TRUE)) %>%
  arrange(desc(Total_Value))

Number_of_Casualties %>%
  group_by(Age.Group) %>%
  summarise(Total_Value = sum(VALUE, na.rm = TRUE)) %>%
  arrange(desc(Total_Value))

Number_of_Casualties %>%
  group_by(Road.User.Type) %>%
  summarise(Total_Value = sum(VALUE, na.rm = TRUE)) %>%
  arrange(desc(Total_Value))





Drivers_in_Collisions %>%
  group_by(Statistic.Label) %>%
  summarise(Total_Value = sum(VALUE, na.rm = TRUE)) %>%
  arrange(desc(Total_Value))

Drivers_in_Collisions %>%
  group_by(Age.Group) %>%
  summarise(Total_Value = sum(VALUE, na.rm = TRUE)) %>%
  arrange(desc(Total_Value))




Car_Users_in_Collisions %>%
  group_by(Statistic.Label) %>%
  summarise(Total_Value = sum(VALUE, na.rm = TRUE)) %>%
  arrange(desc(Total_Value))

Car_Users_in_Collisions %>%
  group_by(Seat.Belt.Usage) %>%
  summarise(Total_Value = sum(VALUE, na.rm = TRUE)) %>%
  arrange(desc(Total_Value))




Vehicles_in_Collisions %>%
  group_by(Statistic.Label) %>%
  summarise(Total_Value = sum(VALUE, na.rm = TRUE)) %>%
  arrange(desc(Total_Value))

Vehicles_in_Collisions %>%
  group_by(Country.of.Residence) %>%
  summarise(Total_Value = sum(VALUE, na.rm = TRUE)) %>%
  arrange(desc(Total_Value))

Vehicles_in_Collisions %>%
  group_by(Type.of.Vehicle) %>%
  summarise(Total_Value = sum(VALUE, na.rm = TRUE)) %>%
  arrange(desc(Total_Value))
