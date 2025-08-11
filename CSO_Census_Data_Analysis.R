library(tidyr)
library(dplyr)

Population_2011to2022 <- read.csv("../Datasets/CSO Census/Population_2011to2022.csv")
Population_2006 <- read.csv("../Datasets/CSO Census/Population_2006.csv")
Population_2002 <- read.csv("../Datasets/CSO Census/Population_2002.csv")



head(Population_2011to2022)
summary(Population_2011to2022)

head(Population_2006)
summary(Population_2006)

head(Population_2002)
summary(Population_2002)



Population_2011to2022 %>%
  group_by(CensusYear) %>%
  summarise(Count = sum(VALUE, na.rm = TRUE)) %>%
  arrange(desc(Count))

Population_2011to2022 %>%
  group_by(General.Health) %>%
  summarise(Count = sum(VALUE, na.rm = TRUE)) %>%
  arrange(desc(Count))

Population_2011to2022 %>%
  group_by(Sex) %>%
  summarise(Count = sum(VALUE, na.rm = TRUE)) %>%
  arrange(desc(Count))

Population_2011to2022 %>%
  group_by(County.and.City) %>%
  summarise(Count = sum(VALUE, na.rm = TRUE)) %>%
  arrange(desc(Count))%>%
  print(n = Inf)

Population_2011to2022 %>%
  group_by(Age.Group) %>%
  summarise(Count = sum(VALUE, na.rm = TRUE)) %>%
  arrange(desc(Count))




Population_2011to2022_v2 <- Population_2011to2022 %>%
  filter(
    General.Health != "General health - All",
    Sex != "Both sexes",
    County.and.City != "State",
    Age.Group != "All ages"
  )

head(Population_2011to2022_v2)



Population_2011to2022_v2 <- Population_2011to2022_v2 %>%
  mutate(
    County.and.City = case_when(
      County.and.City == "Dublin City" ~ "Dublin",
      County.and.City == "Cork City and Cork County" ~ "Cork",
      County.and.City == "Limerick City and County" ~ "Limerick",
      County.and.City == "Waterford City and County" ~ "Waterford",
      County.and.City %in% c("Galway City", "Galway County") ~ "Galway",
      TRUE ~ County.and.City 
    )
  ) %>%
  group_by(CensusYear, General.Health, Sex, County.and.City, Age.Group, Statistic.Label, UNIT) %>%
  summarise(VALUE = sum(VALUE, na.rm = TRUE), .groups = 'drop') %>%
  arrange(County.and.City, CensusYear, Age.Group)



Population_2011to2022_v2 %>%
  group_by(County.and.City) %>%
  summarise(Count = sum(VALUE, na.rm = TRUE)) %>%
  arrange(desc(Count))%>%
  print(n = Inf)


Population_2011to2022_v2 %>%
  group_by(Age.Group) %>%
  summarise(Count = sum(VALUE, na.rm = TRUE)) %>%
  arrange(desc(Count))

head(Population_2011to2022_v2)

Population_2011to2022_final <- Population_2011to2022_v2 %>%
  mutate(
    Age.Group = case_when(
      Age.Group %in% c("45 - 49 years", "50 - 54 years", "55 - 59 years", "60 - 64 years") ~ "45 to 64 years",
      Age.Group %in% c("35 - 39 years", "40 - 44 years") ~ "35 to 44 years",
      Age.Group %in% c("25 - 29 years", "30 - 34 years") ~ "25 to 34 years",
      Age.Group %in% c("18 - 19 years", "20 - 24 years") ~ "18 to 24 years",
      Age.Group %in% c("65 - 69 years", "70 - 74 years", "75 - 79 years", "80 - 84 years", "85 years and over") ~ "65 years or over",
      Age.Group %in% c("0 - 4 years", "5 - 9 years", "10 - 14 years", "15 - 19 years") ~ "Under 18",
      TRUE ~ Age.Group 
    )
  )


Population_2011to2022_final <- Population_2011to2022_final %>%
  group_by(CensusYear, General.Health, Sex, County.and.City, Age.Group, Statistic.Label, UNIT) %>%
  summarise(VALUE = sum(VALUE, na.rm = TRUE), .groups = 'drop')


head(Population_2011to2022_final)


Population_2011to2022_final %>%
  group_by(Age.Group) %>%
  summarise(Count = sum(VALUE, na.rm = TRUE)) %>%
  arrange(desc(Count))









##########################################################################


Population_2006 %>%
  group_by(Age.Group) %>%
  summarise(Count = sum(VALUE, na.rm = TRUE)) %>%
  arrange(desc(Count))

Population_2006 %>%
  group_by(Sex) %>%
  summarise(Count = sum(VALUE, na.rm = TRUE)) %>%
  arrange(desc(Count))

Population_2006 %>%
  group_by(Province.County.or.City) %>%
  summarise(Count = sum(VALUE, na.rm = TRUE)) %>%
  arrange(desc(Count))%>%
  print(n = Inf)


#######################################################################


Population_2002 %>%
  group_by(Age.Group) %>%
  summarise(Count = sum(VALUE, na.rm = TRUE)) %>%
  arrange(desc(Count))

Population_2002 %>%
  group_by(Sex) %>%
  summarise(Count = sum(VALUE, na.rm = TRUE)) %>%
  arrange(desc(Count))

Population_2002 %>%
  group_by(Province.County.or.City) %>%
  summarise(Count = sum(VALUE, na.rm = TRUE)) %>%
  arrange(desc(Count))%>%
  print(n = Inf)

########################################################################



Annual_Rate_Population_Increase <- read.csv("../Datasets/CSO Census/Annual_Rate_Population_Increase.csv")



head(Annual_Rate_Population_Increase)
summary(Annual_Rate_Population_Increase)

Annual_Rate_Population_Increase %>%
  group_by(Sex) %>%
  summarise(Count = sum(VALUE, na.rm = TRUE)) %>%
  arrange(desc(Count))

Annual_Rate_Population_Increase %>%
  group_by(Province.or.County) %>%
  summarise(Count = sum(VALUE, na.rm = TRUE)) %>%
  arrange(desc(Count))%>%
  print(n = Inf)

Annual_Rate_Population_Increase %>%
  group_by(CensusYear) %>%
  summarise(Count = sum(VALUE, na.rm = TRUE)) %>%
  arrange(desc(Count))%>%
  print(n = Inf)

Annual_Rate_Population_Increase %>%
  group_by(Statistic.Label) %>%
  summarise(Count = sum(VALUE, na.rm = TRUE)) %>%
  arrange(desc(Count))



Annual_Rate_Population_Increase_v2 <- Annual_Rate_Population_Increase %>%
  filter(
    Sex != "Both sexes",
    Province.or.County != "State",
    Statistic.Label != "Population",
    Statistic.Label != "Population Change since previous Census"
  )

###################################################################


unique(Population_2011to2022_final$General.Health)
unique(Population_2011to2022_final$CensusYear)
unique(Population_2011to2022_final$County.and.City)
unique(Population_2011to2022_final$Age.Group)

unique(Annual_Rate_Population_Increase_v2$Sex)
unique(Annual_Rate_Population_Increase_v2$Province.or.County)
unique(Annual_Rate_Population_Increase_v2$CensusYear)




################################################################





Annual_Rate_Population_Increase_v2 <- Annual_Rate_Population_Increase_v2 %>%
  rename(County.and.City = Province.or.County)

dublin_counties <- c("Dublin", "Dún Laoghaire-Rathdown", "Fingal", "South Dublin")

rates_clean <- Annual_Rate_Population_Increase_v2 %>%
  filter(CensusYear %in% c(2002, 2006, 2011, 2016) & !is.na(VALUE)) %>%
  rowwise() %>%
  mutate(County.and.City = if_else(County.and.City == "Dublin", list(dublin_counties), list(County.and.City))) %>%
  unnest(County.and.City) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = c(County.and.City, Sex),
    names_from = CensusYear,
    values_from = VALUE,
    names_prefix = "Rate_"
  ) %>%
  mutate(Rate_2022 = Rate_2016)

head(rates_clean)



###########################################################

health_proportions <- Population_2011to2022_final %>%
  group_by(CensusYear, County.and.City, Sex, Age.Group) %>%
  mutate(Total.Population.Group = sum(VALUE)) %>%
  ungroup() %>%
  mutate(Health.Proportion = VALUE / Total.Population.Group) %>%
  select(CensusYear, County.and.City, Sex, Age.Group, General.Health, Health.Proportion)


proportions_for_imputation <- tibble(Year = 2004:2023) %>%
  mutate(ReferenceYear = case_when(
    Year <= 2011 ~ 2011,
    Year <= 2016 ~ 2016,
    TRUE ~ 2022
  )) %>%
  left_join(health_proportions, by = c("ReferenceYear" = "CensusYear"))

#######################################################


total_pop_by_group <- Population_2011to2022_final %>%
  group_by(CensusYear, County.and.City, Sex, Age.Group) %>%
  summarise(Total.VALUE = sum(VALUE), .groups = 'drop')

all_years_scaffold <- expand_grid(
  Year = 2004:2023,
  distinct(Population_2011to2022_final, County.and.City, Sex, Age.Group)
)

imputed_totals <- all_years_scaffold %>%
  left_join(total_pop_by_group, by = c("Year" = "CensusYear", "County.and.City", "Sex", "Age.Group")) %>%
  left_join(rates_clean, by = c("County.and.City", "Sex")) %>%
  group_by(County.and.City, Sex, Age.Group) %>%
  arrange(Year, .by_group = TRUE)

imputed_totals <- imputed_totals %>%
  mutate(
    Growth.Factor = case_when(
      Year >= 2017 ~ 1 + (Rate_2016 / 1000),
      Year >= 2012 ~ 1 + (Rate_2016 / 1000),
      Year <= 2011 ~ 1 + (Rate_2011 / 1000),
      TRUE ~ 1
    )
  )

imputed_totals$Imputed.VALUE <- imputed_totals$Total.VALUE

for (i in 1:nrow(imputed_totals)) {
  if (!is.na(imputed_totals$Imputed.VALUE[i])) next
  
  if (i > 1 && !is.na(imputed_totals$Imputed.VALUE[i-1])) {
    if (imputed_totals$County.and.City[i] == imputed_totals$County.and.City[i-1] &&
        imputed_totals$Sex[i] == imputed_totals$Sex[i-1] &&
        imputed_totals$Age.Group[i] == imputed_totals$Age.Group[i-1]) {
      
      if(imputed_totals$Year[i] > 2011) {
        imputed_totals$Imputed.VALUE[i] <- imputed_totals$Imputed.VALUE[i-1] * imputed_totals$Growth.Factor[i]
      }
    }
  }
}

for (i in nrow(imputed_totals):1) {
  if (!is.na(imputed_totals$Imputed.VALUE[i])) next
  
  if (i < nrow(imputed_totals) && !is.na(imputed_totals$Imputed.VALUE[i+1])) {
    if (imputed_totals$County.and.City[i] == imputed_totals$County.and.City[i+1] &&
        imputed_totals$Sex[i] == imputed_totals$Sex[i+1] &&
        imputed_totals$Age.Group[i] == imputed_totals$Age.Group[i+1]) {
      
      if(imputed_totals$Year[i] < 2011) {
        imputed_totals$Imputed.VALUE[i] <- imputed_totals$Imputed.VALUE[i+1] / imputed_totals$Growth.Factor[i+1]
      }
    }
  }
}


final_imputed_dataset <- proportions_for_imputation %>%
  left_join(
    select(imputed_totals, Year, County.and.City, Sex, Age.Group, Imputed.VALUE),
    by = c("Year", "County.and.City", "Sex", "Age.Group")
  ) %>%
  mutate(VALUE = round(Imputed.VALUE * Health.Proportion)) %>%
  filter(!Year %in% c(2011, 2016, 2022)) %>%
  select(
    CensusYear = Year,
    General.Health,
    Sex,
    County.and.City,
    Age.Group,
    VALUE
  ) %>%
  mutate(
    Statistic.Label = "Population",
    UNIT = "Number"
  )

updated_population_dataset <- bind_rows(Population_2011to2022_final, final_imputed_dataset) %>%
  arrange(CensusYear, County.and.City, Sex, Age.Group, General.Health)


updated_population_dataset <- updated_population_dataset %>%
  rename(
    Year = CensusYear,
    County = County.and.City,
    Age.Range = Age.Group,
    Gender = Sex
  )

head(updated_population_dataset)

unique(updated_population_dataset$Year)
unique(updated_population_dataset$General.Health)
unique(updated_population_dataset$Gender)
unique(updated_population_dataset$County)
unique(updated_population_dataset$Age.Range)

#############################################################################

library(janitor) 

transformed_data <- updated_population_dataset %>%
  group_by(Year, Gender, County, Age.Range) %>%
  mutate(Total_Population = sum(VALUE)) %>%
  ungroup() %>%
  mutate(Percentage = (VALUE / Total_Population) * 100) %>%
  select(-VALUE, -UNIT, -Statistic.Label) %>% 
  pivot_wider(
    names_from = General.Health,
    values_from = Percentage,
    values_fill = 0 
  ) %>%
  janitor::clean_names(.) %>%

  rename(
    Year = year,         
    Gender = gender,   
    County = county,    
    Age.Range = age_range, 
    Total_Population = total_population 
  ) %>%
  rename(
    Very_Bad_General_Health = general_health_very_bad,
    Bad_General_Health = general_health_bad,
    Fair_General_Health = general_health_fair,
    Good_General_Health = general_health_good,
    Very_Good_General_Health = general_health_very_good,
    General_Health_Not_Stated = not_stated
  ) %>%
  glimpse() %>%
  select(
    Year, Gender, County, Age.Range, Total_Population,
    Very_Bad_General_Health, Bad_General_Health, Fair_General_Health,
    Good_General_Health, Very_Good_General_Health, General_Health_Not_Stated
  )

head(transformed_data)

transformed_data_formatted <- transformed_data %>%
  mutate(
    across(
      c(Very_Bad_General_Health, Bad_General_Health, Fair_General_Health,
        Good_General_Health, Very_Good_General_Health, General_Health_Not_Stated),
      ~ round(., 2) 
    )
  )

head(transformed_data_formatted)

aggregated_population_v2 <- transformed_data_formatted





#############################################################################


aggregated_population <- updated_population_dataset %>%
  
  mutate(Health.Score = case_when(
    General.Health == "General health - Very Bad"  ~ 1,
    General.Health == "General health - Bad"       ~ 2,
    General.Health == "General health - Fair"      ~ 3,
    General.Health == "General health - Good"      ~ 4,
    General.Health == "General health - Very good" ~ 5,
    TRUE                                           ~ NA_real_
  )) %>%
  
  group_by(Year, Gender, County, Age.Range) %>%
  
  summarise(
  
    Avg.Health.Score = weighted.mean(Health.Score, w = VALUE, na.rm = TRUE),
    
    Total.Population = sum(VALUE, na.rm = TRUE),
    .groups = 'drop' 
  ) %>%
  
  mutate(Average.General.Health = case_when(
    round(Avg.Health.Score) == 1 ~ "General health - Very Bad",
    round(Avg.Health.Score) == 2 ~ "General health - Bad",
    round(Avg.Health.Score) == 3 ~ "General health - Fair",
    round(Avg.Health.Score) == 4 ~ "General health - Good",
    round(Avg.Health.Score) == 5 ~ "General health - Very good",
    TRUE                         ~ "Unknown"
  )) %>%
  select(Year, Gender, County, Age.Range, Average.General.Health, Total.Population)



head(aggregated_population)