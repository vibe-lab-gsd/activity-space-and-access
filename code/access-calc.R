options(java.parameters = '-Xmx5G')

library(tidyverse)
library(sf)
library(here)
library(r5r)
library(sfarrow)

## Load origin and destination points

origins <- here("data",
                "origins.geojson") |>
  st_read() |>
  mutate(id = as.character(id)) |>
  mutate(car_ownership = case_when(NumCarHH == 0 ~ "1. No car",
                                   NumCarHH == 1 ~ "2. One car",
                                   TRUE ~ "2. Multiple cars")) |>
  filter(area_weekday < 1000000000) |> # Removes one point 
  filter(area_weekend < 1000000000) |> # removes three points
  mutate(Age_sq = Age^2,
         Education = case_when(
           Education == 
             "High School / Vocational Training (Intermediate or higher)" ~ 
             "3. High School",
           Education == "Primary education or lower" ~ "1. Primary",
           Education == "Secondary education" ~ "2. Secondary",
           Education == "University education" ~ "4. University"),
         WorkStatus = case_when(
           LaborStatus == "Employer with employees" ~ "5. Full-time",
           LaborStatus == "Full-time salaried" ~ "5. Full-time",
           LaborStatus == 
             "Household worker, child or elder care" ~ "1. Non-worker",
           LaborStatus == "Part-time salaried" ~ "4. Part-time",
           LaborStatus == "Permanently disabled from working" ~ "1. Non-worker",
           LaborStatus == "Retired" ~ "1. Non-worker",
           LaborStatus == "Self-employed (no employees" ~ "5. Full-time",
           LaborStatus == "Student" ~ "2. Student",
           LaborStatus == "Unemployed" ~ "3. Unemployed"),
         Income = case_when(TotalHHIncome == "From 1,100 to 1,800 Euros" ~ "3. Low Middle",
                            TotalHHIncome == "From 2,701 to 3,900 Euros" ~ "4. High Middle",
                            TotalHHIncome == "Less than 1,100 Euros" ~ "2. Low",
                            TotalHHIncome == "More than 3,900 Euros" ~ "5. High",
                            TRUE ~ "1. Middle")) |>
  mutate(Gender = ifelse(Gender == "Male", "1. Male", "2. Female")) |>
  select(id, 
         car_ownership,
         Age,
         Age_sq,
         Education,
         WorkStatus,
         LivesWithSpousePartner,
         LivesWithChildren,
         LivesWithFatherMother,
         Income,
         Gender,
         area_weekday,
         area_weekend,
         total_trips_workday,
         total_trips_weekend)

destinations <- here("data",
                     "destinations.parquet") |>
  st_read_parquet() |>
  mutate(id = as.character(id)) |>
  mutate(use = ifelse(use == "culture/recreation (indoor)", "leisure", use)) |>
  mutate(use = ifelse(use == "retail/restaurant", "leisure", use)) |>
  mutate(use = ifelse(use == "culture/recreation (outdoor)", "outdoor rec", use))

july_2nd_9am <- as.POSIXct(
  "02-07-2025 09:00:00",
  format = "%d-%m-%Y %H:%M:%S",
  tz = "CET"
)

granada_core <- here("data", 
                     "networks") |>
  setup_r5()

std_access = function(my_use,
                      my_origins,
                      my_destinations,
                      my_mode,
                      my_core,
                      my_date) {
  
  these_dest <- my_destinations |>
    filter(use == my_use)
  
  this_access <- accessibility(r5r_core = my_core,
                               origins = my_origins,
                               destinations = these_dest,
                               opportunities_colnames = "size",
                               decay_function = "logistic",
                               cutoffs = 30,
                               decay_value = 5,
                               mode = my_mode,
                               departure_datetime = my_date) 
  this_access |>
    mutate(center_access = accessibility - mean(accessibility),
           std_access = center_access / sd(center_access)) |>
    select(id, accessibility, std_access)

}

leisure_car <- std_access(my_use = "leisure",
                              my_origins = origins,
                              my_destinations = destinations,
                              my_mode = "CAR",
                              my_core = granada_core,
                              my_date = july_2nd_9am) |>
  rename(leisure_car_access = accessibility,
         leisure_car_access_std = std_access)

outdoor_rec_car <- std_access(my_use = "outdoor rec",
                              my_origins = origins,
                              my_destinations = destinations,
                              my_mode = "CAR",
                              my_core = granada_core,
                              my_date = july_2nd_9am) |>
  rename(outdoor_rec_car_access = accessibility,
         outdoor_rec_car_access_std = std_access)

ag_car <- std_access(my_use = "agriculture",
                     my_origins = origins,
                     my_destinations = destinations,
                     my_mode = "CAR",
                     my_core = granada_core,
                     my_date = july_2nd_9am) |>
  rename(ag_car_access = accessibility,
         ag_car_access_std = std_access)

industrial_car <- std_access(my_use = "industrial",
                             my_origins = origins,
                             my_destinations = destinations,
                             my_mode = "CAR",
                             my_core = granada_core,
                             my_date = july_2nd_9am) |>
  rename(industrial_car_access = accessibility,
         industrial_car_access_std = std_access)

service_car <- std_access(my_use = "other service",
                          my_origins = origins,
                          my_destinations = destinations,
                          my_mode = "CAR",
                          my_core = granada_core,
                          my_date = july_2nd_9am) |>
  rename(service_car_access = accessibility,
         service_car_access_std = std_access)

res_car <- std_access(my_use = "residential",
                          my_origins = origins,
                          my_destinations = destinations,
                          my_mode = "CAR",
                          my_core = granada_core,
                          my_date = july_2nd_9am) |>
  rename(res_car_access = accessibility,
         res_car_access_std = std_access)

leisure_no_car <- std_access(my_use = "leisure",
                                 my_origins = origins,
                                 my_destinations = destinations,
                                 my_mode = "TRANSIT",
                                 my_core = granada_core,
                                 my_date = july_2nd_9am) |>
  rename(leisure_no_car_access = accessibility,
         leisure_no_car_access_std = std_access)

outdoor_rec_no_car <- std_access(my_use = "outdoor rec",
                              my_origins = origins,
                              my_destinations = destinations,
                              my_mode = "TRANSIT",
                              my_core = granada_core,
                              my_date = july_2nd_9am) |>
  rename(outdoor_rec_no_car_access = accessibility,
         outdoor_rec_no_car_access_std = std_access)

ag_no_car <- std_access(my_use = "agriculture",
                     my_origins = origins,
                     my_destinations = destinations,
                     my_mode = "TRANSIT",
                     my_core = granada_core,
                     my_date = july_2nd_9am) |>
  rename(ag_no_car_access = accessibility,
         ag_no_car_access_std = std_access)

industrial_no_car <- std_access(my_use = "industrial",
                             my_origins = origins,
                             my_destinations = destinations,
                             my_mode = "TRANSIT",
                             my_core = granada_core,
                             my_date = july_2nd_9am) |>
  rename(industrial_no_car_access = accessibility,
         industrial_no_car_access_std = std_access)

service_no_car <- std_access(my_use = "other service",
                          my_origins = origins,
                          my_destinations = destinations,
                          my_mode = "TRANSIT",
                          my_core = granada_core,
                          my_date = july_2nd_9am) |>
  rename(service_no_car_access = accessibility,
         service_no_car_access_std = std_access)

res_no_car <- std_access(my_use = "residential",
                      my_origins = origins,
                      my_destinations = destinations,
                      my_mode = "TRANSIT",
                      my_core = granada_core,
                      my_date = july_2nd_9am) |>
  rename(res_car_no_access = accessibility,
         res_car_no_access_std = std_access)

origins_with_access <- origins |>
  left_join(leisure_car) |>
  left_join(industrial_car) |>
  left_join(service_car) |>
  left_join(res_car) |>
  left_join(ag_car) |>
  left_join(outdoor_rec_car) |>
  left_join(leisure_no_car) |>
  left_join(industrial_no_car) |>
  left_join(service_no_car) |>
  left_join(res_no_car) |>
  left_join(ag_no_car) |>
  left_join(outdoor_rec_no_car) |>
  st_drop_geometry() |>
  mutate(car_access = (leisure_car_access_std +
                       service_car_access_std +
                       res_car_access_std +
                       outdoor_rec_car_access_std) / 4,
         no_car_access = (leisure_no_car_access_std +
                            service_no_car_access_std +
                            res_car_no_access_std +
                            outdoor_rec_no_car_access_std) / 4)

access_vars <- origins_with_access |>
  select(leisure_car_access_std,
         leisure_no_car_access_std,
         industrial_car_access_std,
         industrial_no_car_access_std,
         service_car_access_std,
         service_no_car_access_std,
         res_car_access_std,
         res_car_no_access_std,
         ag_car_access_std,
         ag_no_car_access_std,
         outdoor_rec_car_access_std,
         outdoor_rec_no_car_access_std)

cor(access_vars)

write_csv(origins_with_access,
          here("data",
               "origins-access.csv"))
