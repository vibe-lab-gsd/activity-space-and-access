options(java.parameters = '-Xmx5G')

library(tidyverse)
library(sf)
library(here)
library(r5r)
library(sfarrow)
library(maptiles)
library(tidyterra)
library(ggthemes)
library(scales)
library(ggspatial)

###################################################
## Load (and recode) origin data

origins <- here("data",
                "origins.geojson") |>
  st_read() |>
  mutate(id = as.character(id)) |>
  mutate(no_car = NumCarHH == 0) |>
  filter(area_weekday < 1000000000) |> # Removes one point 
  filter(area_weekend < 1000000000) |> # removes three points
  mutate(area_weekday = area_weekday / 1000000,
         area_weekend = area_weekend / 1000000) |>
  mutate(WorkStatus = case_when(
           LaborStatus == "Employer with employees" ~ "5. Full-time",
           LaborStatus == "Full-time salaried" ~ "5. Full-time",
           LaborStatus == 
             "Household worker, child or elder care" ~ "1. Non-worker",
           LaborStatus == "Part-time salaried" ~ "4. Part-time",
           LaborStatus == "Permanently disabled from working" ~ "1. Non-worker",
           LaborStatus == "Retired" ~ "1. Non-worker",
           LaborStatus == "Self-employed (no employees)" ~ "5. Full-time",
           LaborStatus == "Student" ~ "2. Student",
           LaborStatus == "Unemployed" ~ "3. Unemployed"),
         Income = case_when(TotalHHIncome == "From 1,100 to 1,800 Euros" ~ "3. Low Middle",
                            TotalHHIncome == "From 2,701 to 3,900 Euros" ~ "4. High Middle",
                            TotalHHIncome == "Less than 1,100 Euros" ~ "2. Low",
                            TotalHHIncome == "More than 3,900 Euros" ~ "5. High",
                            TotalHHIncome == "From 1,801 to 2,700 Euros" ~ "1. Middle")) |>
  mutate(Gender = ifelse(Gender == "Male", "1. Male", "2. Female")) |>
  select(id, 
         Age,
         no_car,
         WorkStatus,
         LivesWithSpousePartner,
         LivesWithChildren,
         LivesWithFatherMother,
         Income,
         Gender,
         area_weekday,
         area_weekend)

#############################################################################
## Count and filter out missing values by variable

sum(is.na(origins$no_car))

origins <- origins |>
  filter(!is.na(no_car))

sum(is.na(origins$WorkStatus))

origins <- origins |>
  filter(!is.na(WorkStatus))

sum(is.na(origins$Income))

origins <- origins |>
  filter(!is.na(Income)) 

######################################################################
## Load and recode destination data

destinations <- here("data",
                     "destinations.parquet") |>
  st_read_parquet() |>
  mutate(id = as.character(id)) |>
  mutate(use = ifelse(use == "culture/recreation (indoor)", "leisure", use)) |>
  mutate(use = ifelse(use == "retail/restaurant", "leisure", use)) |>
  mutate(use = ifelse(use == "culture/recreation (outdoor)", "outdoor rec", use))

##########################################################################
## Set up R5 analysis

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

######################################################################
## Calculate 12 accessibility metrics

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

stop_r5()

##################################################################
## Join accessibility to origin data and average the accessibilty scores that
## are highly correlated.

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
  mutate(car_access = (leisure_car_access_std +
                       service_car_access_std +
                       res_car_access_std +
                       outdoor_rec_car_access_std) / 4,
         no_car_access = (leisure_no_car_access_std +
                            service_no_car_access_std +
                            res_car_no_access_std +
                            outdoor_rec_no_car_access_std) / 4)

#####################################################################
## Confirm the correlations of the ones we combined.

car_access_vars <- origins_with_access |>
  st_drop_geometry() |>
  select(leisure_car_access_std,
         industrial_car_access_std,
         service_car_access_std,
         res_car_access_std,
         ag_car_access_std,
         outdoor_rec_car_access_std)

no_car_access_vars <- origins_with_access |>
  st_drop_geometry() |>
  select(leisure_no_car_access_std,
         industrial_no_car_access_std,
         service_no_car_access_std,
         res_car_no_access_std,
         ag_no_car_access_std,
         outdoor_rec_no_car_access_std)

ag_ind_vars <- origins_with_access |>
  st_drop_geometry() |>
  select(ag_no_car_access_std,
         ag_car_access_std,
         industrial_no_car_access_std,
         industrial_car_access_std)

cor(car_access_vars)

cor(no_car_access_vars)

cor(ag_ind_vars)

###########################################################################
## Accessibilty maps

## Note that this l
map_origins <- origins_with_access |>
  st_crop(c(xmin = -3.7,
            xmax = -3.5,
            ymin = 37.1,
            ymax = 37.25))

base_map <- get_tiles(map_origins,
                      provider = "Stadia.StamenTonerLines",
                      crop = TRUE,
                      zoom = 11)

ggplot(map_origins) +
  geom_spatraster_rgb(data = base_map) +
  geom_sf(data = map_origins,
          aes(color = car_access),
          size = 0.5) +
  scale_color_gradient2(limits = c(-1.5, 1.5), mid = "yellow") +
  theme_map() +
  theme(legend.position = "none") 

ggsave(here("figures", "car-access-map.png"), 
       dpi = 600, 
       width = 3, 
       height = 3, 
       units = "in")

ggplot(map_origins) +
  geom_spatraster_rgb(data = base_map) +
  geom_sf(data = map_origins,
          aes(color = no_car_access),
          size = 0.5) +
  scale_color_gradient2(limits = c(-1.5, 1.5), mid = "yellow") +
  theme_map() +
  theme(legend.position = "none")

ggsave(here("figures", "no-car-access-map.png"), 
       dpi = 600, 
       width = 3, 
       height = 3, 
       units = "in")

ggplot(map_origins) +
  geom_spatraster_rgb(data = base_map) +
  geom_sf(data = map_origins,
          aes(color = ag_no_car_access_std),
          size = 0.5) +
  scale_color_gradient2(limits = c(-1.5, 1.5), mid = "yellow") +
  theme_map() +
  theme(legend.position = "none")

ggsave(here("figures", "ag-no-car-access-map.png"), 
       dpi = 600, 
       width = 3, 
       height = 3, 
       units = "in")

ggplot(map_origins) +
  geom_spatraster_rgb(data = base_map) +
  geom_sf(data = map_origins,
          aes(color = ag_car_access_std),
          size = 0.5) +
  scale_color_gradient2(limits = c(-1.5, 1.5), mid = "yellow") +
  theme_map() +
  theme(legend.position = "none")

ggsave(here("figures", "ag-car-access-map.png"), 
       dpi = 600, 
       width = 3, 
       height = 3, 
       units = "in")

ggplot(map_origins) +
  geom_spatraster_rgb(data = base_map) +
  geom_sf(data = map_origins,
          aes(color = industrial_no_car_access_std),
          size = 0.5) +
  scale_color_gradient2(limits = c(-1.5, 1.5), mid = "yellow") +
  theme_map() +
  theme(legend.position = "none") +
  annotation_scale(location = "br",
                   pad_x = unit(0.2, "in"),
                   pad_y = unit(0.2, "in"))

ggsave(here("figures", "ind-no-car-access-map.png"), 
       dpi = 600, 
       width = 3, 
       height = 3, 
       units = "in")

ggplot(map_origins) +
  geom_spatraster_rgb(data = base_map) +
  geom_sf(data = map_origins,
          aes(color = industrial_car_access_std),
          size = 0.5) +
  scale_color_gradient2(limits = c(-1.5, 1.5), mid = "yellow",
                        name = "Accessibilty\n(standard\ndeviations\nfrom mean)") +
  theme_map() +
  theme(legend.background = element_rect(fill = alpha("gray", 0.7)))

ggsave(here("figures", "ind-car-access-map.png"), 
       dpi = 600, 
       width = 3, 
       height = 3, 
       units = "in")

####################################################################
## Descriptive statistics

mean(origins_with_access$area_weekday)
sd(origins_with_access$area_weekday)

mean(origins_with_access$area_weekend)
sd(origins_with_access$area_weekend)

mean(origins_with_access$Age)
sd(origins_with_access$Age)

mean(origins_with_access$no_car)

mean(origins_with_access$car_access)
sd(origins_with_access$car_access)

mean(origins_with_access$industrial_car_access_std)
sd(origins_with_access$industrial_car_access_std)

mean(origins_with_access$ag_car_access_std)
sd(origins_with_access$ag_car_access_std)

mean(origins_with_access$no_car_access)
sd(origins_with_access$no_car_access)

table(origins_with_access$WorkStatus) / nrow(origins_with_access)

table(origins_with_access$Gender) / nrow(origins_with_access)

table(origins_with_access$LivesWithChildren) / nrow(origins_with_access)

table(origins_with_access$LivesWithSpousePartner) / nrow(origins_with_access)

table(origins_with_access$LivesWithFatherMother) / nrow(origins_with_access)

table(origins_with_access$Income) / nrow(origins_with_access)

#################################################################
## Models

weekday_area_model <- lm(log(area_weekday) ~
                           Age +
                           Income +
                           Gender +
                           WorkStatus +
                           LivesWithChildren +
                           LivesWithSpousePartner +
                           LivesWithFatherMother +
                           no_car +
                           ag_car_access_std +
                           ag_no_car_access_std +
                           industrial_car_access_std +
                           industrial_no_car_access_std +
                           car_access +
                           no_car_access +
                           no_car:no_car_access +
                           no_car:car_access,
                         data = origins_with_access)

summary(weekday_area_model)

weekend_area_model <- lm(log(area_weekend) ~
                           Age +
                           Income +
                           Gender +
                           WorkStatus +
                           LivesWithChildren +
                           LivesWithSpousePartner +
                           LivesWithFatherMother +
                           no_car +
                           ag_car_access_std +
                           ag_no_car_access_std +
                           industrial_car_access_std +
                           industrial_no_car_access_std +
                           car_access +
                           no_car_access +
                           no_car:no_car_access +
                           no_car:car_access,
                         data = origins_with_access)

summary(weekend_area_model)

####################################################################
## Weekday prediction figure

predict_weekday <- tibble(no_car_access = c(-2, 2)) |>
  mutate(Age = mean(origins_with_access$Age),
                    Income = "1. Middle",
                    Gender = "1. Male",
                    WorkStatus = "1. Non-worker",
                    LivesWithChildren = "No",
                    LivesWithSpousePartner = "No",
                    LivesWithFatherMother = "No",
                    no_car = FALSE,
                    ag_car_access_std = 0,
                    ag_no_car_access_std = 0,
                    industrial_car_access_std = 0,
                    industrial_no_car_access_std = 0,
                    car_access = 0)

area_predict_wkday <- predict(weekday_area_model, 
                              newdata = predict_weekday, 
                              interval = "confidence") |>
  exp() 

radius_predict_wkday <- (area_predict_wkday / pi)^0.5

radius_predict_wkday

center_point <- tibble(x = -3.6,
                       y = 37.175) |>
  st_as_sf(coords = c("x", "y"),
           crs = "WGS84")

small_lwr_area <- center_point |>
  st_buffer(dist = radius_predict_wkday[4] * 1000)

small_mid_area <- center_point |>
  st_buffer(dist = radius_predict_wkday[2] * 1000)

small_upr_area <- center_point |>
  st_buffer(dist = radius_predict_wkday[6] * 1000)

big_lwr_area <- center_point |>
  st_buffer(dist = radius_predict_wkday[3] * 1000)

big_mid_area <- center_point |>
  st_buffer(dist = radius_predict_wkday[1] * 1000)

big_upr_area <- center_point |>
  st_buffer(dist = radius_predict_wkday[5] * 1000)

small_area <- st_difference(small_upr_area, small_lwr_area)
big_area <- st_difference(big_upr_area, big_lwr_area)

ggplot(small_area) + 
  geom_spatraster_rgb(data = base_map) +
  geom_sf(color = NA, aes(fill = "Two standard deviations above\nmean car-free accessibility"), alpha = 0.5) +
  geom_sf(data = small_mid_area,
          aes(color = "Two standard deviations above\nmean car-free accessibility"),
          fill = NA,
          key_glyph = "abline") +
  geom_sf(data = big_area,
          color = NA, aes(fill = "Two standard deviations below\nmean car-free accessibility"), alpha = 0.5) +
  geom_sf(data = big_mid_area,
          aes(color = "Two standard deviations below\nmean car-free accessibility"), 
          fill = NA,
          key_glyph = "abline") +
  scale_color_manual(values = c(muted("blue"), muted("red"))) +
  scale_fill_manual(values = c(muted("blue"), muted("red"))) +
  theme_map() +
  annotation_scale(location = "br") +
  theme(legend.background = element_rect(fill = alpha("gray", 0.7))) +
  labs(color  = "95-percent confidence interval\nfor predicted activity space area", 
       fill = "95-percent confidence interval\nfor predicted activity space area")

ggsave(here("figures", "weekday-comparison.png"), 
       dpi = 600, 
       width = 4, 
       height = 4, 
       units = "in")

#######################################################################
## Weekend prediction figures

predict_weekend <- tibble(no_car = c(rep(TRUE, 3), rep(FALSE, 3)),
                          Income = rep(c("1. Middle",
                                     "2. Low",
                                     "5. High"), 2)) |>
  mutate(Age = mean(origins_with_access$Age),
         Gender = "1. Male",
         WorkStatus = "1. Non-worker",
         LivesWithChildren = "No",
         LivesWithSpousePartner = "No",
         LivesWithFatherMother = "No",
         ag_car_access_std = 0,
         ag_no_car_access_std = 0,
         industrial_car_access_std = 0,
         industrial_no_car_access_std = 0,
         car_access = 0,
         no_car_access = 0)

area_predict_wkend <- predict(weekend_area_model, 
                              newdata = predict_weekend, 
                              interval = "confidence") |>
  exp() 

radius_predict_wkend <- (area_predict_wkend / pi)^0.5 |> 
  as_tibble() |>
  cbind(predict_weekend) |>
  select(no_car, Income, fit, lwr, upr)

radius_predict_wkend 

car_free_low.inc_pred <- center_point |>
  st_buffer(dist = radius_predict_wkend$fit[
    radius_predict_wkend$no_car & 
      radius_predict_wkend$Income == "2. Low"] * 1000)

car_free_mid.inc_pred <- center_point |>
  st_buffer(dist = radius_predict_wkend$fit[
    radius_predict_wkend$no_car & 
      radius_predict_wkend$Income == "1. Middle"] * 1000)

car_free_hi.inc_pred <- center_point |>
  st_buffer(dist = radius_predict_wkend$fit[
    radius_predict_wkend$no_car & 
      radius_predict_wkend$Income == "5. High"] * 1000)

car_low.inc_pred <- center_point |>
  st_buffer(dist = radius_predict_wkend$fit[
    radius_predict_wkend$no_car == FALSE & 
      radius_predict_wkend$Income == "2. Low"] * 1000)

car_mid.inc_pred <- center_point |>
  st_buffer(dist = radius_predict_wkend$fit[
    radius_predict_wkend$no_car == FALSE & 
      radius_predict_wkend$Income == "1. Middle"] * 1000)

car_hi.inc_pred <- center_point |>
  st_buffer(dist = radius_predict_wkend$fit[
    radius_predict_wkend$no_car == FALSE & 
      radius_predict_wkend$Income == "5. High"] * 1000)

car_free_low.inc_lwr <- center_point |>
  st_buffer(dist = radius_predict_wkend$lwr[
    radius_predict_wkend$no_car & 
      radius_predict_wkend$Income == "2. Low"] * 1000)

car_free_mid.inc_lwr <- center_point |>
  st_buffer(dist = radius_predict_wkend$lwr[
    radius_predict_wkend$no_car & 
      radius_predict_wkend$Income == "1. Middle"] * 1000)

car_free_hi.inc_lwr <- center_point |>
  st_buffer(dist = radius_predict_wkend$lwr[
    radius_predict_wkend$no_car & 
      radius_predict_wkend$Income == "5. High"] * 1000)

car_low.inc_lwr <- center_point |>
  st_buffer(dist = radius_predict_wkend$lwr[
    radius_predict_wkend$no_car == FALSE & 
      radius_predict_wkend$Income == "2. Low"] * 1000)

car_mid.inc_lwr <- center_point |>
  st_buffer(dist = radius_predict_wkend$lwr[
    radius_predict_wkend$no_car == FALSE & 
      radius_predict_wkend$Income == "1. Middle"] * 1000)

car_hi.inc_lwr <- center_point |>
  st_buffer(dist = radius_predict_wkend$lwr[
    radius_predict_wkend$no_car == FALSE & 
      radius_predict_wkend$Income == "5. High"] * 1000)

car_free_low.inc_upr <- center_point |>
  st_buffer(dist = radius_predict_wkend$upr[
    radius_predict_wkend$no_car & 
      radius_predict_wkend$Income == "2. Low"] * 1000)

car_free_mid.inc_upr <- center_point |>
  st_buffer(dist = radius_predict_wkend$upr[
    radius_predict_wkend$no_car & 
      radius_predict_wkend$Income == "1. Middle"] * 1000)

car_free_hi.inc_upr <- center_point |>
  st_buffer(dist = radius_predict_wkend$upr[
    radius_predict_wkend$no_car & 
      radius_predict_wkend$Income == "5. High"] * 1000)

car_low.inc_upr <- center_point |>
  st_buffer(dist = radius_predict_wkend$upr[
    radius_predict_wkend$no_car == FALSE & 
      radius_predict_wkend$Income == "2. Low"] * 1000)

car_mid.inc_upr <- center_point |>
  st_buffer(dist = radius_predict_wkend$upr[
    radius_predict_wkend$no_car == FALSE & 
      radius_predict_wkend$Income == "1. Middle"] * 1000)

car_hi.inc_upr <- center_point |>
  st_buffer(dist = radius_predict_wkend$upr[
    radius_predict_wkend$no_car == FALSE & 
      radius_predict_wkend$Income == "5. High"] * 1000)

car_free_low.inc_area <- st_difference(car_free_low.inc_upr, 
                                       car_free_low.inc_lwr)
car_free_mid.inc_area <- st_difference(car_free_mid.inc_upr, 
                                       car_free_mid.inc_lwr)
car_free_hi.inc_area <- st_difference(car_free_hi.inc_upr, 
                                      car_free_hi.inc_lwr)
car_low.inc_area <- st_difference(car_low.inc_upr, 
                                  car_low.inc_lwr)
car_mid.inc_area <- st_difference(car_mid.inc_upr, 
                                  car_mid.inc_lwr)
car_hi.inc_area <- st_difference(car_hi.inc_upr, 
                                 car_hi.inc_lwr)


ggplot(car_hi.inc_area) + 
  geom_spatraster_rgb(data = base_map) +
  geom_sf(color = NA, aes(fill = "Household has one\nor more cars"), alpha = 0.5) +
  geom_sf(data = car_hi.inc_pred,
          aes(color = "Household has one\nor more cars"),
          fill = NA,
          key_glyph = "abline") +
  geom_sf(data = car_free_hi.inc_area,
          color = NA, aes(fill = "Car-free household"), alpha = 0.5) +
  geom_sf(data = car_free_hi.inc_pred,
          aes(color = "Car-free household"), 
          fill = NA,
          key_glyph = "abline") +
  scale_color_manual(values = c(muted("blue"), muted("red"))) +
  scale_fill_manual(values = c(muted("blue"), muted("red"))) +
  theme_map() +
  annotation_scale(location = "br") +
  theme(legend.background = element_rect(fill = alpha("gray", 0.7))) +
  labs(color  = "95-percent confidence\ninterval for predicted\nactivity space area", 
       fill = "95-percent confidence\ninterval for predicted\nactivity space area")

ggsave(here("figures", "hi-inc-wkend.png"), 
       dpi = 600, 
       width = 3, 
       height = 3, 
       units = "in")

ggplot(car_mid.inc_area) + 
  geom_spatraster_rgb(data = base_map) +
  geom_sf(color = NA, aes(fill = "Household has one\nor more cars"), alpha = 0.5) +
  geom_sf(data = car_mid.inc_pred,
          aes(color = "Household has\none or more cars"),
          fill = NA,
          key_glyph = "abline") +
  geom_sf(data = car_free_mid.inc_area,
          color = NA, aes(fill = "Car-free household"), alpha = 0.5) +
  geom_sf(data = car_free_mid.inc_pred,
          aes(color = "Car-free household"), 
          fill = NA,
          key_glyph = "abline") +
  scale_color_manual(values = c(muted("blue"), muted("red"))) +
  scale_fill_manual(values = c(muted("blue"), muted("red"))) +
  theme_map() +
  theme(legend.position = "none")

ggsave(here("figures", "mid-inc-wkend.png"), 
       dpi = 600, 
       width = 3, 
       height = 3, 
       units = "in")

ggplot(car_low.inc_area) + 
  geom_spatraster_rgb(data = base_map) +
  geom_sf(color = NA, aes(fill = "Household has one\nor more cars"), alpha = 0.5) +
  geom_sf(data = car_low.inc_pred,
          aes(color = "Household has one\nor more cars"),
          fill = NA,
          key_glyph = "abline") +
  geom_sf(data = car_free_low.inc_area,
          color = NA, aes(fill = "Car-free household"), alpha = 0.5) +
  geom_sf(data = car_free_low.inc_pred,
          aes(color = "Car-free household"), 
          fill = NA,
          key_glyph = "abline") +
  scale_color_manual(values = c(muted("blue"), muted("red"))) +
  scale_fill_manual(values = c(muted("blue"), muted("red"))) +
  theme_map() +
  theme(legend.position = "none") 

ggsave(here("figures", "low-inc-wkend.png"), 
       dpi = 600, 
       width = 3, 
       height = 3, 
       units = "in")
