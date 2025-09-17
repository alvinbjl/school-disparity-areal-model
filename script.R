# library ---------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(mapview)
library(bruneimap)
library(rnaturalearth)
library(rnaturalearthdata)
library(geodata)
library(sf)
library(viridis)
library(spdep)
library(INLA)
library(leaflet)
library(leaflet.extras2)
library(RColorBrewer)
#library(ggspatial)  # for north arrow and scale bar

# Data: map ---------------------------------------------------------------------
# Download Malaysia boundaries from GADM level1=States level=2Districts (Daerah)
mys_state <- geodata::gadm(country = "MYS", level = 1, path = tempdir())
mys_dis <- geodata::gadm(country = "MYS", level = 2, path = tempdir())
mys_state_sf <-
  st_as_sf(mys_state) %>%
  select(state = NAME_1) %>%
  filter(state %in% c("Sarawak", "Sabah"))
sbh_state_sf <- mys_state_sf %>% filter(state == "Sabah")
swk_state_sf <- mys_state_sf %>% filter(state == "Sarawak")
mys_dis_sf <- 
  st_as_sf(mys_dis) %>% 
  select(state = NAME_1, district = NAME_2)
sbh_dis_sf <- mys_dis_sf %>% filter(state == "Sabah")
swk_dis_sf <- mys_dis_sf %>% filter(state == "Sarawak")
east_mys_dis_sf <- mys_dis_sf %>% filter(state %in% c("Sarawak", "Sabah"))

ggplot() +
  # geom_sf(data = context, fill = "grey95", color = "grey80") +
  geom_sf(data = sbh_state_sf, aes(fill = state), color = "black", size = 1) +
  geom_sf(data = swk_state_sf, aes(fill = state), color = "black", size = 1) +
  geom_sf(data = mkm_sf, aes(fill = mukim), color = "grey", size = 0.3) +
  geom_sf(data = sbh_dis_sf, aes(fill = district), color = "grey", size = 0.3) +
  geom_sf(data = swk_dis_sf, aes(fill = district), color = "grey", size = 0.3) +
  theme(legend.position = "none") +
  scale_fill_viridis_d()
plot(swk_state_sf)

# Data: Primary & secondary Schools ----------------------------------------------------------------
mys_sch_df <- read_csv("source/mys_schools_district.csv")

sbh_sch_df <- mys_sch_df %>% 
  filter(state == "Sabah", 
         date == "2018-01-01",
         stage %in% c("primary", "secondary"),
         district != "All Districts") %>% 
  group_by(district) %>% 
  summarise(schools = sum(schools, na.rm = TRUE))
unique(sbh_sch_df$district)
unique(sbh_dis_sf$district)
setdiff(sbh_dis_sf$district, sbh_sch_df$district)
setdiff(sbh_sch_df$district, sbh_dis_sf$district)
sbh_sch_df <- sbh_sch_df %>%
  mutate(district = case_when(
    district == "Telupid" ~ "Beluran",
    TRUE ~ district
  )) %>% 
  group_by(district) %>% 
  summarise(schools = sum(schools, na.rm = TRUE))

swk_sch_df <-
  mys_sch_df %>% 
  filter(state == "Sarawak", 
         date == "2018-01-01",
         stage %in% c("primary", "secondary"),
         district != "All Districts") %>% 
  group_by(district) %>% 
  summarise(schools = sum(schools, na.rm = TRUE))
setdiff(swk_dis_sf$district, swk_sch_df$district)
setdiff(swk_sch_df$district, swk_dis_sf$district)
unique(swk_sch_df$district)
unique(swk_dis_sf$district)
swk_sch_df <- swk_sch_df %>%
  mutate(district = case_when(
    district == "Maradong" ~ "Meradong",
    district == "Kabong" ~ "Saratok",
    district == "Pusa" ~ "Saratok",
    district == "Sebauh" ~ "Bintulu",
    district == "Subis" ~ "Miri",
    TRUE ~ district
  )) %>% 
  group_by(district) %>% 
  summarise(schools = sum(schools, na.rm = TRUE))

mys_census2021 <- read_csv("source/mys_population_district.csv")
mys_census2021 <- mys_census2021 %>% 
  as_tibble() %>% 
  filter(ethnicity=="overall",
         date=="2021-01-01",
         age=="overall",
         sex=="both",
         state %in% c("Sarawak", "Sabah")) %>% 
  select(state, district, population) %>% 
  mutate(population = population*1000)
swk_census2021 <- mys_census2021 %>% 
  filter(state=="Sarawak") %>%
  select(district, population) %>% 
  mutate(district = case_when(
    district == "Maradong" ~ "Meradong",
    district == "Kabong" ~ "Saratok",
    district == "Pusa" ~ "Saratok",
    district == "Sebauh" ~ "Bintulu",
    district == "Subis" ~ "Miri",
    district == "Beluru" ~ "Miri",
    district == "Bukit Mabong" ~ "Kapit",
    district == "Matu" ~ "Mukah",
    district == "Pakan" ~ "Sarikei",
    district == "Selangau" ~ "Sibu",
    district == "Tanjung Manis" ~ "Mukah",
    district == "Tebedu" ~ "Serian",
    district == "Telang Usan" ~ "Miri",
    TRUE ~ district
  )) %>% 
  group_by(district) %>% 
  summarise(population = sum(population, na.rm = TRUE))
sbh_census2021 <- mys_census2021 %>% 
  filter(state=="Sabah") %>% 
  select(district, population) %>% 
  mutate(district = case_when(
    district == "Telupid" ~ "Beluran",
    district == "Kalabakan" ~ "Tawau",
    TRUE ~ district
  )) %>% 
  group_by(district) %>% 
  summarise(population = sum(population, na.rm = TRUE))

setdiff(sbh_sch_df$district, sbh_census2021$district)
setdiff(sbh_census2021$district, sbh_sch_df$district)
setdiff(swk_sch_df$district, swk_census2021$district)
setdiff(swk_census2021$district, swk_sch_df$district)

# sbh_sch_sf <- left_join(sbh_dis_sf, sbh_sch_df, by="district") %>% select(-state)
# swk_sch_sf <- left_join(swk_dis_sf, swk_sch_df, by="district") %>% select(-state)
sbh_sch_df <- left_join(sbh_sch_df, sbh_census2021, by="district")
swk_sch_df <- left_join(swk_sch_df, swk_census2021, by="district")
sbh_sch_sf <- left_join(sbh_dis_sf, sbh_sch_df, by="district") %>% select(-state)
swk_sch_sf <- left_join(swk_dis_sf, swk_sch_df, by="district") %>% select(-state)
east_mys_sch_sf <- rbind(sbh_sch_sf, swk_sch_sf)

mapview(east_mys_sch_sf, zcol="schools")
mapview(swk_sch_sf, zcol="schools") +
  mapview(sbh_sch_sf, zcol="schools") 

# Telupid was formerly part of the Beluran District and is now a separate 
# administrative district with Telupid Town as its capital. 

# osm doesnt give good district data
# library(osmdata)
# library(sf)
# # Example: bounding box for Malaysia
# bbox <- getbb("Malaysia")
# query <- opq(bbox) %>%
#   add_osm_feature(key = "boundary", value = "administrative") %>%
#   add_osm_feature(key = "admin_level", value = "3")  # Adjust if needed
# districts <- osmdata_sf(query)
# district_polygons <- districts$osm_multipolygons  # or $osm_polygons if needed

# Fix school on water village (slightly out of bound)
brn_sch_sf <- bruneimap::sch_sf %>% 
  mutate(district = case_when(
    School == "Sekolah Rendah Tanjong Kindana" ~ "Brunei-Muara",
    TRUE ~ district
  )) %>% 
  mutate(mukim = case_when(
    School == "Sekolah Rendah Tanjong Kindana" ~ "Mukim Kota Batu",
    TRUE ~ mukim
  ))

brn_mkm_sch_df <- brn_sch_sf %>% 
  filter(Sector == "MOE") %>% 
  select(School, Education.Level, kampong, mukim, district) %>% 
  filter(!Education.Level %in% c("Vocational / Technical Education", 
                                 "Higher Education",
                                 "Pre-primary",
                                 "Technical / Vocational Institution",
                                 "Vocational / Technical  Education")) %>% 
  group_by(mukim) %>% 
  summarise(schools = n()) %>% 
  st_drop_geometry()

brn_dis_sch_df <- brn_sch_sf %>% 
  filter(Sector == "MOE") %>%  # government schools
  select(School, Education.Level, kampong, mukim, district) %>% 
  filter(!Education.Level %in% c("Vocational / Technical Education", 
                                 "Higher Education",
                                 "Pre-primary",
                                 "Technical / Vocational Institution",
                                 "Vocational / Technical  Education")) %>% 
  group_by(district) %>% 
  summarise(schools = n()) %>% 
  st_drop_geometry()

mkm_pop <- bruneimap::census2021 %>% 
  group_by(mukim) %>% 
  summarise(population = sum(population, na.rm = TRUE))
dis_pop <- bruneimap::census2021 %>% 
  mutate(district = case_when(
    district == "Brunei Muara" ~ "Brunei-Muara",
    TRUE ~ district
  )) %>% 
  group_by(district) %>% 
  summarise(population = sum(population, na.rm = TRUE))

brn_mkm_sch_sf <- left_join(mkm_sf, brn_mkm_sch_df, by="mukim") 
brn_dis_sch_sf <- left_join(dis_sf, brn_dis_sch_df, by="district") %>% select(district, schools)
brn_mkm_sch_sf <- left_join(brn_mkm_sch_sf, mkm_pop, by="mukim")
brn_dis_sch_sf <- left_join(brn_dis_sch_sf, dis_pop, by="district")

nborneo_sch_sf <- rbind(east_mys_sch_sf, brn_dis_sch_sf)
view(nborneo_sch_sf)


# 0. EDA1: school count ------------------------------------------------------------------
nborneo_sch_sf <- nborneo_sch_sf %>% mutate(area = as.numeric(st_area(geometry)))
nborneo_sch_sf <- nborneo_sch_sf %>% mutate(sch_pop = schools/population,
                                             sch_area = schools/area)

# sch
m1 <- mapview(nborneo_sch_sf, zcol="schools")
# sch:pop
m2 <- mapview(nborneo_sch_sf, zcol="sch_pop")
#sch:area
m3 <- mapview(nborneo_sch_sf, zcol="sch_area")
#scale:
leafsync::sync(m1, m2, m3)

# needa improve map

# 0. EDA2: std_tcr --------------------------------------------------------
# student teacher ratio
brn_tchr <- bruneimap::tchr %>% 
  mutate(teachers = as.numeric(M) + as.numeric(`F`),
         district = District,
         stage = case_when(
           `Education Level` == "Primary" ~ "primary",
           `Education Level` == "Secondary" ~ "secondary",
           TRUE ~ `Education Level`)) %>% 
  filter(Sector == "MOE",
         stage %in% c("primary", "secondary")) %>% 
  select(district, stage, teachers)
  
brn_enrolment <- bruneimap::enrolment %>% 
  mutate(students = as.numeric(M) + as.numeric(`F`),
         district = District,
         stage = case_when(
           `Education Level` == "Primary" ~ "primary",
           `Education Level` == "Secondary" ~ "secondary",
           TRUE ~ `Education Level`)) %>% 
  filter(Sector == "MOE",
         stage %in% c("primary", "secondary")) %>% 
  select(district, stage, students)

mys_tchr <- read_csv("source/mys_teacher.csv")
mys_enrolment <- read_csv("source/mys_enrolment.csv")
east_mys_tchr <- mys_tchr %>% 
  filter(state %in% c("Sarawak", "Sabah"),
         stage %in% c("primary", "secondary"),
         district != "All Districts",
         sex == "both",
         date == "2018-01-01") %>% 
  select(-sex, -date, -state) %>% 
  mutate(district = case_when(
    district == "Telupid" ~ "Beluran",
    district == "Kalabakan" ~ "Tawau",
    district == "Maradong" ~ "Meradong",
    district == "Kabong" ~ "Saratok",
    district == "Pusa" ~ "Saratok",
    district == "Sebauh" ~ "Bintulu",
    district == "Subis" ~ "Miri",
    district == "Beluru" ~ "Miri",
    district == "Bukit Mabong" ~ "Kapit",
    # district == "Matu" ~ "Mukah",
    # district == "Pakan" ~ "Sarikei",
    # district == "Selangau" ~ "Sibu",
    district == "Tanjung Manis" ~ "Mukah",
    district == "Tebedu" ~ "Serian",
    district == "Telang Usan" ~ "Miri",
    TRUE ~ district
  )) %>% 
  group_by(district, stage) %>% 
  summarise(teachers = sum(teachers, na.rm = TRUE))
east_mys_enrolment <- mys_enrolment %>% 
  filter(state %in% c("Sarawak", "Sabah"),
         stage %in% c("primary", "secondary"),
         district != "All Districts",
         sex == "both",
         date == "2018-01-01") %>% 
  select(-sex, -date, -state) %>% 
  mutate(district = case_when(
    district == "Telupid" ~ "Beluran",
    district == "Kalabakan" ~ "Tawau",
    district == "Maradong" ~ "Meradong",
    district == "Kabong" ~ "Saratok",
    district == "Pusa" ~ "Saratok",
    district == "Sebauh" ~ "Bintulu",
    district == "Subis" ~ "Miri",
    district == "Beluru" ~ "Miri",
    district == "Bukit Mabong" ~ "Kapit",
    # district == "Matu" ~ "Mukah",
    # district == "Pakan" ~ "Sarikei",
    # district == "Selangau" ~ "Sibu",
    district == "Tanjung Manis" ~ "Mukah",
    district == "Tebedu" ~ "Serian",
    district == "Telang Usan" ~ "Miri",
    TRUE ~ district
  )) %>% 
  group_by(district, stage) %>% 
  summarise(students = sum(students, na.rm = TRUE))

setdiff(east_mys_tchr$district, east_mys_dis_sf$district)
setdiff(east_mys_dis_sf$district, east_mys_tchr$district)
setdiff(east_mys_enrolment$district, east_mys_dis_sf$district)
setdiff(east_mys_dis_sf$district, east_mys_enrolment$district)

# mys_enrolment[mys_enrolment$district %in% c("Matu", "Pakan", "Selangau"),]
# some changes in administartive boudnaries in 2017/2018

brn_std_tchr <- left_join(brn_enrolment, brn_tchr, by=c("district"="district", "stage"="stage"))
brn_std_tchr_sf <- brn_std_tchr %>% 
  mutate(district = case_when(
    district == "Brunei Muara" ~ "Brunei-Muara",
    TRUE ~ district
  )) %>% 
  left_join(., dis_sf, by="district") %>% 
  select(-id, -X, -Y, -perimeter, -area)

east_mys_std_tchr <- left_join(east_mys_enrolment, east_mys_tchr, by=c("district"="district", "stage"="stage"))
east_mys_std_tchr_sf <- east_mys_std_tchr %>% 
  left_join(., east_mys_dis_sf, by="district") %>% 
  select(-state)

nborneo_std_tchr_sf <- rbind(brn_std_tchr_sf, east_mys_std_tchr_sf)
nborneo_std_tchr_sf <- nborneo_std_tchr_sf %>% 
  mutate(std_tcr=students/teachers)

std_tcr_primary <- nborneo_std_tchr_sf %>%  filter(stage == "primary") %>%  st_as_sf()
std_tcr_secondary <- nborneo_std_tchr_sf %>%  filter(stage == "secondary") %>%  st_as_sf()

pal <- colorRampPalette(brewer.pal(9, "YlOrRd"))
at <- seq(min(std_tcr_primary$std_tcr), max(std_tcr_primary$std_tcr), length.out = 5)
mapview(std_tcr_primary, zcol="std_tcr", col.regions = pal, at = at)

at <- seq(min(std_tcr_secondary$std_tcr), max(std_tcr_secondary$std_tcr), length.out = 5)
mapview(std_tcr_secondary, zcol="std_tcr", col.regions = pal, at = at)
# Putatan NA <= only 1 primary school, 0 secondary

# 1. spatial autocorrelation (primary & secondary) ----------------------------------------------
# needa filter gov. schools for bn
# drop some districts in sarawak

# sarawk vs # sabah  vs #brunei



# 2. Model y ~ pop + wealth + u_i (Focus Brunei) --------------------------
# not enough good age group data
hp <- read_csv("source/brn_house_price.csv")
hp <- hp %>% 
  group_by(mukim) %>%
  summarise(price = median(price, na.rm = TRUE))
setdiff(mkm_sf$mukim, hp$mukim)

# replace NA schools <= 0
brn_mkm_sch_sf$schools[is.na(brn_mkm_sch_sf$schools)] <- 0

brn_mkm_sch_sf <- left_join(brn_mkm_sch_sf, hp, by = "mukim")
brn_mkm_sch_sf <- brn_mkm_sch_sf %>% 
  mutate(hp = price) %>% 
  select(-X, -Y, -perimeter, -area, -price, -id)

mapview(brn_mkm_sch_sf, zcol="hp")
# Issue: simulate missing data using house price using   
# Fix 1: INLA? (bad prediction, not enough covariates)
test <- brn_mkm_sch_sf
nb <- poly2nb(test)
nb2INLA("map.adj", nb)
g <- inla.read.graph(filename = "map.adj")
test$re_u <- 1:nrow(test)
test$log.hp <- log(test$hp)
formula <- log.hp ~ f(re_u, model = "bym2", graph = g)
res <- inla(formula, family="gaussian", data=test, 
            control.predictor=list(compute=TRUE),
            control.compute = list(return.marginals.predictor = TRUE))

res$summary.fitted.values$mean # values too high for forested area
test$PM <- res$summary.fitted.values[, "mean"]

# scale back to original
marginals <- lapply(res$marginals.fitted.values,
                    FUN = function(marg){inla.tmarginal(function(x) exp(x), marg)})

# Obtain summaries of the marginals with inla.zmarginal()
marginals_summaries <- lapply(marginals,
                              FUN = function(marg){inla.zmarginal(marg)})

# Posterior mean and 95% CI
test$PMoriginal <- sapply(marginals_summaries, '[[', "mean") 
test$hp[is.na(test$hp)] <- test$PMoriginal[is.na(test$hp)]

m1 <- mapview(test, zcol="PMoriginal")
m2 <- mapview(test, zcol="hp")
leafsync::sync(m1, m2)

# Option 2: simulate by guess (judging by surrounding & population)
test2 <- brn_mkm_sch_sf
t <- test2[is.na(test2$hp),]
mapview(t)

quantile(hp$price, 0.05, na.rm = TRUE)
test2 <- test2 %>% 
  mutate(hp = case_when(
    mukim == "Mukim Kuala Balai" ~ 100000,
    mukim == "Mukim Bukit Sawat" ~ 170000,
    mukim == "Mukim Ukong" ~ 180000,
    mukim == "Mukim Sukang" ~ 100000,
    mukim == "Mukim Melilas" ~ 100000,
    mukim == "Mukim Bokok" ~ 200000,
    mukim == "Mukim Labu" ~ 220000,
    mukim == "Mukim Tamoi" ~ 250000,
    mukim == "Mukim Sungai Kedayan" ~ 250000,
    mukim == "Mukim Burong Pingai Ayer" ~ 250000,
    mukim == "Mukim Peramu" ~ 250000,
    mukim == "Mukim Saba" ~ 250000,
    TRUE ~ hp
  )) 

mapview(test2, zcol="hp")

# X. option 3: using mean of neighbours, still NA, need iterative, complicated
test3 <- brn_mkm_sch_sf
nb <- poly2nb(test3)                # neighbours
lw <- nb2listw(nb, style = "W", zero.policy=TRUE)

# neighbour mean
house_price_nbmean <- lag.listw(lw, test3$hp, zero.policy=TRUE, NAOK=TRUE)

# replace NAs
test3$hp[is.na(test3$hp)] <- house_price_nbmean[is.na(test3$hp)]

# X. option 4: drop data (would drop too much info?)






# SIR (bad, over highlihts, since low school count)
test2 <- test2 %>%  filter(schools>3) # optional, to avoid extremes
test2$area <- as.numeric(st_area(test2))
test2$Y <- test2$schools
test2$E <- sum(test2$schools)/sum(test2$population) * test2$population
test2$SIR <- test2$Y/test2$E
mapview(test2, zcol="SIR")

# fit model
# Is school count too little. If poisson, ok.
view(test2)
nb <- poly2nb(test2)
nb2INLA("map.adj", nb)
g <- inla.read.graph(filename = "map.adj")
test2$re_u <- 1:nrow(test2)
test2 <- test2 %>%
  mutate(
    pop_s = population / 1000,   # per 1000 people
    area_s = as.numeric(area) / 1000000,  # e.g., km^2 instead of m^2
    hp_s = hp / 1000              # per 1000 currency units
  )

formula <- Y ~ pop_s + area_s + hp_s + f(re_u, model = "bym2", graph = g)
formula <- Y ~ pop_s + hp_s + f(re_u, model = "bym2", graph = g)
formula <- Y ~ pop_s + f(re_u, model = "bym2", graph = g)
formula <- Y ~ hp_s + f(re_u, model = "bym2", graph = g)

res <- inla(formula, family = "poisson", data = test2, E=E,
            control.predictor = list(compute = TRUE),
            control.compute = list(return.marginals.predictor = TRUE))

res$summary.fixed

test2$RA <- res$summary.fitted.values[, "mean"]

m1 <- mapview(test2, zcol = "RA")
m2 <- mapview(test2, zcol = "hp")
m3 <- mapview(test2, zcol = "population")
leafsync::sync(m1, m2, m3)
mapview(brn_mkm_sch_sf, zcol="population")

ggplot() +
  geom_sf(data = test2, aes(fill = RA)) +
  theme_bw() +
  scale_fill_gradientn(
    colours = c("red", "white", "grey"),   # multiple colors
    limits = c(0,3),
    breaks = c(0,1,2,3),
    labels = c("0","1","2","3")
  ) +
  labs(fill = "Relative Abundance")

ggplot() +
  geom_sf(data = test2, aes(fill = SIR)) +
  theme_bw() +
  scale_fill_gradientn(
    colours = c("red", "white", "grey")   # multiple colors
    # limits = c(0,3),
    # breaks = c(0,1,2,3),
    # labels = c("0","1","2","3")
  ) +
  labs(fill = "SIR")

ggplot() +
  geom_sf(data = test2, aes(fill = population)) +
  theme_bw() +
  scale_fill_gradientn(
    colours = c("red", "white", "grey")   # multiple colors
    # limits = c(0,3),
    # breaks = c(0,1,2,3),
    # labels = c("0","1","2","3")
  ) +
  labs(fill = "Population")

ggplot() +
  geom_sf(data = test2, aes(fill = hp)) +
  theme_bw() +
  scale_fill_gradientn(
    colours = c("grey", "white", "red")   # multiple colors
    # limits = c(0,3),
    # breaks = c(0,1,2,3),
    # labels = c("0","1","2","3")
  ) +
  labs(fill = "House Price")

test2$exc <- sapply(res$marginals.fitted.values,
                  FUN = function(marg){inla.pmarginal(q = 1, marginal = marg)})
ggplot() +
  geom_sf(data = test2, aes(fill = exc)) +
  theme_bw() +
  scale_fill_gradientn(
    colours = c("grey", "white", "red")   # multiple colors
    # limits = c(0,3),
    # breaks = c(0,1,2,3),
    # labels = c("0","1","2","3")
  ) +
  labs(fill = "Exceedance Probability RA < 1 ")

# still dispersed by 

# try malaysia?
mapview(nborneo_sch_sf, zcol="schools")
as.numeric(st_area(test3))

