# library -----------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(mapview)
# Reorder OpenStreetMap as default
all_basemaps <- c("OpenStreetMap", 
                  "CartoDB.Positron",  
                  "CartoDB.DarkMatter", 
                  "Esri.WorldImagery", 
                  "Esri.WorldStreetMap") 
# Set default basemap to OSM
mapviewOptions(basemaps = all_basemaps)
mapviewOptions(fgb = FALSE)
library(bruneimap)
library(geodata)
library(sf)
library(viridis)
library(spdep)
library(INLA)
library(leaflet)
library(leaflet.extras2)
library(RColorBrewer)
library(patchwork)
set.seed(123)
# EXTRA: Map-malaysia ---------------------------------------------------------------------
# Download Malaysia boundaries from GADM level1=States level=2Districts (Daerah)
mys_state <- geodata::gadm(country = "MYS", level = 1, path = tempdir())
mys_dis <- geodata::gadm(country = "MYS", level = 2, path = tempdir())
# Filter only North Borneo
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

# EXTRA: Data-MYS-Primary & secondary Schools, Population, sf ----------------------------------------------------------------
# A. Malaysia
mys_sch_df <- read_csv("source/mys_schools_district.csv")

sbh_sch_df <- mys_sch_df %>% 
  filter(state == "Sabah", 
         date == "2018-01-01",
         stage %in% c("primary", "secondary"),
         district != "All Districts") %>% 
  group_by(district) %>% 
  summarise(schools = sum(schools, na.rm = TRUE))

# Telupid was formerly part of the Beluran District and is now a separate 
# administrative district with Telupid Town as its capital. 
# Fix: merge to old map
sbh_sch_df <- sbh_sch_df %>%
  mutate(district = case_when(
    district == "Telupid" ~ "Beluran",
    TRUE ~ district
  )) %>% 
  group_by(district) %>% 
  summarise(schools = sum(schools, na.rm = TRUE))
setdiff(sbh_dis_sf$district, sbh_sch_df$district)
setdiff(sbh_sch_df$district, sbh_dis_sf$district)

swk_sch_df <-
  mys_sch_df %>% 
  filter(state == "Sarawak", 
         date == "2018-01-01",
         stage %in% c("primary", "secondary"),
         district != "All Districts") %>% 
  group_by(district) %>% 
  summarise(schools = sum(schools, na.rm = TRUE))

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
setdiff(swk_dis_sf$district, swk_sch_df$district)
setdiff(swk_sch_df$district, swk_dis_sf$district)

# Join Population
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

# Join sf
sbh_sch_df <- left_join(sbh_sch_df, sbh_census2021, by="district")
swk_sch_df <- left_join(swk_sch_df, swk_census2021, by="district")
sbh_sch_sf <- left_join(sbh_dis_sf, sbh_sch_df, by="district") %>% select(-state)
swk_sch_sf <- left_join(swk_dis_sf, swk_sch_df, by="district") %>% select(-state)
east_mys_sch_sf <- rbind(sbh_sch_sf, swk_sch_sf)

mapview(east_mys_sch_sf, zcol="schools")



# MAIN: Data-Brunei-Primary & secondary Schools, Population, sf ----------
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

# Filter Only Primary, Secondary Government Schools
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

# Join population
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

# Join sf
brn_mkm_sch_sf <- left_join(mkm_sf, brn_mkm_sch_df, by="mukim") 
brn_dis_sch_sf <- left_join(dis_sf, brn_dis_sch_df, by="district") %>% select(district, schools)
brn_mkm_sch_sf <- left_join(brn_mkm_sch_sf, mkm_pop, by="mukim")
brn_dis_sch_sf <- left_join(brn_dis_sch_sf, dis_pop, by="district")

# C. North Borneo
nborneo_sch_sf <- rbind(east_mys_sch_sf, brn_dis_sch_sf)


# MAIN: EDA1-spatial count ----------------------------------------------------
mv1 <- mapview(brn_mkm_sch_sf , zcol = "schools", col.region=pal, layer.name="schools")
mv1

#ggplot
label_sf <- brn_mkm_sch_sf |> 
  arrange(desc(schools)) |> 
  slice_head(n = 5) |> 
  mutate(label = paste0(mukim, "\n", schools))
ggplot() +
  geom_sf(data = brn_mkm_sch_sf, aes(fill = schools)) +
  geom_sf(data = mkm_sf, color="grey", alpha=0, linewidth=0.7) +
  geom_sf(data = dis_sf, color="red", alpha=0, linewidth=0.5) +
  ggrepel::geom_label_repel(
    data = label_sf,
    aes(label = label, geometry = geometry),
    stat = "sf_coordinates",
    inherit.aes = FALSE,
    box.padding = 1,
    size = 3,
    alpha = 0.7,
    force=5,
    max.overlaps = Inf
  ) +
  scale_fill_viridis_b(
    option = "E",
    direction = 1,
    name = "Schools Count",
    na.value = NA,
    breaks = c(0,2,4,6,8)    # Number of bins
  ) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(legend.position = "top")

# EXTRA: EDA2-school count ------------------------------------------------------------------
nborneo_sch_sf <- nborneo_sch_sf %>% mutate(area = as.numeric(st_area(geometry)))
nborneo_sch_sf <- nborneo_sch_sf %>% mutate(sch_pop = schools/population*100000, # per 10000 people
                                            sch_area = schools/area * 10000000) # per 10 km^2

pal <- colorRampPalette(brewer.pal(9, "YlOrRd"))
# sch
m1 <- mapview(nborneo_sch_sf, zcol="schools", col.regions = pal, layer.name="School Count")
# sch:pop per 1000
m2 <- mapview(nborneo_sch_sf, zcol="sch_pop", col.regions = pal, layer.name="School per 10000 people")
#sch:area
m3 <- mapview(nborneo_sch_sf, zcol="sch_area", col.regions = pal, layer.name="School per 10 km^2")
leafsync::sync(m1, m2, m3)

# m1: sch
label_sf <- nborneo_sch_sf |> 
  arrange(desc(schools)) |> 
  slice_head(n = 5) |> 
  mutate(label = paste0(district, "\n", schools))

ggplot(nborneo_sch_sf) +
  geom_histogram(aes(schools), binwidth = 10)

m1 <- ggplot() +
  geom_sf(data = nborneo_sch_sf, aes(fill = schools)) +
  geom_sf(data = filter(brn_sf, name=="Mainland"), color="black", alpha=0, linewidth=0.7)+
  ggrepel::geom_label_repel(
    data = label_sf,
    aes(label = label, geometry = geometry),
    stat = "sf_coordinates",
    inherit.aes = FALSE,
    box.padding = 1,
    size = 3,
    alpha = 0.7,
    max.overlaps = Inf
  ) +
  scale_fill_viridis_b(
    option = "E",
    name = "School Count",
    na.value = NA,
    breaks = c(0,20,50,75,100)        # Number of bins
  ) +
  labs(x = NULL, y = NULL) +
  theme_minimal() 

# m2: population
label_sf <- nborneo_sch_sf |> 
  arrange(desc(population)) |> 
  slice_head(n = 6) |> 
  mutate(label = paste0(district, "\n", format(population, big.mark=",", scientific=F)))

ggplot(nborneo_sch_sf) +
  geom_histogram(aes(population), binwidth = 10000)

m2 <- ggplot(nborneo_sch_sf) +
  geom_sf(data = nborneo_sch_sf, aes(fill = population)) +
  geom_sf(data = filter(brn_sf, name=="Mainland"), color="black", alpha=0, linewidth=0.7)+
  ggrepel::geom_label_repel(
    data = label_sf,
    aes(label = label, geometry = geometry),
    stat = "sf_coordinates",
    inherit.aes = FALSE,
    box.padding = 1,
    size = 3,
    alpha = 0.7,
    force=5,
    max.overlaps = Inf
  ) +
  scale_fill_viridis_b(
    option = "E",
    name = "Population",
    labels = scales::comma,
    na.value = NA,
    breaks = c(0,30000,70000,100000, 200000,400000)        # Number of bins
  ) +
  labs(x = NULL, y = NULL) +
  theme_minimal() 

#m3: sch_area
label_sf <- nborneo_sch_sf |> 
  arrange(desc(sch_area)) |> 
  slice_head(n = 5) |> 
  mutate(label = paste0(district, "\n", round(sch_area,2)))

ggplot(nborneo_sch_sf) +
  geom_histogram(aes(sch_area), binwidth = 0.01)

m3 <- ggplot() +
  geom_sf(data= nborneo_sch_sf, aes(fill = sch_area)) +
  geom_sf(data = filter(brn_sf, name=="Mainland"), color="black", alpha=0, linewidth=0.7)+
  ggrepel::geom_label_repel(
    data = label_sf,
    aes(label = label, geometry = geometry),
    stat = "sf_coordinates",
    inherit.aes = FALSE,
    box.padding = 1,
    size = 3,
    alpha = 0.7,
    force=5,
    max.overlaps = Inf
  ) +
  scale_fill_viridis_b(
    option = "E",
    name = "School per 10 km^2",
    na.value = NA,
    breaks = c(0,0.1,0.2,0.3,0.5,1)        # Number of bins
  ) +
  labs(x = NULL, y = NULL) +
  theme_minimal() 

# m4: sch_pop
label_sf <- nborneo_sch_sf |> 
  arrange(sch_pop) |> 
  slice_head(n = 5) |> 
  mutate(label = paste0(district, "\n", round(sch_pop,1)))

ggplot(nborneo_sch_sf) +
  geom_histogram(aes(sch_pop), binwidth = 5)

m4 <- ggplot() +
  geom_sf(data = nborneo_sch_sf, aes(fill = sch_pop)) +
  geom_sf(data = filter(brn_sf, name=="Mainland"), color="black", alpha=0, linewidth=0.7) +
  ggrepel::geom_label_repel(
    data = label_sf,
    aes(label = label, geometry = geometry),
    stat = "sf_coordinates",
    inherit.aes = FALSE,
    box.padding = 1,
    size = 3,
    alpha = 0.7,
    force=5,
    max.overlaps = Inf
  ) +
  scale_fill_viridis_b(
    option = "E",
    name = "School per 10000 people",
    na.value = NA,
    breaks = c(0,50,100,200)        # Number of bins
  ) +
  labs(x = NULL, y = NULL) +
  theme_minimal()

# 2x1 grid
(m1 | m3) 
# 2x1 gird
(m2 | m4)
# EXTRA: EDA3-std_tcr --------------------------------------------------------
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

at <- seq(min(std_tcr_primary$std_tcr), max(std_tcr_primary$std_tcr), length.out = 5)
mapview(std_tcr_primary, zcol="std_tcr", col.regions = pal, at = at,
        color = "#3b3b3b",
        layer.name="Primary - std:tcr",
        highlight = leaflet::highlightOptions(weight = 4),
        popup = leafpop::popupTable(dplyr::mutate_if(std_tcr_primary, is.numeric,
                                                     round, digits = 2),
                                    zcol = c("district", "students", "teachers", "std_tcr"),
                                    row.numbers = FALSE, feature.id = FALSE))

at <- seq(min(std_tcr_secondary$std_tcr, na.rm = TRUE), 
          max(std_tcr_secondary$std_tcr, na.rm = TRUE), length.out = 5)
mapview(std_tcr_secondary, zcol="std_tcr", col.regions = pal, at = at,
        color = "#3b3b3b",
        layer.name="Secondary - std:tcr",
        highlight = leaflet::highlightOptions(weight = 4),
        popup = leafpop::popupTable(dplyr::mutate_if(std_tcr_secondary, is.numeric,
                                                     round, digits = 2),
                                    zcol = c("district", "students", "teachers", "std_tcr"),
                                    row.numbers = FALSE, feature.id = FALSE))
# Putatan NA <= only 1 primary school, 0 secondary

# ggplot
label_sf <- std_tcr_primary |> 
  arrange(std_tcr) |> 
  slice_head(n = 5) |> 
  mutate(label = paste0(district, "\n", round(std_tcr,1)))

ggplot(std_tcr_primary) + 
  geom_histogram(aes(std_tcr), binwidth = 1)

m5 <- ggplot() +
  geom_sf(data = std_tcr_primary, aes(fill = std_tcr)) +
  geom_sf(data = filter(brn_sf), color="black", alpha=0, linewidth=0.7) +
  ggrepel::geom_label_repel(
    data = label_sf,
    aes(label = label, geometry = geometry),
    stat = "sf_coordinates",
    inherit.aes = FALSE,
    box.padding = 1,
    size = 3,
    alpha = 0.7,
    force=5,
    max.overlaps = Inf
  ) +
  scale_fill_viridis_b(
    option = "E",
    name = "Primary std-tcr ratio",
    na.value = NA,
    breaks = c(0,8,10,12,14)        # Number of bins
  ) +
  labs(x = NULL, y = NULL) +
  theme_minimal()

label_sf <- std_tcr_secondary |> 
  arrange(std_tcr) |> 
  slice_head(n = 5) |> 
  mutate(label = paste0(district, "\n", round(std_tcr,1)))

ggplot(std_tcr_secondary) + 
  geom_histogram(aes(std_tcr), binwidth = 1)

m6 <- ggplot() +
  geom_sf(data = std_tcr_secondary, aes(fill = std_tcr)) +
  geom_sf(data = filter(brn_sf, name=="Mainland"), color="black", alpha=0, linewidth=0.7) +
  ggrepel::geom_label_repel(
    data = label_sf,
    aes(label = label, geometry = geometry),
    stat = "sf_coordinates",
    inherit.aes = FALSE,
    box.padding = 1,
    size = 3,
    alpha = 0.7,
    force=5,
    max.overlaps = Inf
  ) +
  scale_fill_viridis_b(
    option = "E",
    name = "Secondary std-tcr ratio",
    na.value = NA,
    breaks = c(0,8,10,12,14)        # Number of bins
  ) +
  labs(x = NULL, y = NULL) +
  theme_minimal()

m5 + m6



# MAIN: Model y ~ pop + hp (socioeconomic) + u_i +v_i (Only Brunei, by mukim) --------------------------
# hp
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





# Fix missing hp using INLA (gaussian)
# Other options: 
# educated guess based on location & population
test <- brn_mkm_sch_sf #refer to appendix
#   Alt 1. drop missing data (would lost info)
#   Alt 2. Fill in using mean of neighbours (need loop, complicated, some neighbours also NA)
nb <- poly2nb(brn_mkm_sch_sf)
nb2INLA("map.adj", nb)
g <- inla.read.graph(filename = "map.adj")
brn_mkm_sch_sf$re_u <- 1:nrow(brn_mkm_sch_sf)
brn_mkm_sch_sf$log.hp <- log(brn_mkm_sch_sf$hp)
formula <- log.hp ~ f(re_u, model = "bym2", graph = g)
res <- inla(formula, family="gaussian", data=brn_mkm_sch_sf, 
            control.predictor=list(compute=TRUE),
            control.compute = list(return.marginals.predictor = TRUE))

res$summary.fitted.values$mean # values too high for forested area
brn_mkm_sch_sf$PM <- res$summary.fitted.values[, "mean"]

# Transformation marginals with inla.tmarginal()
marginals <- lapply(res$marginals.fitted.values,
                    FUN = function(marg){inla.tmarginal(function(x) exp(x), marg)})

# Obtain summaries of the marginals with inla.zmarginal()
marginals_summaries <- lapply(marginals,
                              FUN = function(marg){inla.zmarginal(marg)})

# Posterior mean
brn_mkm_sch_sf$PMoriginal <- sapply(marginals_summaries, '[[', "mean") 
# Replace missing hp with predicted
brn_mkm_sch_sf$hp[is.na(brn_mkm_sch_sf$hp)] <- brn_mkm_sch_sf$PMoriginal[is.na(brn_mkm_sch_sf$hp)]






# SIR (bad, overhighlihts, since low school count)
# brn_mkm_sch_sf <- brn_mkm_sch_sf %>%  filter(schools>3) # optional, to avoid extremes
brn_mkm_sch_sf$area <- as.numeric(st_area(brn_mkm_sch_sf))
brn_mkm_sch_sf$Y <- brn_mkm_sch_sf$schools
brn_mkm_sch_sf$E <- sum(brn_mkm_sch_sf$schools)/sum(brn_mkm_sch_sf$population) * brn_mkm_sch_sf$population
brn_mkm_sch_sf$SIR <- brn_mkm_sch_sf$Y/brn_mkm_sch_sf$E
at <- c(0,0.5,1,2,3,4,10)
mapview(brn_mkm_sch_sf, zcol="SIR", col.region=pal, at=at, layer.name="SIR") # bad, overhighlihts, since low school count

label_sf <- brn_mkm_sch_sf |> 
  filter(SIR!=0) |> 
  arrange(SIR) |> 
  slice_head(n = 5) |> 
  mutate(label = paste0(mukim, "\n", round(SIR,2)))

ggplot(brn_mkm_sch_sf) + 
  geom_histogram(aes(SIR), binwidth = 1)

ggplot() +
  geom_sf(data = brn_mkm_sch_sf, aes(fill = SIR)) +
  geom_sf(data = filter(mkm_sf), color="grey", alpha=0, linewidth=0.7) +
  ggrepel::geom_label_repel(
    data = label_sf,
    aes(label = label, geometry = geometry),
    stat = "sf_coordinates",
    inherit.aes = FALSE,
    box.padding = 1,
    size = 3,
    alpha = 0.7,
    force=5,
    max.overlaps = Inf
  ) +
  scale_fill_viridis_b(
    option = "E",
    direction = -1,
    name = "SIR",
    na.value = NA,
    breaks = c(0,1,2,3)    # Number of bins
  ) +
  labs(x = NULL, y = NULL) +
  theme_minimal()



# fit model y ~ pop + area + hp (socioeconomic) + u_i +v_i
# Concern: Is school count too little. If poisson, ok.
nb <- poly2nb(brn_mkm_sch_sf)
nb2INLA("map.adj", nb)
g <- inla.read.graph(filename = "map.adj")
brn_mkm_sch_sf$re_u <- 1:nrow(brn_mkm_sch_sf)
brn_mkm_sch_sf <- brn_mkm_sch_sf %>%
  mutate(
    pop_s = population / 10000,   # per 10000 people
    area_s = as.numeric(area) / 10000000,  # e.g., 10 km^2 instead of m^2
    hp_s = hp / 1000000              # per million $
  )

formula <- Y ~ pop_s + area_s + hp_s + f(re_u, model = "bym2", graph = g)
# formula <- Y ~ pop_s + hp_s + f(re_u, model = "bym2", graph = g)
# formula <- Y ~ pop_s + f(re_u, model = "bym2", graph = g)
# formula <- Y ~ hp_s + f(re_u, model = "bym2", graph = g)

res <- inla(formula, family = "poisson", data = brn_mkm_sch_sf, E=E,
            control.predictor = list(compute = TRUE),
            control.compute = list(return.marginals.predictor = TRUE),
            verbose = TRUE)
summary(res)
res$summary.fixed
res$summary.fitted.values$mean
res$summary.linear.predictor$mean
all.equal(exp(res$summary.linear.predictor$mean),
          res$summary.fitted.values$mean)

brn_mkm_sch_sf$RA <- res$summary.fitted.values[, "mean"]

m1 <- mapview(brn_mkm_sch_sf, zcol = "RA", col.region=pal, at=at, layer.name="RA")
m2 <- mapview(brn_mkm_sch_sf, zcol = "hp", col.region=pal, layer.name="House Price")
at <- c(0,100,1000,10000,20000,45000)
m3 <- mapview(brn_mkm_sch_sf, zcol = "population", col.region=pal, at=at, layer.name="Population")
mv2 <- leafsync::sync(m1, m2, m3, mv1)
mv2

label_sf <- brn_mkm_sch_sf |> 
  filter(RA!=0) |> 
  arrange(RA) |> 
  slice_head(n = 5) |> 
  mutate(label = paste0(mukim, "\n", round(RA,2)))
ggplot() +
  geom_sf(data = brn_mkm_sch_sf, aes(fill = RA)) +
  geom_sf(data = mkm_sf, color="grey", alpha=0, linewidth=0.7) +
  ggrepel::geom_label_repel(
    data = label_sf,
    aes(label = label, geometry = geometry),
    stat = "sf_coordinates",
    inherit.aes = FALSE,
    box.padding = 1,
    size = 3,
    alpha = 0.7,
    force=5,
    max.overlaps = Inf
  ) +
  scale_fill_viridis_b(
    option = "E",
    direction = -1,
    name = "RR",
    na.value = NA,
    breaks = c(0,1,2,3)    # Number of bins
  ) +
  labs(x = NULL, y = NULL) +
  theme_minimal()



# Use Exceedance Prob.
brn_mkm_sch_sf$exc <- sapply(res$marginals.fitted.values,
                    FUN = function(marg){inla.pmarginal(q = 0.7, marginal = marg)})

at <- c(0,0.25,0.5,0.75,1)
sch_sf <- sch_sf |> filter(Sector == "MOE",
                           !Education.Level %in% c("Vocational / Technical Education", 
                                         "Higher Education",
                                         "Pre-primary",
                                         "Technical / Vocational Institution",
                                         "Vocational / Technical  Education")) 
mv3 <- mapview(brn_mkm_sch_sf, zcol = "exc", col.region=pal, at=at, 
        layer.name="Non-Exceedance Probability RA < 0.7") + 
        mapview(sch_sf, cex=4)
mv3

label_sf <- brn_mkm_sch_sf |> 
  arrange(desc(exc)) |> 
  slice_head(n = 6) |> 
  mutate(label = paste0(mukim, "\n", round(exc,2)))
ggplot() +
  geom_sf(data = brn_mkm_sch_sf, aes(fill = exc)) +
  geom_sf(data = mkm_sf, color="grey", alpha=0, linewidth=0.7) +
  geom_sf(data = sch_sf) +
  ggrepel::geom_label_repel(
    data = label_sf,
    aes(label = label, geometry = geometry),
    stat = "sf_coordinates",
    inherit.aes = FALSE,
    box.padding = 1,
    size = 3,
    alpha = 0.7,
    force=5,
    max.overlaps = Inf
  ) +
  scale_fill_viridis_b(
    option = "E",
    direction = 1,
    name = "Exceedance Probability RA < 0.8",
    na.value = NA,
    breaks = c(0,0.25,0.5,0.75)    # Number of bins
  ) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(legend.position = "top")

brn_mkm_sch_sf$exc2 <- sapply(res$marginals.fitted.values,
                             FUN = function(marg){1 - inla.pmarginal(q = 1, marginal = marg)})

at <- c(0,0.25,0.5,0.75,1)
mapview(brn_mkm_sch_sf, zcol = "exc2", col.region=pal, at=at, 
        layer.name="Exceedance Probability RA > 1.2")

fitted_counts <- brn_mkm_sch_sf$E * brn_mkm_sch_sf$RA
brn_mkm_sch_sf$residuals_pearson <- (brn_mkm_sch_sf$Y - fitted_counts) / sqrt(fitted_counts)
# 3.2 Convert to spatial weights list
lw <- nb2listw(nb, style = "W")
# 3.3 Run Moran’s I test on residuals
moran.test(residuals_pearson, lw)

my_palette <- colorRampPalette(brewer.pal(11, "RdBu"))(100)
mapview(brn_mkm_sch_sf, zcol="residuals_pearson", col.regions = my_palette, at = seq(-1, 3, length.out = 101))
mv4 <- ggplot() +
  geom_sf(data = brn_mkm_sch_sf, aes(fill = residuals_pearson)) +
  geom_sf(data = mkm_sf, color="grey", alpha=0, linewidth=0.7) +
  scale_fill_viridis_b(
    option = "E",
    direction = 1,
    name = "Residual",
    na.value = NA,
    n.breaks = 6    # Number of bins
  ) +
  labs(x = NULL, y = NULL) +
  theme_minimal()
mv4

save(mv1, mv2, mv3, mv4, file = "presentation_maps.RData")

# ARCHIVED- --------------------------------------------------------------------------------------
# Sarawak
# view(swk_sch_sf)
# swk_sch_sf_clean <- swk_sch_sf[complete.cases(st_drop_geometry(swk_sch_sf)), ]
# 
# swk_sch_sf_clean$area <- as.numeric(st_area(swk_sch_sf_clean))
# swk_sch_sf_clean$Y <- swk_sch_sf_clean$schools
# swk_sch_sf_clean$E <- sum(swk_sch_sf_clean$schools)/sum(swk_sch_sf_clean$population) * swk_sch_sf_clean$population
# nb <- poly2nb(swk_sch_sf_clean)
# nb2INLA("map.adj", nb)
# g <- inla.read.graph(filename = "map.adj")
# swk_sch_sf_clean$re_u <- 1:nrow(swk_sch_sf_clean)
# swk_sch_sf_clean <- swk_sch_sf_clean %>%
#   mutate(
#     pop_s = population / 10000,   # per 10000 people
#     area_s = as.numeric(area) / 10000000  # e.g., 10 km^2 instead of m^2
#   )
# 
# formula <- Y ~ pop_s + area_s  + f(re_u, model = "bym2", graph = g)
# # formula <- Y ~ pop_s + hp_s + f(re_u, model = "bym2", graph = g)
# # formula <- Y ~ pop_s + f(re_u, model = "bym2", graph = g)
# # formula <- Y ~ hp_s + f(re_u, model = "bym2", graph = g)
# 
# res <- inla(formula, family = "poisson", data = swk_sch_sf_clean, E=E,
#             control.predictor = list(compute = TRUE),
#             control.compute = list(return.marginals.predictor = TRUE),
#             verbose = TRUE)
# summary(res)
# res$summary.fixed
# 
# swk_sch_sf_clean$RA <- res$summary.fitted.values[, "mean"]
# 
# m1 <- mapview(swk_sch_sf_clean, zcol = "RA", col.region=pal, at=at)
# m1
# swk_sch_sf_clean$exc <- sapply(res$marginals.fitted.values,
#                              FUN = function(marg){inla.pmarginal(q = 0.7, marginal = marg)})
# 
# at <- c(0,0.25,0.5,0.75,1)
# mapview(swk_sch_sf_clean, zcol = "exc", col.region=pal, at=at, 
#         layer.name="Exceedance Probability RA < 0.8")
# 
# # Sabah
# # view(swk_sch_sf)
# sbh_sch_sf_clean <- sbh_sch_sf[complete.cases(st_drop_geometry(sbh_sch_sf)), ]
# 
# sbh_sch_sf_clean$area <- as.numeric(st_area(sbh_sch_sf_clean))
# sbh_sch_sf_clean$Y <- sbh_sch_sf_clean$schools
# sbh_sch_sf_clean$E <- sum(sbh_sch_sf_clean$schools)/sum(sbh_sch_sf_clean$population) * sbh_sch_sf_clean$population
# nb <- poly2nb(sbh_sch_sf_clean)
# nb2INLA("map.adj", nb)
# g <- inla.read.graph(filename = "map.adj")
# sbh_sch_sf_clean$re_u <- 1:nrow(sbh_sch_sf_clean)
# sbh_sch_sf_clean <- sbh_sch_sf_clean %>%
#   mutate(
#     pop_s = population / 10000,   # per 10000 people
#     area_s = as.numeric(area) / 10000000  # e.g., 10 km^2 instead of m^2
#   )
# 
# formula <- Y ~ pop_s + area_s  + f(re_u, model = "bym2", graph = g)
# # formula <- Y ~ pop_s + hp_s + f(re_u, model = "bym2", graph = g)
# # formula <- Y ~ pop_s + f(re_u, model = "bym2", graph = g)
# # formula <- Y ~ hp_s + f(re_u, model = "bym2", graph = g)
# 
# res <- inla(formula, family = "poisson", data = sbh_sch_sf_clean, E=E,
#             control.predictor = list(compute = TRUE),
#             control.compute = list(return.marginals.predictor = TRUE),
#             verbose = TRUE)
# summary(res)
# res$summary.fixed
# 
# sbh_sch_sf_clean$RA <- res$summary.fitted.values[, "mean"]
# 
# m1 <- mapview(sbh_sch_sf_clean, zcol = "RA", col.region=pal, at=at)
# m1
# sbh_sch_sf_clean$exc <- sapply(res$marginals.fitted.values,
#                                FUN = function(marg){inla.pmarginal(q = 0.7, marginal = marg)})
# 
# at <- c(0,0.25,0.5,0.75,1)
# mapview(sbh_sch_sf_clean, zcol = "exc", col.region=pal, at=at, 
#         layer.name="Exceedance Probability RA < 0.8")
