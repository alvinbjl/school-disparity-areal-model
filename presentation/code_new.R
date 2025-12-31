#  library -----------------------------------------------------------------
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

#  Data: Primary & secondary Schools, Population, sf ----------
#  Fix school on water village (slightly out of bound)
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

# Join population
mkm_pop <- bruneimap::census2021 %>% 
  group_by(mukim) %>% 
  summarise(population = sum(population, na.rm = TRUE))

# Join youth population
pop_youth <- read_csv("source/census_2021_age.csv")
pop_youth <- 
  pop_youth |>  
  mutate(population_youth = `Male 00-14` +
                            `Female 00-14` +
                            `Male 15-24` +
                            `Female 15-24`) |> 
  select(mukim, population_youth)

# Join sf
brn_mkm_sch_sf <- left_join(mkm_sf, brn_mkm_sch_df, by="mukim") 
brn_mkm_sch_sf <- left_join(brn_mkm_sch_sf, mkm_pop, by="mukim")
brn_mkm_sch_sf <- left_join(brn_mkm_sch_sf, pop_youth, by="mukim")

#  EDA1: school count ----------------------------------------------------
pal <- colorRampPalette(brewer.pal(9, "YlOrRd"))
mv1 <- mapview(brn_mkm_sch_sf , zcol = "schools", col.region=pal, layer.name="schools")
mv1

#ggplot
label_sf <- brn_mkm_sch_sf |> 
  arrange(desc(schools)) |> 
  slice_head(n = 5) |> 
  mutate(label = paste0(mukim, "\n", schools))
ggplot() +
  geom_sf(data = brn_mkm_sch_sf, aes(fill = schools)) +
  geom_sf(data = mkm_sf, color="grey", alpha=0, linewidth=0.5) +
  geom_sf(data = dis_sf, color="white", alpha=0, linewidth=0.8) +
  ggrepel::geom_label_repel(
    data = label_sf,
    aes(label = label, geometry = geometry),
    stat = "sf_coordinates",
    inherit.aes = FALSE,
    box.padding = 1,
    size = 6,
    alpha = 0.7,
    force=5,
    max.overlaps = Inf
  ) +
  scale_fill_viridis_b(
    option = "mako",
    direction = 1,
    name = "Schools Count",
    na.value = NA,
    breaks = c(0,2,4,6,8)    # Number of bins
  ) +
  labs(x = NULL, y = NULL) +
  theme_void()
  # theme(legend.position = "top")


  #  Model covariate: house price --------------------------
hp <- read_csv("source/house_price_phi.csv")

# replace NA schools <= 0
brn_mkm_sch_sf$schools[is.na(brn_mkm_sch_sf$schools)] <- 0

# join
brn_mkm_sch_sf <- left_join(brn_mkm_sch_sf, hp, by = "mukim")
brn_mkm_sch_sf <- brn_mkm_sch_sf %>% 
  mutate(hp = hp_phi) %>% 
  select(-X, -Y, -perimeter, -area, -hp_phi, -id)

# Fix missing hp using INLA (gaussian)
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



# SIR (bad not smooth) ----------------------------------------------------
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



#  Fit model y ~ pop_youth + area + hp (socioeconomic) + u_i +v_i -----------------------------------------------------------------------
# Concern: Is school count too little. If poisson, ok.
nb <- poly2nb(brn_mkm_sch_sf)
nb2INLA("map.adj", nb)
g <- inla.read.graph(filename = "map.adj")
brn_mkm_sch_sf$re_u <- 1:nrow(brn_mkm_sch_sf)
brn_mkm_sch_sf <- brn_mkm_sch_sf %>%
  mutate(area_s = as.numeric(area) / 10000000, # e.g., 10 km^2 instead of m^2
    pop_youth_s = population_youth / 1000   # per 1000 people
    #hp_s = hp / 1000000              # per million $
  )

formula <- Y ~ pop_youth_s + area_s + hp + f(re_u, model = "bym2", graph = g)

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
  geom_sf(data = mkm_sf, color="grey", alpha=0, linewidth=0.5) +
  geom_sf(data = dis_sf, color="white", alpha=0, linewidth=0.8) +
  ggrepel::geom_label_repel(
    data = label_sf,
    aes(label = label, geometry = geometry),
    stat = "sf_coordinates",
    inherit.aes = FALSE,
    box.padding = 1,
    size = 6,
    alpha = 0.7,
    force=5,
    max.overlaps = Inf
  ) +
  scale_fill_viridis_b(
    option = "mako",
    direction = 1,
    name = "RA",
    na.value = NA,
    breaks = c(0,1,2,3)    # Number of bins
  ) +
  labs(x = NULL, y = NULL) +
  theme_void()



#  Exceedance Prob. -----------------------------------------------------------------------
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
               layer.name="Non-Exceedance Probability RA_lt_0.7") + 
  mapview(sch_sf, cex=4)
mv3

label_sf <- brn_mkm_sch_sf |> 
  arrange(desc(exc)) |> 
  slice_head(n = 5) |> 
  mutate(label = paste0(mukim, "\n", round(exc,2)))
ggplot() +
  geom_sf(data = brn_mkm_sch_sf, aes(fill = exc)) +
  geom_sf(data = mkm_sf, color="grey", alpha=0, linewidth=0.7) +
  geom_sf(data = dis_sf, color="white", alpha=0, linewidth=0.8) +
  # geom_sf(data = sch_sf) +
  ggrepel::geom_label_repel(
    data = label_sf,
    aes(label = label, geometry = geometry),
    stat = "sf_coordinates",
    inherit.aes = FALSE,
    box.padding = 1,
    size = 6,
    alpha = 0.7,
    force=5,
    max.overlaps = Inf
  ) +
  scale_fill_viridis_b(
    option = "mako",
    direction = 1,
    name = "P(RA) < 0.7",
    na.value = NA,
    breaks = c(0,0.25,0.5,0.75)    # Number of bins
  ) +
  labs(x = NULL, y = NULL) +
  theme_void()
  # theme(legend.position = "top")

brn_mkm_sch_sf$exc2 <- sapply(res$marginals.fitted.values,
                              FUN = function(marg){1 - inla.pmarginal(q = 1, marginal = marg)})

at <- c(0,0.25,0.5,0.75,1)
mapview(brn_mkm_sch_sf, zcol = "exc2", col.region=pal, at=at, 
        layer.name="Exceedance Probability RA > 1.2")



#  residual ---------------------------------------------------------------
fitted_counts <- brn_mkm_sch_sf$E * brn_mkm_sch_sf$RA
brn_mkm_sch_sf$residuals_pearson <- (brn_mkm_sch_sf$Y - fitted_counts) / sqrt(fitted_counts)
# 3.2 Convert to spatial weights list
lw <- nb2listw(nb, style = "W")
# 3.3 Run Moran’s I test on residuals
moran.test(brn_mkm_sch_sf$residuals_pearson, lw)
my_palette <- colorRampPalette(brewer.pal(11, "RdBu"))(100)
mapview(brn_mkm_sch_sf, zcol="residuals_pearson", col.regions = my_palette, at = seq(-1, 3, length.out = 101))
mv4 <- ggplot() +
  geom_sf(data = brn_mkm_sch_sf, aes(fill = residuals_pearson)) +
  geom_sf(data = mkm_sf, color="white", alpha=0, linewidth=0.7) +
  scale_fill_viridis_b(
    option = "mako",
    direction = 1,
    name = "Residual",
    na.value = NA,
    n.breaks = 6    # Number of bins
  ) +
  labs(x = NULL, y = NULL) +
  theme_minimal()
mv4

save(mv1, mv2, mv3, mv4, file = "presentation_maps.RData")
