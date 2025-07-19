#----------------------------------------------------------#
# GBIF data  -----
#----------------------------------------------------------#
data_w %>%
  count(year) %>%
  ggplot(aes(x = year, y = n)) +
  geom_col(fill = "grey30") +
  labs(title = "Sampling effort over time", x = "Year", y = "Number of records") +
  theme_bw()

effort_by_year <- data_w %>%
  count(year, name = "n_records")

data_w_effort <- data_w %>%
  left_join(effort_by_year, by = "year") %>%
  mutate(log_effort = log(n_records + 1))  # add 1 to avoid log(0)

table(data_w_effort$zone)

model_w_lat_effort <- lm(
  decimalLatitude ~ year + log_effort,
  data = data_w_effort
)
summary(model_w_lat_effort)

# Choose a uniform sample size (e.g., 10 per year)
min_records_per_year <- 10

data_w_rarefied <- data_w %>%
  group_by(year) %>%
  filter(n() >= min_records_per_year) %>%
  slice_sample(n = min_records_per_year) %>%
  ungroup()

table(data_w_rarefied$zone)

model_w_lat_rarefied <- lm(
  decimalLatitude ~ year*country,
  data = data_w_rarefied
)
summary(model_w_lat_rarefied)

ggplot(data_w_effort, aes(x = year, y = decimalLatitude)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", aes(color = "Original"), se = FALSE) +
  geom_smooth(data = data_w_rarefied, method = "lm", aes(color = "Rarefied"), se = FALSE) +
  labs(color = "Model", title = "Latitudinal trend in time: original vs rarefied") +
  theme_bw()

ggplot(data_w, aes(x = year, y = decimalLatitude)) +
  geom_point(alpha = 0.5, color = "darkgreen") +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(
    title = "Latitudinal shift of *Coprimorphus scrutator*",
    x = "Year",
    y = "Latitude (decimal degrees)"
  ) +
  theme_bw()

data_w_decade <- 
  data_w %>%
  dplyr::mutate(decade = floor(year / 10) * 10)

ggplot(data_w_decade, aes(x = decade, y = decimalLatitude)) +
  stat_summary(fun = mean, geom = "point", color = "blue", size = 3) +
  stat_summary(fun = mean, geom = "line", color = "blue") +
  labs(
    title = "Average Latitude of *Coprimorphus scrutator* by Decade",
    x = "Decade",
    y = "Mean Latitude"
  ) +
  theme_bw()


#--------------------------------------------------#
## Plot the european data  -----
#--------------------------------------------------#
library(rnaturalearth)
library(rnaturalearthdata)

# Prepare Europe map (country polygons)
europe <- ne_countries(continent = "Europe", scale = "medium", returnclass = "sf")

# Convert to sf points
orig_points <- data_w %>%
  filter(!is.na(decimalLongitude), !is.na(decimalLatitude)) %>%
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

rarefied_points <- data_w_rarefied %>%
  filter(!is.na(decimalLongitude), !is.na(decimalLatitude)) %>%
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

# Plot
ggplot(data = europe) +
  geom_sf(fill = "white", color = "gray40") +
  geom_sf(data = orig_points, aes(color = factor(decade)), alpha = 0.4, size = 1, shape = 16) +
  #geom_sf(data = rarefied_points, aes(color = factor(decade)), alpha = 0.8, size = 1.5, shape = 17) +
  coord_sf(xlim = c(-11, 40), ylim = c(35, 72), expand = FALSE) +
  scale_color_viridis_d(name = "Decade", option = "C") +
  labs(
    title = "GBIF occurrences in Europe",
    subtitle = "Original (dots) and rarefied (triangles) colored by decade",
    caption = "Data from GBIF via rgbif"
  ) +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "aliceblue"),
    panel.grid.major = element_line(color = "gray90"),
    legend.position = "right"
  )

#----------------------------------------------------------#
# Czech data  -----
#----------------------------------------------------------#
model_c_lat <-
  lm(
    data = data,
    DATUM_OD ~ Y
  )
summary(model_c_lat)

data %>%
  count(year) %>%
  ggplot(aes(x = year, y = n)) +
  geom_col(fill = "grey30") +
  labs(title = "Sampling effort over time", x = "Year", y = "Number of records") +
  theme_bw()

