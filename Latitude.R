#----------------------------------------------------------#
# GBIF data  -----
#----------------------------------------------------------#

model_w_lat <-
  lm(
  data = data_world[["data"]],
  year ~ decimalLatitude
  )
summary(model_w_lat)

data_world[["data"]] %>%
  count(year) %>%
  ggplot(aes(x = year, y = n)) +
  geom_col(fill = "grey30") +
  labs(title = "Sampling effort over time", x = "Year", y = "Number of records") +
  theme_bw()

ggplot(data_world[["data"]], aes(x = decimalLatitude, y = year)) +
  geom_point(alpha = 0.5, color = "darkgreen") +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(
    title = "Latitudinal shift of *Coprimorphus scrutator*",
    x = "Year",
    y = "Latitude (decimal degrees)"
  ) +
  theme_bw()

data_world_decade <- 
  data_world[["data"]] %>%
  dplyr::mutate(decade = floor(year / 10) * 10)

ggplot(data_world_decade, aes(x = decade, y = decimalLatitude)) +
  stat_summary(fun = mean, geom = "point", color = "blue", size = 3) +
  stat_summary(fun = mean, geom = "line", color = "blue") +
  labs(
    title = "Average Latitude of *Coprimorphus scrutator* by Decade",
    x = "Decade",
    y = "Mean Latitude"
  ) +
  theme_bw()

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

