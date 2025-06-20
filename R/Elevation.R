library(terra)

# Get raster for the bounding box of your data
bbox <- sf::st_bbox(data)
bbox_sf <- st_as_sfc(bbox) |> st_as_sf()

# Optional: buffer to avoid edge cutoffs
bbox_sf <- st_buffer(bbox_sf, dist = 0.01)

# Download raster
dem <- get_elev_raster(locations = bbox_sf, z = 10, src = "aws")

# Extract elevations to your points
data$elevation <- terra::extract(terra::rast(dem), terra::vect(data))[, 2]