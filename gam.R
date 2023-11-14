library(mgcv) # GAM
library(sf) # geographic data
library(tidyr) # expand_grid
library(ggthemes) # for map theme
library(cowplot) # plot_grid
library(dplyr) # group_by
library(scales) # percent
library(ggplot2) # plot
library(nngeo) # removing the Brussels hole

#
# GAM
#

build_gam <- function(df, response_variable) {
  gam_formula <- as.formula(paste0(response_variable,
                                   " ~ s(long) + s(lat) + s(long, lat)"))
  
  fit <- gam(
    gam_formula,
    data = df,
    family = binomial,
    method = "REML"
  )
  
  return(fit)
}

# Shapefiles

# == Flanders ==
flanders <- st_read(dsn = "geo/regions/régions_08.shp")
brussels <- flanders[flanders$Nom == "Bruxelles-Capitale",]
flanders <-
  flanders[flanders$Nom == "Vlaams Gewest", ] # Wallonia bye
flanders <- st_transform(flanders, 4326) # convert to GPS coordinates system
brussels <- st_transform(brussels, 4326)

# == Dialect area borders ==
dialects <- st_read("geo/dialects/POLYGON.shp")
dialects <- st_transform(dialects, 4326)
# Intersection for nicer map
dialects <- st_intersection(dialects, flanders)

# == Municipalities ==
gemeenten <- st_read("geo/gemeenten/Refgem.shp")
gemeenten <- st_transform(gemeenten, 4326)
# Remove superfluous data
keep_columns <- c("NAAM", "geometry")
gemeenten <- subset(gemeenten, select = keep_columns)
# Prepare Brussels for a merger
# (lol, this is not a political statement I promise)
brussels$NAAM <- c("Brussel")
brussels <- subset(brussels, select = keep_columns)
# Merger! Don't tell the Flemish nationalists
gemeenten <- rbind(gemeenten, brussels)

# == Provincies ==
provinces <- st_read("geo/gemeenten/Refprv.shp")
provinces <- st_transform(provinces, 4326)
plot(provinces$geometry)
provinces <- subset(provinces, select = keep_columns)

brabant <- provinces[provinces$NAAM == "Vlaams Brabant",]
plot(brabant$geometry)
antwerpen <- provinces[provinces$NAAM == "Antwerpen",]
plot(antwerpen$geometry)
groot_brabant <- st_union(brabant, antwerpen)
plot(groot_brabant$geometry)
groot_brabant <- st_remove_holes(groot_brabant)
groot_brabant <- subset(groot_brabant,
                        select = keep_columns)
groot_brabant$NAAM <- c("Brabant")

provinces <- rbind(provinces, groot_brabant)
provinces <- provinces[!(provinces$NAAM %in% c("Vlaams Brabant", "Antwerpen")),]

# Predictions
# We generate data points to make the tile plot
# The more points, the higher resolution the plot will be

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

predict_coords <- function(df, fit) {
  long_from <- min(df$long) - 0.04
  long_to <- max(df$long) + 0.1
  
  lat_from <- min(df$lat)
  lat_to <- max(df$lat) + 0.05
  
  df_pred <- expand_grid(
    long = seq(
      from = long_from,
      to = long_to,
      length.out = 100
    ),
    lat = seq(
      from = lat_from,
      to = lat_to,
      length.out = 100
    )
  )
  
  # Turn into dataframe
  df_pred <- predict(fit, newdata = df_pred,
                     se.fit = TRUE) %>%
    as_tibble() %>%
    cbind(df_pred)
  
  df_pred$prob <- logit2prob(df_pred$fit)
  
  return(df_pred)
}

plot_map <- function(df_pred, gemeenten, phenomenon) {
  title <- paste0("Kans op '", phenomenon, "'")
  
  # The mighty plot!
  # Do not be surprised if it takes around a minute to generate this plot
  # Tiling the 10.000 predictions is a tough job!
  # color=alpha(gemeenten$value, 0.4),
  ggplot() +
    theme_map() +
    # theme(legend.position = "none") +
    geom_sf(
      data = gemeenten$geometry,
      aes(fill = gemeenten$prob,
          color = gemeenten$prob > 0.5),
    ) +
    scale_fill_distiller(palette = "YlGnBu",
                         labels = scales::label_percent()) +
    scale_color_manual(values = c("white", "#323232"),
                       guide = "none") +
    geom_sf(
      data = provinces$geometry,
      color = "#c16465",
      linewidth = 0.5,
      fill = "transparent"
    ) +
    geom_sf_text(
      data = gemeenten$geometry,
      check_overlap=T,
      size=2,
      aes(label = gemeenten$zip,
          color = gemeenten$prob > 0.5)) +
    # geom_contour(data = df_pred,
    #              aes(x = long, y = lat, z = prob),
    #              colour = "white") +
    coord_sf(default_crs = sf::st_crs(4326)) +
    guides(fill=guide_legend(title=title)) +
    theme(legend.position = "bottom")
}

nearest_point <- function(df_pred, lat, long, column) {
  # We supply the lat long coordinates to define ap point
  point <- st_point(c(long, lat)) %>% st_sfc(crs = 4326)
  
  point_index <- st_nearest_feature(point, df_pred, longlat=TRUE)
  
  #return(point_index)
  return(df_pred[point_index,][[column]])
}

df_to_plot <- function(df, response_variable, phenomenon, too.far=NA) {
  df_pred <- df %>%
    build_gam(response_variable) %>%
    predict_coords(df, .)
  
  coords <- df_pred %>% st_as_sf(coords = c("long", "lat"), crs=4326)
  
  # Copy gemeenten
  gemeenten_ <- gemeenten
  
  # For each town, we want to compute the closest point
  # From that point, we can get the prediction for that town
  # And like that, we can make a nice map :)
  
  gemeenten_$prob <- apply(gemeenten_, 1, function(row) {
    nearest_point(coords, row$lat, row$long, "prob")
  })
  
  if (!is.na(too.far)) {
    too_far <-
      exclude.too.far(df_pred$long, df_pred$lat, df$long, df$lat, too.far)
  } else {
    too_far <- rep(FALSE, df_pred$long %>% length)
  }
  
  # Remove "too far" data points
  #df_pred$too_far <- too_far
  #df_pred <- df_pred[!df_pred$too_far,]
  
  plot_map(df_pred, gemeenten_, phenomenon)
}
