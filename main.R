
library(XML)
library(jsonlite)
library(openxlsx)
library(httr)
library(XML)
library(ggplot2)
library(stringr)
library(tidyr)
library(sf)
library(dplyr)
library(reshape2)

xls <- read.xlsx("water_temp_from_IMGW-PIB.xlsx", sheet = 1)
xls$temperature_C <- as.numeric(xls$temperature_C)
xls$lat <- NA
xls$lon <- NA

base_url <- "https://nominatim.openstreetmap.org/search?q="

for (i in 1:nrow(xls)) { # nolint
  query <- paste0(xls[i, "station"], " ", xls[i, "region"])
  url <- paste0(base_url, URLencode(query), "&format=xml")
  response <- GET(url)
  xml_content <- content(response, as = "text", encoding = "UTF-8")
  parsed_xml <- xmlParse(xml_content)
  xml_list <- xmlToList(parsed_xml)
  if (!"place" %in% names(xml_list)){
    next
  }
  xls$lon[i] <- as.numeric(xml_list$place["lon"])
  xls$lat[i] <- as.numeric(xml_list$place["lat"])
}

base_url <- "https://nominatim.openstreetmap.org/search?q="
query <- paste0("Poland")
url <- paste0(base_url, URLencode(query), "&format=xml", "&polygon_text=1")
response <- GET(url)
xml_content <- content(response, as = "text", encoding = "UTF-8")


geotext <- xpathSApply(xmlParse(xml_content), "//place/@geotext")
coords <- str_replace_all(geotext, "POLYGON\\(\\(", "")
coords <- str_replace_all(coords, "\\)\\)", "")
coords <- strsplit(coords, ",")[[1]]
data <- tibble(coords)

data <- data %>%
  separate(
    coords,
    into = c("lat", "lon"),
    sep = " ",
    convert = TRUE
  )

poland_map <- map_data("world") %>%
  filter(region == "Poland")

p1 <- ggplot() +
  # contours from nominatim (q=Poland) gave Polish border with Border Waters
  # geom_polygon(data = data, aes(x = lat, y = lon),
  #              fill = "white", color = "black", linetype = "dotted") +
  geom_polygon(data = poland_map,
               aes(x = long, y = lat, group = group),
               fill = "white", color = "black") +
  geom_point(data = xls, aes(x = lon, y = lat, color = temperature_C),
             size = 4) +
  scale_color_viridis_c(name = "Temperature (°C)", na.value = "gray") +
  theme_minimal() +
  coord_sf(crs = st_crs(4326)) +
  ggtitle("Lake Temperatures in Poland (source: IMGW-PIB)") +
  theme(panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggsave("lake_temperatures_poland.png", plot = p1, width = 10, height = 8)
print("Saved plot no. 1")

data_wide <- data.frame(xls)
data_long <- melt(data_wide, id.vars = "temperature_C",
                  measure.vars = c("lat", "lon"),
                  variable.name = "Coordinate",
                  value.name = "Value")

# remove temperature outlier
data_long <- data_long %>%
  filter(
    data_long$temperature_C >= quantile(data_long$temperature_C, 0.05, na.rm = TRUE) &
    data_long$temperature_C <= quantile(data_long$temperature_C, 0.95, na.rm = TRUE)
  )
regression_labels <- data_long %>%
  group_by(Coordinate) %>%
  summarize(
    formula = {
      fit <- lm(temperature_C ~ Value, data = cur_data())
      intercept <- coef(fit)[1]
      slope <- coef(fit)[2]
      r_squared <- summary(fit)$r.squared
      paste0("y = ", round(slope, 2), "x + ", round(intercept, 2),
             "\nR² = ", round(r_squared, 3))
    },
    x_pos = mean(range(Value, na.rm = TRUE)),
    y_pos = max(temperature_C, na.rm = TRUE) + 0.5
  )

p2 <- ggplot(data_long, aes(x = Value, y = temperature_C)) +
  geom_point(alpha = 0.6) +
  facet_wrap(
    ~ Coordinate, scales = "free_x", nrow = 1,
    labeller = labeller(Coordinate = c(lat = "Latitude (°)", lon = "Longitude (°)"))
  ) +
  geom_smooth(method = "lm", fill = "pink", se = TRUE, formula = y ~ x) +
  geom_text(
    data = regression_labels,
    aes(x = x_pos, y = y_pos, label = formula),
    inherit.aes = FALSE,
    hjust = 0.1,
    color = "black"
  ) +
  labs(
    title = "Temperature vs Latitude and Longitude",
    x = "",
    y = "Temperature (°C)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.spacing = unit(2, "lines"),
    panel.grid.minor = element_blank()
  )

ggsave("multi_faceted_plot.png", plot = p2, width = 8, height = 6)
p2
print("Saved plot no. 2")
