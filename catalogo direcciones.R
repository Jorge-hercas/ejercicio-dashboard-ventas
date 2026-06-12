
library(tidyr)
library(dplyr)
library(ggmap)

register_google("AIzaSyC-L1JqEFQyWuwMiflGyA-8HRMi8K31noM")

sales_data <- read.csv("https://raw.githubusercontent.com/Jorge-hercas/r-para-inteligencia-de-negocios/main/CSV/ventas_proyecto_final.csv")

# Catálogo de direcciones únicas
catalogo_direcciones <- 
  sales_data |> 
  select(ADDRESSLINE1, CITY, COUNTRY) |> 
  mutate(
    Direccion_str = paste(ADDRESSLINE1, CITY, COUNTRY, sep = ", ")
  ) |> 
  distinct()

# Obtención de latitudes/longitudes
catalogo_direcciones <-
  catalogo_direcciones |> 
  mutate_geocode(
    Direccion_str
  ) |> 
  rename(
    address_lat = lat,
    address_lng = lon
  ) |> 
  left_join(
    catalogo_direcciones |> 
      select(COUNTRY) |> 
      distinct() |> 
      mutate_geocode(
        COUNTRY
      ) |> 
      rename(
        country_lat = lat,
        country_lng = lon
      ),
    by = c("COUNTRY" = "COUNTRY")
  )

# Guardado de catálogo en un objeto
write.csv(catalogo_direcciones, file = "data/direcciones.csv")