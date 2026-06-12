


sales_data <- read.csv("https://raw.githubusercontent.com/Jorge-hercas/r-para-inteligencia-de-negocios/main/CSV/ventas_proyecto_final.csv")
catalogo_direcciones <- read.csv("data/direcciones.csv")

sales_data <- 
  sales_data |> 
  left_join(
    catalogo_direcciones,
    by = c("ADDRESSLINE1" = "ADDRESSLINE1")
  ) |> 
  mutate(
    ORDERDATE = as_date(
      ORDERDATE, format = "%m/%d/%Y %H:%M"
    )
  )

# Usuarios autorizados
usuarios <- tibble::tibble(
  user = c("admin", "analista"),
  password = c("ejemplo_123", "datos_456"),
  name = c("admin", "ventas"),
  posicion = c("Gerente", "Vendedor")
)