
body <- dashboardBody(
  useShinyjs(),
  tabItems(
    # General
    tabItem(
      tabName = "general",
      fluidRow(
        bs4ValueBoxOutput("vbox_ventas",   width = 3),
        bs4ValueBoxOutput("vbox_ordenes",  width = 3),
        bs4ValueBoxOutput("vbox_clientes", width = 3),
        bs4ValueBoxOutput("vbox_ticket",   width = 3)
      ),
      fluidRow(
        column(
          width = 12,
          style = "display:flex; justify-content:center; padding-bottom:4px;",
          radioButtons(
            inputId = "granularidad_mapa",
            label = NULL,
            choices = c("País" = "pais", "Ciudad" = "ciudad"),
            selected = "pais",
            inline = TRUE
          )
        )
      ),
      fluidRow(
        bs4Card(
          title = "Ventas mensuales",
          width = 12,
          maximizable = TRUE,
          status = "navy",
          echarts4rOutput("chart_monthly", height = "380px")
        )
      ),
      fluidRow(
        bs4Card(
          title = "Concentración geográfica de ventas",
          width = 8,
          maximizable = TRUE,
          status = "navy",
          mapboxerOutput("mapa_ventas", height = "380px")
        ),
        bs4Card(
          title = "Distribución por estatus",
          width = 4,
          maximizable = TRUE,
          status = "navy",
          echarts4rOutput("chart_status", height = "380px")
        )
      ),
      fluidRow(
        bs4Card(
          title = "Ventas por línea de producto",
          width = 6,
          maximizable = TRUE,
          status = "navy",
          reactableOutput("tbl_productline")
        ),
        bs4Card(
          title = "Ventas por cliente (Top)",
          width = 6,
          maximizable = TRUE,
          status = "navy",
          reactableOutput("tbl_customer")
        )
      ),
      fluidRow(
        bs4Card(
          title = "Ventas por región",
          width = 12,
          maximizable = TRUE,
          status = "navy",
          echarts4rOutput("chart_region", height = "320px")
        )
      )
    ),
    # Forecast
    tabItem(
      tabName = "forecast",
      fluidRow(
        column(
          width = 12,
          style = "display:flex; justify-content:flex-end; padding-bottom:8px;",
          downloadButton(
            outputId = "download_forecast",
            label = "Descargar forecast",
            class = "btn btn-primary btn-sm"
          )
        )
      ),
      fluidRow(
        bs4Card(
          title = "Pronóstico de ventas (Prophet)",
          width = 12,
          maximizable = TRUE,
          status = "navy",
          echarts4rOutput("chart_forecast", height = "420px")
        )
      ),
      fluidRow(
        bs4Card(
          title = "Tendencia",
          width = 4,
          maximizable = TRUE,
          status = "navy",
          echarts4rOutput("chart_trend", height = "280px")
        ),
        bs4Card(
          title = "Descomposición",
          width = 8,
          maximizable = TRUE,
          status = "navy",
          reactableOutput("chart_descomp", height = "280px")
        )
      )
    )
  )
)