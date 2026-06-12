
controlbar <-
  dashboardControlbar(
    width = "350px",
    skin = "light",
    pinned = FALSE,
    collapsed = TRUE,
    overlay = F,
    controlbarMenu(
      id = "controlbar_menu",
      type = "tabs",
      # Filtros generales
      controlbarItem(
        title = "Filtros",
        icon  = icon("filter"),
        tags$div(
          style = "padding: 10px;",
          tags$h6(
            tags$i(class = "fas fa-calendar-alt me-1"),
            "Período de análisis",
            style = "font-weight:600; margin-bottom:8px;"
          ),
          airDatepickerInput(
            inputId = "fecha_rango",
            range = T,
            multiple = T,
            value = c(min(sales_data$ORDERDATE) - 1, max(sales_data$ORDERDATE) + 1),
            minDate = min(sales_data$ORDERDATE) - 1,
            maxDate = max(sales_data$ORDERDATE) + 1,
            inline = T
          ),
          tags$hr(),
          tags$h6(
            tags$i(class = "fas fa-boxes me-1"),
            "Línea de producto",
            style = "font-weight:600; margin-bottom:8px;"
          ),
          uiOutput("ui_productline"),
          tags$hr(),
          tags$h6(
            tags$i(class = "fas fa-globe-americas me-1"),
            "País",
            style = "font-weight:600; margin-bottom:8px;"
          ),
          uiOutput("ui_country"),
          tags$hr(),
          tags$h6(
            tags$i(class = "fas fa-city me-1"),
            "Estado del pedido",
            style = "font-weight:600; margin-bottom:8px;"
          ),
          uiOutput("ui_status"),
          tags$hr(),
          actionButton(
            "btn_reset",
            "Restablecer filtros",
            icon  = icon("rotate-left"),
            class = "btn-outline-secondary btn-sm w-100"
          )
        )
      ),
      # Filtros del forecast
      controlbarItem(
        title = "Forecast",
        icon  = icon("chart-line"),
        tags$div(
          style = "padding: 10px;",
          tags$h6(
            tags$i(class = "fas fa-forward me-1"),
            "Horizonte de pronóstico",
            style = "font-weight:600; margin-bottom:8px;"
          ),
          sliderInput(
            inputId = "forecast_periods",
            label = "Meses a proyectar",
            min = 3,
            max = 36,
            value = 12,
            step = 3,
            ticks = TRUE
          )
        )
      )
    )
  )