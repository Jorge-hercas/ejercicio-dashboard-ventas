function(input, output, session) {
  
  autenticacion <- secure_server(
    check_credentials = check_credentials(usuarios)
  )
  
  
  output$user <- renderUser({
    dashboardUser(
      name = autenticacion$name,
      image = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcQIf4R5qPKHPNMyAqV-FjS_OTBB8pfUV29Phg&s",
      title = autenticacion$posicion
    )
  })
  
  # Reactivo que devuelve TRUE cuando el modo oscuro está activo
  es_dark <- reactive({ isTRUE(input$dark_mode) })
  
  # Tema echarts4r según modo
  tema_e <- reactive({ if (es_dark()) "dark" else "infographic" })
  
  # Paleta de texto para reactable
  color_texto <- reactive({ if (es_dark()) "#e0e0e0" else "#111111" })
  color_header <- reactive({ if (es_dark()) "#1a2a4a"  else "#081e3d" })
  color_bg <- reactive({ if (es_dark()) "#1e1e2e"  else "transparent" })
  color_bg_strip <- reactive({ if (es_dark()) "#16213e"  else "#f9f9f9" })
  color_borde <- reactive({ if (es_dark()) "#2e3a5a"  else "#eeeeee" })
  
  # Helper: devuelve reactableTheme adaptado al modo
  tema_reactable <- reactive({
    reactableTheme(
      backgroundColor = color_bg(),
      borderColor = color_borde(),
      stripedColor = color_bg_strip(),
      color = color_texto(),
      searchInputStyle = list(background = color_bg()),
      pageButtonStyle = list(color = color_texto())
    )
  })
  
  # Helper: opciones de tooltip echarts con fondo adaptado
  tooltip_dark_bg <- reactive({
    if (es_dark())
      list(backgroundColor = "#1e1e2e", borderColor = "#3a4a6a",
           textStyle = list(color = "#e0e0e0"))
    else
      list(backgroundColor = "#fff")
  })
  
  output$ui_productline <- renderUI({
    vals <- sort(unique(sales_data$PRODUCTLINE))
    pickerInput("productline", choices = vals, selected = vals,
                options = opts, multiple = TRUE)
  })
  
  output$ui_country <- renderUI({
    vals <- sort(unique(sales_data$COUNTRY.x))
    pickerInput("country", choices = vals, selected = vals,
                options = opts, multiple = TRUE)
  })
  
  output$ui_status <- renderUI({
    vals <- sort(unique(sales_data$STATUS))
    pickerInput("status_filter", choices = vals, selected = vals,
                options = opts, multiple = TRUE)
  })
  
  # Reset de filtros
  observeEvent(input$btn_reset, {
    updatePickerInput(session, "productline", selected = unique(sales_data$PRODUCTLINE))
    updatePickerInput(session, "country", selected = unique(sales_data$COUNTRY.x))
    updatePickerInput(session, "status_filter", selected = unique(sales_data$STATUS))
  })
  
  datos_filtrados <- reactive({
    req(input$fecha_rango)
    
    df <- sales_data |>
      filter(
        ORDERDATE >= input$fecha_rango[1],
        ORDERDATE <= input$fecha_rango[2]
      )
    
    if (!is.null(input$productline))
      df <- df |> filter(PRODUCTLINE %in% input$productline)
    
    if (!is.null(input$country) )
      df <- df |> filter(COUNTRY.x %in% input$country)
    
    if (!is.null(input$status_filter))
      df <- df |> filter(STATUS %in% input$status_filter)
    
    df
  })
  
  output$vbox_ventas <- renderbs4ValueBox({
    total <- sum(datos_filtrados()$SALES, na.rm = TRUE)
    bs4ValueBox(
      value = scales::dollar(total, big.mark = ",", prefix = "$"),
      subtitle = "Ventas totales",
      icon = icon("dollar-sign"),
      color = "navy",
      width = 12
    )
  })
  
  output$vbox_ordenes <- renderbs4ValueBox({
    n <- n_distinct(datos_filtrados()$ORDERNUMBER)
    bs4ValueBox(
      value = scales::comma(n),
      subtitle = "Órdenes",
      icon = icon("receipt"),
      color = "info",
      width = 12
    )
  })
  
  output$vbox_clientes <- renderbs4ValueBox({
    n <- n_distinct(datos_filtrados()$CUSTOMERNAME)
    bs4ValueBox(
      value = scales::comma(n),
      subtitle = "Clientes únicos",
      icon = icon("users"),
      color = "success",
      width = 12
    )
  })
  
  output$vbox_ticket <- renderbs4ValueBox({
    total  <- sum(datos_filtrados()$SALES, na.rm = TRUE)
    orders <- n_distinct(datos_filtrados()$ORDERNUMBER)
    ticket <- if (orders > 0) total / orders else 0
    bs4ValueBox(
      value = scales::dollar(ticket, big.mark = ",", prefix = "$"),
      subtitle = "Ticket promedio",
      icon = icon("tag"),
      color = "warning",
      width = 12
    )
  })
  
  output$chart_monthly <- renderEcharts4r({
    paleta <- if (es_dark()) "Blues" else "YlGn"
    
    datos_filtrados() |>
      mutate(ORDERDATE_round = floor_date(ORDERDATE, "months")) |>
      group_by(ORDERDATE_round) |>
      summarise(TOTAL_SALES = sum(SALES, na.rm = TRUE), .groups = "drop") |>
      mutate(
        color = scales::col_quantile(paleta, TOTAL_SALES)(TOTAL_SALES)
      ) |>
      e_charts(ORDERDATE_round, dispose = FALSE) |>
      e_bar(TOTAL_SALES, name = "Ventas") |>
      e_add("itemStyle", color) |>
      e_tooltip(
        trigger = "axis",
        backgroundColor = tooltip_dark_bg()$backgroundColor,
        borderColor = tooltip_dark_bg()$borderColor,
        textStyle = tooltip_dark_bg()$textStyle,
        formatter = htmlwidgets::JS("
          function(params) {
            var d = new Date(params[0].value[0]);
            var mes = d.toLocaleDateString('es-MX', 
            {year:'numeric', month:'short'});
            var val = '$' + params[0].value[1].toLocaleString('es-MX', 
            {maximumFractionDigits: 0});
            return mes + '<br/>' + params[0].marker + ' Ventas: <b>' 
            + val + '</b>';
          }
        ")
      ) |>
      e_x_axis(
        type = "time",
        axisLabel = list(color = if (es_dark()) "#c0c0c0" else NULL)
      ) |>
      e_y_axis(
        axisLabel = list(
          color = if (es_dark()) "#c0c0c0" else NULL,
          formatter = htmlwidgets::JS("function(v){ return '$' + (v/1000).toFixed(0) + 'k'; }")
        )
      ) |>
      e_theme(tema_e()) |>
      e_toolbox_feature("dataZoom") |>
      e_toolbox_feature("restore") |>
      e_grid(backgroundColor = "transparent")
  })
  
  output$chart_status <- renderEcharts4r({
    datos_filtrados() |>
      group_by(STATUS) |>
      summarise(total = sum(SALES, na.rm = TRUE), .groups = "drop") |>
      e_charts(STATUS, dispose = FALSE) |>
      e_pie(
        total,
        radius = c("50%", "70%"),
        name = "Ventas",
        label = list(
          color = if (es_dark()) "#e0e0e0" else NULL
        ),
        labelLine = list(
          lineStyle = list(color = if (es_dark()) "#888" else NULL)
        )
      ) |>
      e_tooltip(
        backgroundColor = tooltip_dark_bg()$backgroundColor,
        borderColor = tooltip_dark_bg()$borderColor,
        textStyle = tooltip_dark_bg()$textStyle,
        formatter = htmlwidgets::JS("
          function(params){
            return params.name + '<br/>' +
                   params.marker + ' $' +
                   params.value.toLocaleString('es-MX', 
                   {maximumFractionDigits:0}) +
                   ' (' + params.percent + '%)';
          }
        ")
      ) |>
      e_legend(
        bottom = 0,
        textStyle = list(color = if (es_dark()) "#e0e0e0" else "#000")
      ) |>
      e_theme(tema_e())
  })
  
  
  
  output$tbl_productline <- renderReactable({
    x <- datos_filtrados() |>
      group_by(PRODUCTLINE) |>
      summarise(total = sum(SALES, na.rm = TRUE), .groups = "drop") |>
      arrange(desc(total))
    
    # Paleta de barras: tonos cálidos en light, neones fríos en dark
    pal_barras <- if (es_dark())
      c("#1a3a5c","#1e5f8a","#2180b8","#3fa0d4","#6abde6","#9dd4f0","#c5e8fa")
    else
      RColorBrewer::brewer.pal(9, "Reds")
    
    x |> 
      reactable(
      theme  = tema_reactable(),
      defaultPageSize = 10,
      striped = TRUE,
      highlight = TRUE,
      defaultColDef = colDef(
        headerStyle = list(
          background = color_header(),
          color = "#ffffff",
          fontWeight = "600",
          fontSize = "12px"
        ),
        style = list(
          color = color_texto(),
          fontSize = "12px"
        )
      ),
      columns = list(
        PRODUCTLINE = colDef(
          name = "Categoría de producto",
          style = list(fontSize = "12px")
        ),
        total = colDef(
          name = "Ventas totales",
          align = "left",
          minWidth = 180,
          format = colFormat(digits = 0),
          style = list(fontSize = "12px"),
          cell = data_bars(
            data = x,
            fill_color = pal_barras,
            background = "transparent",
            bar_height = 7,
            number_fmt = scales::comma,
            text_position = "outside-end",
            round_edges = TRUE,
            text_color = color_texto()
          )
        )
      )
    )
  })
  
  output$tbl_customer <- renderReactable({
    x <- datos_filtrados() |>
      group_by(CUSTOMERNAME) |>
      summarise(total = sum(SALES, na.rm = TRUE), .groups = "drop") |>
      arrange(desc(total))
    
    pal_barras <- if (es_dark())
      c("#2d1b5c","#4a2d8a","#6a3fb8","#8a5cd4","#aa80e6","#c8a8f0","#e0ccfa")
    else
      RColorBrewer::brewer.pal(9, "Blues")
    
    x |> 
      reactable(
      theme = tema_reactable(),
      pagination = FALSE,
      height = 350,
      striped = TRUE,
      highlight = TRUE,
      defaultColDef = colDef(
        headerStyle = list(
          background = color_header(),
          color = "#ffffff",
          fontWeight = "600",
          fontSize = "12px"
        ),
        style = list(
          color = color_texto(),
          fontSize = "12px"
        )
      ),
      columns = list(
        CUSTOMERNAME = colDef(
          name = "Cliente",
          style = list(fontSize = "12px")
        ),
        total = colDef(
          name = "Ventas totales",
          align = "left",
          minWidth = 180,
          format = colFormat(digits = 0),
          style = list(fontSize = "12px"),
          cell = data_bars(
            data = x,
            fill_color = pal_barras,
            background = "transparent",
            bar_height = 7,
            number_fmt = scales::comma,
            text_position  = "outside-end",
            round_edges = TRUE,
            text_color = color_texto()
          )
        )
      )
    )
  })
  
  agregar_mapa <- function(df, granularidad) {
    group_cols <- if (granularidad == "ciudad")
      c("Direccion_str", "address_lng", "address_lat")
    else
      c("COUNTRY.y", "country_lng", "country_lat")
    
    df |>
      group_by(across(all_of(group_cols))) |>
      summarise(total = sum(SALES, na.rm = TRUE), .groups = "drop") |>
      mutate(radio = (total / sum(total)) * 100) |>
      setNames(c("Zone", "lng", "lat", "total", "radio")) |>
      filter(!is.na(lng), !is.na(lat))
  }
  
  output$mapa_ventas <- renderMapboxer({
    datos_iniciales <- agregar_mapa(sales_data, "pais")
    
    style <- ifelse(
      isTRUE(input$dark_mode),
      "mapbox://styles/mapbox/dark-v11",
      "mapbox://styles/jorgehdez1998/clhsaghny00ys01qn6f27870h"
    )
    
    datos_iniciales |>
      as_mapbox_source() |>
      mapboxer(
        center = c(0, 20),
        maxZoom = 7,
        zoom = 1.5,
        token = "xxxxxxxxxxxxxxxxxxxxxxxxxxx",
        style = style
      ) |>
      add_circle_layer(
        circle_color = "red",
        circle_radius = c("get", "radio"),
        circle_opacity = 0.7
      )
  })
  
  datos_mapa <- reactive({
    req(input$granularidad_mapa)
    agregar_mapa(datos_filtrados(), input$granularidad_mapa)
  })
  
  bounds_mapa <- reactive({
    df <- datos_mapa()
    req(nrow(df) > 0)
    c(
      min(df$lng, na.rm = TRUE),
      min(df$lat, na.rm = TRUE),
      max(df$lng, na.rm = TRUE),
      max(df$lat, na.rm = TRUE)
    )
  })
  
  observeEvent(datos_mapa(), {
    df <- datos_mapa()
    req(nrow(df) > 0)
    
    mapboxer_proxy("mapa_ventas") |>
      set_data(data = df) |>
      fit_bounds(bounds_mapa()) |>
      update_mapboxer()
  }, ignoreInit = TRUE)
  
  output$chart_region <- renderEcharts4r({
    group_var <- if (input$granularidad_mapa == "ciudad") "CITY.x" else "COUNTRY.x"
    
    datos_filtrados() |>
      group_by_at(group_var) |>
      summarise(total = sum(SALES, na.rm = TRUE), .groups = "drop") |>
      arrange(desc(total)) |>
      slice_head(n = 15) |>
      e_charts_(group_var, dispose = FALSE) |>
      e_bar(total, name = "Ventas") |>
      e_x_axis(
        axisLabel = list(
          color = if (es_dark()) "#c0c0c0" else NULL
        )
      ) |>
      e_tooltip(
        trigger = "axis",
        backgroundColor = tooltip_dark_bg()$backgroundColor,
        borderColor = tooltip_dark_bg()$borderColor,
        textStyle = tooltip_dark_bg()$textStyle,
        formatter = htmlwidgets::JS("
      function(params) {
        var p = params[0];
        var ciudad = p.axisValue || p.name;
        var valor = Array.isArray(p.value) ? p.value[1] : p.value;
        var val = '$' + Number(valor).toLocaleString('es-MX', {
          maximumFractionDigits: 0
        });
        return ciudad + '<br/>' +
               p.marker + ' Ventas: <b>' + val + '</b>';
      }
    ")
      ) |>
      e_theme(tema_e())
  })
  
 
  modelo_reactive <- reactive({
    
    sales_ts <- datos_filtrados() |>  
      mutate(ORDERDATE_round = floor_date(ORDERDATE, "month")) |> 
      group_by(ORDERDATE_round) |> 
      summarise(total = sum(SALES, na.rm = TRUE), .groups = "drop") |> 
      arrange(ORDERDATE_round)
    
    # columnas para Prophet
    prophet_data <- sales_ts |> 
      transmute(
        ds = as.Date(ORDERDATE_round),
        y = total
      )
    
    prophet_data
  })
  
  
  forecast_reactive <- reactive({
    
    # Modelo Prophet
    modelo_prophet <- prophet(
      modelo_reactive(),
      yearly.seasonality = TRUE,
      weekly.seasonality = FALSE,
      daily.seasonality = FALSE,
      seasonality.mode = "multiplicative",
      changepoint.prior.scale = 0.05,
      seasonality.prior.scale = 10
    )
    
    # Pronóstico a 12 meses
    future <- make_future_dataframe(
      modelo_prophet,
      periods = if (is.null(input$forecast_periods)) 12 
      else input$forecast_periods,
      freq = "month",
      include_history = TRUE
    )
    
    forecast_prophet <- predict(modelo_prophet, future)
    
    forecast_prophet
    
  })
  
  
  output$chart_forecast <- renderEcharts4r({
    
    historico <- modelo_reactive()
    forecast <- forecast_reactive()
    max_fecha <- max(historico$ds, na.rm = TRUE)
    
    plot_data <- forecast |> 
      select(ds, yhat, yhat_lower, yhat_upper) |> 
      left_join(historico, by = "ds") |> 
      mutate(
        `Point Forecast` = if_else(ds >= max_fecha, yhat, NA),
        `Lower 95%` = if_else(ds >= max_fecha, yhat_lower, NA),
        `Upper 95%` = if_else(ds >= max_fecha, yhat_upper, NA)
      )
    
    plot_data |> 
      e_charts(ds, dispose = FALSE) |> 
      e_line(
        y, name = "Ventas reales", symbol = "none") |> 
      e_line(`Point Forecast`, name = "Pronóstico", symbol = "none",
        lineStyle = list(width = 3)) |> 
      e_line(`Lower 95%`, name = "Límite inferior 95%",
        symbol = "none", lineStyle = list(type = "dashed")) |> 
      e_line(`Upper 95%`, name = "Límite superior 95%",
        symbol = "none", lineStyle = list(type = "dashed")) |> 
      e_tooltip(
        trigger = "axis",
        backgroundColor = tooltip_dark_bg()$backgroundColor,
        borderColor = tooltip_dark_bg()$borderColor,
        textStyle = tooltip_dark_bg()$textStyle,
        formatter = htmlwidgets::JS("
        function(params) {
          var d = new Date(params[0].value[0]);
          var mes = d.toLocaleDateString('es-MX', {
            year: 'numeric',
            month: 'short'
          });
          var html = mes + '<br/>';
          params.forEach(function(p) {
            var val = p.value[1];
            if (val !== null && val !== undefined && !isNaN(val)) {
              var val_fmt = '$' + Number(val).toLocaleString('es-MX', {
                maximumFractionDigits: 0
              });
              html += p.marker + ' ' + p.seriesName + ': <b>' 
              + val_fmt + '</b><br/>';
            } }); return html;}")
      ) |> 
      e_x_axis(
        type = "time",
        axisLabel = list(
          color = if (es_dark()) "#c0c0c0" else "#000000"
        )
      ) |> 
      e_y_axis(
        axisLabel = list(
          color = if (es_dark()) "#c0c0c0" else "#000000",
          formatter = htmlwidgets::JS("
          function(v){
            return '$' + (v / 1000).toFixed(0) + 'k';
          }"))) |> 
      e_legend(
        textStyle = list(color = if (es_dark()) "#c0c0c0" else "#000000")) |> 
      e_theme(tema_e()) |> 
      e_toolbox_feature("dataZoom") |> 
      e_toolbox_feature("restore") |> 
      e_grid(backgroundColor = "transparent")
    
  })
  
  
  output$chart_trend <- renderEcharts4r({
    
    forecast <- forecast_reactive()
    
    plot_data <- forecast |> 
      mutate(
        `Tendencia` = trend,
        `Límite inferior 95%` = if ("trend_lower" %in% names(forecast)) trend_lower else NA,
        `Límite superior 95%` = if ("trend_upper" %in% names(forecast)) trend_upper else NA
      ) |> 
      select(ds, `Tendencia`, `Límite inferior 95%`, `Límite superior 95%`)
    
    plot_data |> 
      e_charts(ds, dispose = FALSE) |> 
      e_line(`Tendencia`, name = "Tendencia", symbol = "none",
        lineStyle = list(width = 3)) |> 
      e_line(`Límite inferior 95%`, name = "Límite inferior 95%",
        symbol = "none", lineStyle = list(type = "dashed")) |> 
      e_line(`Límite superior 95%`, name = "Límite superior 95%",
        symbol = "none", lineStyle = list(type = "dashed")) |> 
      e_tooltip(
        trigger = "axis",
        backgroundColor = tooltip_dark_bg()$backgroundColor,
        borderColor = tooltip_dark_bg()$borderColor,
        textStyle = tooltip_dark_bg()$textStyle,
        formatter = htmlwidgets::JS("
        function(params) {
          var d = new Date(params[0].value[0]);
          var mes = d.toLocaleDateString('es-MX', {
            year: 'numeric',
            month: 'short'
          });
          var html = mes + '<br/>';
          params.forEach(function(p) {
            var val = p.value[1];
            if (val !== null && val !== undefined && !isNaN(val)) {
              var val_fmt = '$' + Number(val).toLocaleString('es-MX', {
                maximumFractionDigits: 0});
              html += p.marker + ' ' + p.seriesName + ': <b>' 
                + val_fmt + '</b><br/>';} });
          return html;}")) |> 
      e_x_axis(type = "time",
        axisLabel = list(
          color = if (es_dark()) "#c0c0c0" else "#000000"
        )) |> 
      e_y_axis(
        axisLabel = list(
          color = if (es_dark()) "#c0c0c0" else "#000000",
          formatter = htmlwidgets::JS("
          function(v){
            return '$' + (v / 1000).toFixed(0) + 'k';}"))) |> 
      e_legend(
        textStyle = list(
          color = if (es_dark()) "#c0c0c0" else "#000000")) |> 
      e_theme(tema_e()) |> 
      e_toolbox_feature("dataZoom") |> 
      e_toolbox_feature("restore") |> 
      e_grid(backgroundColor = "transparent")
    
  })
  
  
  tabla_reactive <- reactive({
    
    historico <- modelo_reactive()
    forecast <- forecast_reactive()
    max_fecha <- max(historico$ds, na.rm = TRUE)
    
    x <- forecast |> 
      left_join(historico, by = "ds") |> 
      mutate(
        tipo = if_else(ds <= max_fecha, "Histórico", "Pronóstico"),
        fecha = format(ds, "%Y-%m"),
        `Ventas reales` = y,
        `Pronóstico` = yhat,
        `Límite inferior 95%` = yhat_lower,
        `Límite superior 95%` = yhat_upper,
        `Tendencia` = trend,
        `Tendencia inferior` = if ("trend_lower" %in% names(forecast)) 
          trend_lower else NA,
        `Tendencia superior` = if ("trend_upper" %in% names(forecast)) 
          trend_upper else NA,
        `Estacionalidad anual` = if ("yearly" %in% names(forecast)) 
          yearly else NA,
        `Términos aditivos` = if ("additive_terms" %in% names(forecast)) 
          additive_terms else NA,
        `Términos multiplicativos` = if ("multiplicative_terms" 
                                         %in% names(forecast)) 
          multiplicative_terms else NA
      ) |> 
      select(fecha, tipo, `Ventas reales`, `Pronóstico`,
             `Límite inferior 95%`, `Límite superior 95%`, `Tendencia`,
             `Tendencia inferior`, `Tendencia superior`, `Estacionalidad anual`,
             `Términos aditivos`, `Términos multiplicativos`) |> 
      arrange(fecha)
    
    x
    
  })
  
  
  output$chart_descomp <- renderReactable({
    
    fmt_money <- colFormat(prefix = "$", separators = TRUE, digits = 0)
    fmt_pct <- colFormat(percent = TRUE, digits = 2)
    
    cols_money <- c("Ventas reales", "Pronóstico", "Límite inferior 95%", 
                    "Límite superior 95%", "Tendencia", "Tendencia inferior", 
                    "Tendencia superior", "Términos aditivos")
    
    cols_pct <- c("Estacionalidad anual", "Términos multiplicativos")
    
    tabla_reactive() |> 
      reactable(
        theme = tema_reactable(), defaultPageSize = 12, searchable = FALSE,
        striped = TRUE, highlight = TRUE,
        defaultColDef = colDef(
          minWidth = 130,
          headerStyle = list(background = color_header(), 
                             color = "#ffffff", fontWeight = "600", 
                             fontSize = "12px"),
          style = list(color = color_texto(), fontSize = "12px")),
        columns = c(
          list(fecha = colDef(name = "Mes", minWidth = 95),
               tipo = colDef(name = "Tipo", minWidth = 100)),
          setNames(lapply(cols_money, \(z) colDef(align = "right", 
                                                  format = fmt_money)), cols_money),
          setNames(lapply(cols_pct, \(z) colDef(align = "right", 
                                                format = fmt_pct)), cols_pct)
        )
      )
  })
  
  
  output$download_forecast <- downloadHandler(
    filename = function() paste0("componentes_forecast_", Sys.Date(), ".xlsx"),
    content = function(file) {
      x <- tabla_reactive()
      n_rows <- nrow(x); n_cols <- ncol(x)
      
      cols_money <- c("Ventas reales", "Pronóstico", "Límite inferior 95%", "Límite superior 95%",
                      "Tendencia", "Tendencia inferior", "Tendencia superior", "Términos aditivos")
      cols_pct <- c("Estacionalidad anual", "Términos multiplicativos")
      
      money_cols <- which(names(x) %in% cols_money)
      pct_cols   <- which(names(x) %in% cols_pct)
      date_cols  <- which(names(x) == "fecha")
      
      wb <- createWorkbook(creator = "Shiny App")
      addWorksheet(wb, "Forecast", gridLines = FALSE)
      
      title_style <- createStyle(fontSize = 16, textDecoration = "bold", fontColour = color_header(), halign = "left")
      subtitle_style <- createStyle(fontSize = 10, fontColour = "#666666", textDecoration = "italic")
      header_style <- createStyle(fontSize = 12, fontColour = "#FFFFFF", fgFill = color_header(),
                                  textDecoration = "bold", halign = "center", valign = "center",
                                  border = "TopBottom", borderColour = "#D9D9D9")
      body_style  <- createStyle(fontSize = 11, fontColour = "#333333", border = "bottom", borderColour = "#EFEFEF")
      money_style <- createStyle(numFmt = '"$"#,##0', halign = "right")
      pct_style   <- createStyle(numFmt = "0.00%", halign = "right")
      date_style  <- createStyle(numFmt = "mmm yyyy", halign = "left")
      
      writeData(wb, "Forecast", "Componentes del pronóstico de ventas", startRow = 1, startCol = 1, colNames = FALSE)
      mergeCells(wb, "Forecast", cols = 1:n_cols, rows = 1)
      addStyle(wb, "Forecast", title_style, rows = 1, cols = 1:n_cols, gridExpand = TRUE)
      
      writeData(wb, "Forecast", paste("Archivo generado el", Sys.Date()), startRow = 2, startCol = 1, colNames = FALSE)
      mergeCells(wb, "Forecast", cols = 1:n_cols, rows = 2)
      addStyle(wb, "Forecast", subtitle_style, rows = 2, cols = 1:n_cols, gridExpand = TRUE)
      
      writeData(wb, "Forecast", x, startRow = 4, startCol = 1, colNames = TRUE, withFilter = TRUE)
      addStyle(wb, "Forecast", header_style, rows = 4, cols = 1:n_cols, gridExpand = TRUE)
      
      if (n_rows > 0) {
        body_rows <- 5:(n_rows + 4)
        
        addStyle(wb, "Forecast", body_style, rows = body_rows, cols = 1:n_cols, gridExpand = TRUE)
        if (length(money_cols) > 0) addStyle(wb, "Forecast", money_style, rows = body_rows, cols = money_cols, gridExpand = TRUE, stack = TRUE)
        if (length(pct_cols) > 0) addStyle(wb, "Forecast", pct_style, rows = body_rows, cols = pct_cols, gridExpand = TRUE, stack = TRUE)
        if (length(date_cols) > 0 && inherits(x$fecha, "Date")) addStyle(wb, "Forecast", date_style, rows = body_rows, cols = date_cols, gridExpand = TRUE, stack = TRUE)
      }
      
      setColWidths(wb, "Forecast", cols = 1:n_cols, widths = "auto")
      setRowHeights(wb, "Forecast", rows = 1, heights = 28)
      freezePane(wb, "Forecast", firstActiveRow = 5, firstActiveCol = 1)
      
      saveWorkbook(wb, file = file, overwrite = TRUE)
    }
  )
  
  
}
