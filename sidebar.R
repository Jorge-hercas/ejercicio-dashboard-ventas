
sidebar <-
  dashboardSidebar(
    status = "navy",
    minified = T,
    elevation = 4,
    fixed = T,
    skin = "light",
    bs4SidebarUserPanel("Sales dashboard",
                        image = "shitpost.webp"),
    sidebarMenu(
      id = "menu",
      menuItem(
        text = "General Numbers",
        tabName = "general",
        icon = icon("coins")
      ),
      menuItem(
        text = "Forecast",
        tabName = "forecast",
        icon = icon("chart-line")
      )
    )
  )

