

frontend <- 
dashboardPage(
  header = dashboardHeader(
    sidebarIcon = icon("home"),
    controlbarIcon = icon("bars"),
    status = "navy",
    fixed = T,
    rightUi = userOutput("user")
  ),
  footer = dashboardFooter(
    left  = "R-conomics",
    right = paste0("@Jorge Hernández, ", year(today()))
  ),
  sidebar = sidebar,
  controlbar = controlbar,
  body = body
)


secure_app(frontend,
           theme = "cosmo",
           background  = "linear-gradient(rgba(0, 0, 0, 0.5),
                    rgba(0, 0, 0, 0.5)),
                    url('https://cdn.pixabay.com/photo/2018/08/21/23/29/
                    forest-3622519_1280.jpg')
                    no-repeat center fixed;
                    -webkit-background-size: cover;
                    -moz-background-size: cover;
                    -o-background-size: cover;
                    background-size: cover;")