## app.R ##
library(shinyWidgets)
library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)
library(shinyjs)
library(tidyr)
library(memoise)
library(highcharter)
library(markdown)

source('global.R')
importShinyModules()

ui <- dashboardPage(
    skin = 'purple',
    dashboardHeader(title = "Demo Dashboard"),
    dashboardSidebar(
        sidebarMenu(id = "sidebar_menu",
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("About", tabName = "about", icon = icon("info-circle"))
        )
    ),
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "dashboard",
                    dashboard_ui('dashboard')
            ),
            
            # Second tab content
            tabItem(tabName = "about",
                    about_ui('about')
            )
        )
    )
)

server <- function(input, output) {
  
  # Tab 1: Server
  dashboard_server('dashboard')
  # Tab 2: Server 
  about_server('about')
}

shinyApp(ui, server)