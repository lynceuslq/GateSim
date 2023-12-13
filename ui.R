#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyBS)
library(shinycssloaders)
library(shinydashboard)
library(dashboardthemes)
library(shinyjs)
library(shinyWidgets)
library(ggpubr)
library(survminer)
library(tidyverse)
library(rlang)
library(plyr)
library(tippy)
library(plotly)
library(kableExtra)
library(reactable)

data.base <- list(
  nor.1 = read.table("www/cello.notgate.tsv", header = T, sep = "\t"),
  nor.2 =  read.table("www/notgate.tsv", header = T, sep = "\t"),
  activ.1 = read.table("www/cello.sensor.tsv", header = T, sep = "\t"),
  activ.2 = read.table("www/sensor.tsv", header = T, sep = "\t"),
  and.1 = read.table("www/andgate.tsv", header = T, sep = "\t")
)

js <- '.nav-tabs-custom .nav-tabs li.active {
    border-top-color: #fac002;
}"'

ui <-    dashboardPage(title = "GateSim",
                       skin = "black",
                       dashboardHeader(title = "GateSim", titleWidth = 200),
                       dashboardSidebar(
                         collapsed = T,
                         width = 200,
                         sidebarMenu(
                           menuItem("ODE simulation", icon = icon("bar-chart-o"), tabName = "sim_tab_ode"),
                           menuItem("Stochastic Simulation", icon = icon("bar-chart-o"), tabName = "sim_tab_stoch"),
                           menuItem("Github", icon = icon("github"), href = "https://github.com/lynceuslq"),
                           menuItem("About", icon = icon("question-circle-o"), tabName = "menu_about")
                         )
                       ),
                       dashboardBody(
                         tags$style(js),
                         tabItems(
                           tabItem(tabName = "sim_tab_ode",
                                   fluidRow(
                                         box(title = "Gate parameters", width = 4, solidHeader = T, status = "primary",
                                             fluidRow(
                                               column(3,
                                                      selectInput("func","Select gate: ",choices = names(data.base),selected = names(data.base)[1] )
                                               ),
                                               column(3,
                                                      actionBttn("byrec","Select from record ")
                                               ),
                                               column(3,
                                                      checkboxInput("genran","Random sampling")
                                               ),
                                               column(3,
                                                      uiOutput("script")
                                               )
                                             ),
                                             fluidRow(
                                               column(6,
                                                "Paramter values:",
                                              uiOutput("parlist")
                                               ),
                                              column(6,
                                                     "Initial values:",     
                                             uiOutput("initlist"),
                                             "Time: ",
                                             numericInput("start","Start (h)",value=0,min=0),
                                             numericInput("end","End (h)",value=10,min=0),
                                             numericInput("intv","Interval (h)",value=0.1,min=0)
                                              )
                                             ),
                                             fluidRow(
                                               column(6),
                                             column(4, actionBttn("simu", "Simulate",style = "stretch",color="primary"))
                                             )
                                         ),
                                         box(title = "Gate simulation", width = 8,collapsible =T, solidHeader = T, status = "success",
                                            plotlyOutput("plotode")
                                             
                                             ),
                                         box(title = "Gate simulation with random sampling",collapsible =T, width = 8, solidHeader = T, status = "success",
                                             plotlyOutput("plotrand"),
                                             plotlyOutput("postdist")
                                             
                                         )
                                     ),
                                   fluidRow(
                                     box(title = "Parameter table", width = 8, solidHeader = T, status = "success",
                                        # DT::dataTableOutput("record_tab")
                                   )
                                   
                                   )
                           ),
                         tabItem(tabName = "sim_tab_stoch",
                                 fluidRow(
                                   
                                 )),
                         tabItem(tabName = "menu_about" )
                         )
                       )
                       
)
