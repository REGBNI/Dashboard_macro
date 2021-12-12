#Dashboard de indicadores macroecómicos  con SHINY app
#Elaborado por Raúl Enrique Godínez Balmaceda

#Dashboard of macroeconomic indicators with the SHINY application
#Created by Raúl Enrique Godínez Balmaceda


#Nota: La fuente de datos es el Banco Central de Nicaragua, los cuales fueron procesados previamente.
#Note: The data source is the Central Bank of Nicaragua, which were previously processed.



#Librerias a utilizar
#Used libraries
library(shiny)
library(bslib)
library(thematic)
library(readxl)
library(tidyverse)
library(dplyr)
library(plotly)
library(DT)
library(shinyBS)
library(shinyAce)
library(magrittr)
library(TSstudio)
library(gsheet)
library(openxlsx)


#Cargar datos
#Upload data

imae_tc =  read_excel("imae_tc.xlsx") #IMAE tendencia Ciclo/ Monthly index of economic activity (MIEA) Cycle trend
imae_de =  read_excel("imae_de.xlsx") #IMAE desestacionalizada/ MIEA seasonally adjusted
imae_so =  read_excel("imae_so.xlsx") #IMAE serie original/ MIEA original series
x=  read_excel("x.xlsx") #Exportaciones/Exports
m =  read_excel("m.xlsx") #Importaciones/Imports


#Procesamiento de los datos/Data processing
itc_im = round(imae_tc$`value Intermensual (1,1)`[176:188],2)
inicio <- as.Date("2020-08-01")
fin <- as.Date("2021-08-01")
year = seq(from=inicio, by="month", length.out=13)
itc_im = data.frame(year,itc_im)

  #interanual TC/Interannual CT

  itc_ia = round(imae_tc$`value Interanual (T1,12)`[176:188],2)
  inicio <- as.Date("2020-08-01")
  fin <- as.Date("2021-08-01")
  year = seq(from=inicio, by="month", length.out=13)
  itc_ia = data.frame(year,itc_ia)


  #intermensual DE/ Intermonthly SA
  ide_im = round(imae_de$`value Intermensual (1,1)`[176:188],2)

  inicio <- as.Date("2020-08-01")
  fin <- as.Date("2021-08-01")
  year = seq(from=inicio, by="month", length.out=13)
  ide_im = data.frame(year,ide_im)

  #interanual DE/Interannual SA

  ide_ia = round(imae_de$`value Interanual (T1,12)`[176:188],2)
  inicio <- as.Date("2020-08-01")
  fin <- as.Date("2021-08-01")
  year = seq(from=inicio, by="month", length.out=13)
  ide_ia = data.frame(year,ide_ia)

  #interanual SO/ Interannual OS
  iso_ia = round(imae_so$`value Interanual (T1,12)`[176:188],2)
  inicio <- as.Date("2020-08-01")
  fin <- as.Date("2021-08-01")
  year = seq(from=inicio, by="month", length.out=13)
  iso_ia = data.frame(year,iso_ia)


  #promedio anual SO/ Anual average OS 
  iso_im = round(imae_so$`value Promedio anual (T12,12)`[176:188],2)
  inicio <- as.Date("2020-08-01")
  fin <- as.Date("2021-08-01")
  year = seq(from=inicio, by="month", length.out=13)
  iso_im= data.frame(year,iso_im)
  
  #Importaciones/Imports
  m_t = data.frame(m$variable, m$value)
  
  inicio <- as.Date("2006-01-01")
  fin <- as.Date("2021-08-01")
  year = seq(from=inicio, by="month", length.out=188)
  m_t= data.frame(rep(year,4),m_t)
  colnames(m_t) = c("year","variable","value")
  



#Convertir la data en tipo ts (Series de tiempo)/#Convert the data to type ts (Time series)

imae_tc_s = ts(imae_tc$`value Interanual (T1,12)`, start=c(2007, 1), frequency=12)
imae_tc_s_im = ts(imae_tc$`value Intermensual (1,1)`, start=c(2006, 2), frequency=12)

imae_de_s = ts(imae_de$`value Interanual (T1,12)`,start=c(2007, 1), frequency=12)
imae_de_i = ts(imae_de$`value Intermensual (1,1)`,start=c(2006, 2), frequency=12)

imae_so_s = ts(imae_so$`value Interanual (T1,12)`,start=c(2007, 1), frequency=12)
imae_so_i = ts(imae_so$`value Promedio anual (T12,12)`,start=c(2007, 1), frequency=12)

x_s =  ts(x[1:188,4],start=c(2006, 1), frequency=12)
xz_s =  ts(x[189:328,4],start=c(2006, 1), frequency=12)

m_s = ts(m[1:188,4],start=c(2006, 1), frequency=12)


#Graficos series de tiempo/Time series plots

itc_is_p = ts_plot(imae_tc_s_im, color = "#001B2E",title = "Crecimiento intermensual IMAE-TC (%)", slider =FALSE)
itc_p = ts_plot(imae_tc_s,color = "#9b2915", title = "Crecimiento interanual IMAE TC (%)", slider =FALSE)

ide_p =  ts_plot(imae_de_s,color = "rgb(205, 12, 24)", title = "Crecimiento interanual IMAE DE (%)", slider =TRUE)
ide_is_p =  ts_plot(imae_de_i,color = "#00798c", title = "Crecimiento intermensual IMAE DE (%)", slider =TRUE)

iso_p =  ts_plot(imae_so_s,color = "#0b6e4f", title = "Crecimiento interanual IMAE Original (%)", slider =FALSE)
iso_i_p =  ts_plot(imae_so_i,color = "#ddb771", title = "Crecimiento promedio anual IMAE Original (%)", slider =FALSE)


xp =  ts_plot(x_s,color = "#ec9a29", title = "Exportaciones de mercancías (Millones USD)", slider =TRUE)
xzp =  ts_plot(xz_s,color = "#001a23", title = "Exportaciones de zona francas (Miles USD)", slider =TRUE)
mp = ts_plot(m_s,color = "#0f8b8d", title = "Importaciones de mercancías (Millones USD)", slider =TRUE)



mp_t= plot_ly(m_t, 
              x = ~year, 
              y = ~value, color = ~variable) %>%
  group_by(variable) %>%
  add_lines() %>% 
  layout(legend = list(orientation = "h", itemsizing = 'constant'),
         title = "Importaciones por tipo de uso (Millones de USD)",
         
         xaxis = list(
           title = "",
           zerolinecolor = "#ffff",
           gridcolor = "#ffff"
         ),
         yaxis =  list(
           title = "",
           zerolinecolor = "#ffff",
           gridcolor = "#ffff"
         ))



#Graficos de barra/Bar chart

    #intermensual_tc
    itc_im_b = plot_ly(itc_im, y=~itc_im, x = ~year, type = 'bar', 
                       marker = list(color = "#001B2E")) %>%
      layout(title = 'Crecimiento intermensual (2020-2021)',
             xaxis = list( 
               title = "",
               zerolinecolor = '#ffff', 
               zerolinewidth = 2, 
               gridcolor = 'ffff'), 
             yaxis = list( 
               title ="",
               zerolinecolor = '#ffff', 
               zerolinewidth = 2, 
               gridcolor = 'ffff'))

    itc_ia_b = plot_ly(itc_ia, y=~itc_ia, x = ~year, type = 'bar', 
                       marker = list(color = "#9b2915")) %>%
      layout(title = 'Crecimiento interanual (2020-2021)',
             xaxis = list( 
               title = "",
               zerolinecolor = '#ffff', 
               zerolinewidth = 2, 
               gridcolor = 'ffff'), 
             yaxis = list( 
               title ="",
               zerolinecolor = '#ffff', 
               zerolinewidth = 2, 
               gridcolor = 'ffff'))


      ide_im_b = plot_ly(ide_im, y=~ide_im, x = ~year, type = 'bar', 
                         marker = list(color = "#00798c")) %>%
        layout(title = 'Crecimiento intermensual (2020-2021)',
               xaxis = list( 
                 title = "",
                 zerolinecolor = '#ffff', 
                 zerolinewidth = 2, 
                 gridcolor = 'ffff'), 
               yaxis = list( 
                 title ="",
                 zerolinecolor = '#ffff', 
                 zerolinewidth = 2, 
                 gridcolor = 'ffff'))

      ide_ia_b = plot_ly(ide_ia, y=~ide_ia, x = ~year, type = 'bar', 
                         marker = list(color = "rgb(205, 12, 24)")) %>%
        layout(title = 'Crecimiento interanual (2020-2021)',
               xaxis = list( 
                 title = "",
                 zerolinecolor = '#ffff', 
                 zerolinewidth = 2, 
                 gridcolor = 'ffff'), 
               yaxis = list( 
                 title ="",
                 zerolinecolor = '#ffff', 
                 zerolinewidth = 2, 
                 gridcolor = 'ffff'))
      
      iso_ia_b = plot_ly(iso_ia, y=~iso_ia, x = ~year, type = 'bar', 
                         marker = list(color = "#0b6e4f")) %>%
        layout(title = 'Crecimiento interanual (2020-2021)',
               xaxis = list( 
                 title = "",
                 zerolinecolor = '#ffff', 
                 zerolinewidth = 2, 
                 gridcolor = 'ffff'), 
               yaxis = list( 
                 title ="",
                 zerolinecolor = '#ffff', 
                 zerolinewidth = 2, 
                 gridcolor = 'ffff'))
      
      iso_im_b = plot_ly(iso_im, y=~iso_im, x = ~year, type = 'bar', 
                         marker = list(color = "#ddb771")) %>%
        layout(title = 'Crecimiento promedio anual (2020-2021)',
               xaxis = list( 
                 title = "",
                 zerolinecolor = '#ffff', 
                 zerolinewidth = 2, 
                 gridcolor = 'ffff'), 
               yaxis = list( 
                 title ="",
                 zerolinecolor = '#ffff', 
                 zerolinewidth = 2, 
                 gridcolor = 'ffff'))


#SHINY APP      
      
#Personalizamos el tema/Customize theme
nic <- bs_theme( bootswatch = "cosmo",
                 bg = "#FFFFFF", fg = "black", primary = "rgb(3, 60, 115)",
                 base_font = font_google("Poppins"),
                 code_font = font_google("Space Mono") 
)




shinyUI(fluidPage(
  
  theme = nic, 
   
  
  navbarPage(
    
    id='mainNavBar',
    title= "Dashboard",
    windowTitle="Nicaragua",
    
    
    
    #--------------Pestañas a crear/Page------------------------------------    
    
    tabPanel(h5("Indicadores mensuales"),
             
             h1(tags$b("Índice Mensual de la Actividad Económica")),
             h5("III trimestre del año 2021"),
             hr(),
             
             tabsetPanel(
               id =  "perio",
               
               
               tabPanel("Serie Tendencia-ciclo",
                        icon = icon("far fa-chart-bar"),
                        hr(),
                        br(),
                        
                        fluidRow(
                          column(6, itc_is_p),
                          column(6, itc_p)),
                        
                        fluidRow(
                          column(6, itc_im_b),
                          column(6, itc_ia_b )
                        )
                        
                        
                        
                        
               ),
               
               
               tabPanel("Seria desestacionalizada",
                        icon = icon("far fa-calendar-alt"),
                        hr(),
                        br(),
                        
                        fluidRow(
                          column(6, ide_is_p),
                          column(6, ide_p)),
                        
                        fluidRow(
                          column(6, ide_im_b),
                          column(6, ide_ia_b )
                        )
                        
                        
                        
               ),
               
               tabPanel("Seria original",
                        icon = icon("chart-line"),
                        hr(),
                        br(),
                        
                        fluidRow(
                          column(6, iso_i_p),
                          column(6, iso_p)),
                        
                        fluidRow(
                          column(6, iso_im_b),
                          column(6, iso_ia_b ))
                        
                        
                        
               ),
               tabPanel("Comercio exterior",
                        icon = icon("globe"),
                        hr(),
                        br(),
                        
                        fluidRow(
                          column(6, xp),
                          column(6, xzp)),
                        
                        fluidRow(
                          column(6, mp),
                          column(6, mp_t))
               )
 
             )),

  ),

))


