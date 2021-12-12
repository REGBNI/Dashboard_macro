library(shiny)
library(readxl)
library(plotly)
library(ggplot2)



shinyServer(function(input, output, session) {
  
  
  output$ep =renderPlotly(ep)
  output$mp =renderPlotly(mp)
  output$xzp =renderPlotly(xzp)
  output$mp_t =renderPlotly(mp_t)
  
  
  output$itc_is_p =renderPlotly(itc_is_p)
  output$itc_p  =renderPlotly(itc_p)
  output$ide_is_p =renderPlotly(ide_is_p)
  output$iso_p =renderPlotly(iso_p )
  output$iso_i_p  =renderPlotly(iso_i_p)
  output$itc_im_b =renderPlotly(itc_im_b)
  output$itc_ia_b =renderPlotly(itc_ia_b)
  output$ide_im_b =renderPlotly(ide_im_b)
  output$ide_ia_b =renderPlotly(ide_ia_b)
  output$iso_im_b =renderPlotly(iso_im_b)
  output$iso_ia_b =renderPlotly(iso_ia_b)
  
  
  
  
  
})
