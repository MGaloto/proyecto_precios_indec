

source('global_indec.R', encoding = 'UTF-8')







ui = fluidPage(
  h1(
    'Inflacion Argentina',  
    style="color:white"),
  setBackgroundColor("#191970"),
  p('Dashboard sobre la inflacion en Argentina entre 2017 y 2021',
    style="color:white"), 
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = 'day',
                  label = p('Seleccione el Indice Inflacionario: ',  style="color:white"),
                  choices = sort(variables_totales),
                  selected = 'Nivel general'), style = "background: #191970"
    ),
    mainPanel(plotlyOutput(outputId = 'grafico1'))
  )
)




server = function(input, output){
  output$grafico1 = renderPlotly({
    
    
    indect %>%  select(variable = input$day) %>% plot_ly() %>% 
      add_trace(x = ~x, y = ~variable[indect$Año == "2017"], name= "2017", type = 'bar',
                textposition = 'auto', 
                marker = list(color = "#2e76b8", alpha = 0.5,
                              line = list(color = 'black', width = 1.5))) %>% 
      add_trace(x = ~x, y = ~variable[indect$Año == "2018"], name= "2018", type = 'bar',
                textposition = 'auto',
                marker = list(color = "#2bb0d9", alpha = 0.5,
                              line = list(color = 'black', width = 1.5))) %>% 
      add_trace(x = ~x, y = ~variable[indect$Año == "2019"], name= "2019", type = 'bar',
                textposition = 'auto',
                marker = list(color = "#a734d9", alpha = 0.5,
                              line = list(color = 'black', width = 1.5))) %>% 
      add_trace(x = ~x, y = ~variable[indect$Año == "2020"], name= "2020", type = 'bar',
                textposition = 'auto',
                marker = list(color = "#b01a15", alpha = 0.5,
                              line = list(color = 'black', width = 1.5))) %>% 
      add_trace(x = ~x, y = ~append(variable[indect$Año == "2021"],rep(0, each=agregar_valores)), name= "2021", type = 'bar',
                textposition = 'auto',
                marker = list(color = "#eb5bc2", alpha = 0.5,
                              line = list(color = 'black', width = 1.5))) %>% 
      layout(title = "Inflacion mensual" ,
             barmode = 'group', 
             xaxis = list(title = "Mes"),
             yaxis = list(title = "% Inflacion")) %>% 
      layout(plot_bgcolor='#191970', 
             paper_bgcolor='#191970', 
             font = list(color = '#FFFFFF'))
    
    
  })
  
  
  
}

shinyApp(ui = ui, server = server)
