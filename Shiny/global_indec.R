

library(shiny)
library(tidyverse)
library(ggplot2)
library(corrplot) 
library(GGally)
library(readr)  
library(dplyr)  
library(crayon) 
library(modeest)
library(readxl)
library(plotly)
library(ggthemes)
library(reshape)
library(shinyWidgets)
library(gdata)

# 1- Descargamos de la pagina del Indec los indices de precios generales:
#    Índices y variaciones porcentuales mensuales e interanuales según principales aperturas de la canasta. 
#    Buscamos el archivo en la carpeta y lo importamos:
#    Pagina del Indec: https://www.indec.gob.ar/indec/web/Nivel4-Tema-3-5-31



setwd("C:/Users/maxig/Desktop/Carpetas/Trabajos en R/Indec/Shiny App Indec")

download.file("https://www.indec.gob.ar/ftp/cuadros/economia/sh_ipc_aperturas.xls","indec_descargado.xls",mode="wb")

indec = read_excel('indec_descargado.xls')


# 
# indec = read_excel("C:/Users/maxig/Desktop/Carpetas/Trabajos en R/Indec/sh_ipc_aperturas.xls", 
#                     col_names = TRUE, skip = 0)

# 2- Nos quedamos con la fila 8 a la 52 donde estan los indices generales 


indec = indec[8:52,]


# 3- Renombramos las columnas 


colnames(indec) = c("Rubro", "01/17", "02/17", "03/17", "04/17", 
                    "05/17", "06/17", "07/17" ,"08/17" ,"09/17" ,
                    "10/17", "11/17", "12/17" ,"01/18" ,"02/18", 
                    "03/18", "04/18", "05/18", "06/18", "07/18",
                    "08/18", "09/18", "10/18", "11/18", "12/18", 
                    "01/19", "02/19", "03/19", "04/19", "05/19", 
                    "06/19", "07/19", "08/19", "09/19", "10/19", 
                    "11/19", "12/19", "01/20", "02/20", "03/20",
                    "04/20", "05/20" ,"06/20", "07/20", "08/20",
                    "09/20", "10/20", "11/20", "12/20", "01/21",
                    "02/21", "03/21","04/21","05/21","06/21",
                    "07/21","08/21","09/21","10/21","11/21","12/21")


# 4- Trasponemos el DataSet


indect = data.frame(t(indec))



#5 - Se crea una variable con el nombre de las filas que son los %m/%Y


indec1names = row.names(indect)[-1.]


#6 - Se crea un vector con los nombres de la primera fila transpuesta


vector = c(indect[1,])



#7 - se utiliza el vector del paso anterior para cambiarle el nombre a las columnas


colnames(indect) = vector


#8 - Se elimina la primer fila que utilizamos para hacernos de los nombres de las variables


indect = indect[-1,]


#9 - Se cambian los nombres de las filas por numero ascendentes


row.names(indect) = 1:nrow(indect)


# limpieza del data set


indect[indect == "///"] <- '0'
indect[indect == "////"] <- '0'
indect[indect == "//"] <- '0'

#10 - Se crean variables: Periodo, Año, Mes


indect$Periodo = indec1names # Se usa la variable creada antes

indect$Periodo = paste0("01/",indect$Periodo) # Se le pega el dia adelante para poder pasar a fecha despues

indect$Periodo = as.Date(indect$Periodo, format = "%d/%m/%y") # Se pasa a formato fecha

indect$Mes = substr(indect$Periodo, 6 , 7) # Se crea la variable mes

indect$Año = substr(indect$Periodo, 1 , 4) # Se crea la variable año




#11 -  se pasan a numéricas todas las variables



indect[ , c(1:(ncol(indect) - 3 ))] = apply(indect[ , c(1:(ncol(indect) - 3 ))], 2, trimws )   # elimino los espacios vacíos


indect[ , c(1:(ncol(indect) - 3 ))] = apply(indect[ , c(1:(ncol(indect) - 3 ))], 2,     
                                            function(x) as.numeric(as.character(x)))



apply(is.na(indect), 2, sum)  # cantidad de na por columna 


indect$`Restaurantes y comidas fuera del hogar`[is.na(indect$`Restaurantes y comidas fuera del hogar` == TRUE)] = round(mean(indect$`Restaurantes y comidas fuera del hogar`, na.rm = TRUE),1)

indect$Calzado[is.na(indect$Calzado == TRUE)] = round(mean(indect$Calzado, na.rm = TRUE),1)

indect$`Prendas de vestir y materiales`[is.na(indect$`Prendas de vestir y materiales` == TRUE)] = round(mean(indect$`Prendas de vestir y materiales`, na.rm = TRUE),1)


#12 - Se crea la variable nivel general interanual


interanual = ((indect$`Nivel general`/100)+1) *  ((lag(indect$`Nivel general`, 1)/100)+1)	* 
  ((lag(indect$`Nivel general`, 2)/100)+1)		*  ((lag(indect$`Nivel general`, 3)/100)+1)* 
  ((lag(indect$`Nivel general`, 4)/100)+1)				*  ((lag(indect$`Nivel general`, 5)/100)+1)		*  
  ((lag(indect$`Nivel general`, 6)/100)+1)	*  ((lag(indect$`Nivel general`, 7)/100)+1)		* 
  ((lag(indect$`Nivel general`, 8)/100)+1)				*  ((lag(indect$`Nivel general`, 9)/100)+1)	*  
  ((lag(indect$`Nivel general`, 10)/100)+1)				*  ((lag(indect$`Nivel general`, 11)/100)+1)		

interanual = round(((interanual -1 ) * 100),2)

indect$Interanual = interanual



# nombre de variables


variables_totales = colnames(indect[1:(ncol(indect) - 4 )])


#13 - Grafico de barras del nivel general


grafico_barras =  indect %>% 
  ggplot(aes(x=Mes, y=`Nivel general`)) +
  geom_bar(stat = "identity", color = "grey", alpha = 0.6, fill = "#f50000") +
  ggtitle("Inflacion en Bebidas Alcoholicas") +
  xlab("Mes") + ylab("% ") +
  theme(plot.title = element_text(hjust = 0.5, colour = "black"),
        plot.background = element_rect(fill = "#edf0ee"),
        panel.background = element_rect(fill = "#edf0ee"),
        axis.text = element_text(colour = "black"),
        axis.title = element_text(colour = "black")) +
  facet_wrap(.~Año)


#13 - Creamos algunas variables de utilidad


variable = indect$`Nivel general`

x = indect$Mes[1:12]

años = c('2017','2018','2019','2020','2021')

y_2017 = variable[indect$Año == '2017']

y_2018 = variable[indect$Año == '2018']

y_2019 = variable[indect$Año == '2019']

y_2020 = variable[indect$Año == '2020']

y_2021 = variable[indect$Año == '2021']

agregar_valores = length(y_2020) - length(y_2021)


y_2021 = append(variable[indect$Año == '2021'],rep(0, each=agregar_valores))

text = c('Enero','Febrero','Marzo', 'Abril', 'Mayo', 'Junio', 'Julio', 'Agosto', 'Septiembre',
         'Octubre', 'Noviembre', 'Diciembre')

#13 - grafico de inflacion nivel general

variables_totales = as.factor(variables_totales)

write.csv(indect,"indect.csv", row.names = F)