 #carregando os pacotes...
 library('shiny')
 library('shinyWidgets')
 library('leaflet')
 library('leaflet.extras')
 library('leaflet.extras2')
 library('htmltools')
 library('shinycssloaders') #add em 18-nov-2022(14:07h)

 library('dplyr')        #manipulação de dados - tydiverse
 library('stringr')      #funções de string  - tydiverse
 library('sf') #plot maps
 library('magrittr')     #para mudar nome de colunas
 
 library('RColorBrewer')
 library('lubridate') #para operações de data
 library('reactable') #análogo ao DT
 
 #tabelas
 library('kableExtra')
 #library('formattable') #tabelas personalizadas
 
 #carregand dados
 
 source('./variaveis_ext.R', local = T, encoding = 'UTF-8')
 source('./treating_data.R', local = T, encoding = 'UTF-8')
 options(warn = -1)
 
 #apexcharts
 source('./www/apexchart/general_apex.R')
 
 #echarts (add em 13-jun-2023)
 #source('./www/echarts/general_echarts.R')
 
 #carregando funçes dashboard
 source('./www/tablerdash/funcoes_dashboard.R')
 source('./www/tablerdash/cards.R')
 source('./www/tablerdash/icons.R')
 

 