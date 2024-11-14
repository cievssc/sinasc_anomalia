 #app Painel Sinasc (15-out-24, 14:48h)
 
 shinyServer(function(input, output, session) {

 #organizando variáveis
 #
 
    #----------------------------------------------------------------------
    #nascidos vivos
    source('./server/server_nascido.R', local = T, encoding = 'UTF-8')

    #hospital
    source('./server/server_hospital.R', local = T, encoding = 'UTF-8')

     #anomalia
    source('./server/server_anomalia.R', local = T, encoding = 'UTF-8')

    
 }) #end server function
