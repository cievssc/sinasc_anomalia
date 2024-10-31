 #app Painel Sinasc (15-out-24, 14:48h)
 
 shinyServer(function(input, output, session) {

 #organizando variáveis
 #
 
    #----------------------------------------------------------------------
    #nascidos vivos
    source('./server/server_nascido.R', local = T, encoding = 'UTF-8')

    #casos
    #source('./server/server_casos.R', local = T, encoding = 'UTF-8')

    
 }) #end server function
