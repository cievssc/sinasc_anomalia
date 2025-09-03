 #--------------
 #lendo o organizando os dados do Sinasc (14-ago-2025)

  #------Carregando os pacotes necessrios------------------------------------------#
 library('reshape2')
 library('scales')
 library('dplyr')
 library('data.table') 
 library('read.dbc') #devtools::install_github('danicat/read.dbc')

 library('stringr')      #funÇOes de string  - tydiverse
 library('downloader')	  # downloads and then runs the source() function on scripts from github
 library('magrittr')     #para mudar nome de colunas
 library('RCurl')


 link <- 'ftp://ftp.datasus.gov.br/dissemin/publicos/SINASC/1996_/Dados/DNRES'
 temporario_dir <- tempdir()

 dados <- lapply(2012:2023, function(x, y = 'SC'){ #mudar a UF aqui!
 
 dbf <- paste0('DN', y,x, '.dbc')
 download.file(file.path(link, dbf), destfile = file.path(temporario_dir, dbf), mode = 'wb')
 dadoi <- read.dbc::read.dbc(file.path(temporario_dir, dbf))
 names(dadoi) <- tolower(names(dadoi))
 dadoi <- dadoi[,c('dtnasc','codestab', 'codanomal','codmunnasc','codmunres','idademae','gestacao','gravidez','parto','consultas',
        'apgar5','peso','idanomal','semagestac','mesprenat','consprenat')] #selecionando variáveis de interesse
 dadoi[,-c(1,2,3)] <- sapply(dadoi[,-c(1,2,3)], function(x){as.numeric(as.character(x))}) #transformando as variáveis de interesse em numéricas

 dadoi$cat_peso  <- sapply(1:nrow(dadoi),function(x){
                if(is.na(dadoi[x,'peso'])){NA}else{
                if(dadoi[x,'peso'] < 1000 ){return('Extremamente baixo')}
                if(dadoi[x,'peso'] >= 1000 & dadoi[x,'peso'] < 1500){return('Muito baixo')}
                if(dadoi[x,'peso'] >= 1500 & dadoi[x,'peso'] < 2500){return('Baixo')}
                if(dadoi[x, 'peso'] >= 2500){return('Acima de 2500g')}}
 }) %>% factor(., levels = c('Extremamente baixo', 'Muito baixo', 'Baixo', 'Acima de 2500g'))

 dadoi$cat_gestacao <- sapply(1:nrow(dadoi),function(x){
                dado <- dadoi[x, 'gestacao']
                if(is.na(dado)){NA}else{
                switch(as.character(dado),
                '1' = 'Menos de 22 semanas',
                '2' = '22 à 27 semanas',
                '3' = '28 à 31 semanas',
                '4' = '32 à 36 semanas',
                '5' = '37 à 41 semanas',
                '6' = '42 ou mais semanas',
                '9' =' Ignorado')}
 })  %>% factor(., levels = c('Menos de 22 semanas', '22 à 27 semanas', '28 à 31 semanas',
                    '32 à 36 semanas', '37 à 41 semanas', '42 ou mais semanas', 'Ignorado'))

 dadoi$cat_parto <- sapply(1:nrow(dadoi),function(x){
                        if(is.na(dadoi[x,'parto'])){NA}else{
                           switch(as.character(dadoi[x,'parto']),
                             '1' = 'Vaginal',
                             '2' = 'Cesariana',
                             '9' = 'Ignorado')
                        }
 }) %>% factor(., levels = c('Vaginal', 'Cesariana', 'Ignorado'))

 dadoi$cat_consultas <- sapply(1:nrow(dadoi),function(x){
                        if(is.na(dadoi[x,'consultas'])){NA}else{
                            switch(as.character(dadoi[x,'consultas']),
                            '1' = 'Nenhuma',
                            '2' = 'De 1 à 3',
                            '3' = 'De 4 à 6',
                            '4' = '7 ou mais',
                            '9' = 'Ignorado')
                        }
 }) %>% factor(., levels = c('Nenhuma', 'De 1 à 3', 'De 4 à 6', '7 ou mais', 'Ignorado'))

 dadoi$cat_apgar5 <- sapply(1:nrow(dadoi),function(x){
                        if(is.na(dadoi[x,'apgar5'])){NA}else{
                            if(dadoi[x,'apgar5'] < 7){'Até 7'}else{'Maior que 7'}
                        }
 }) %>% factor(., levels = c('Até 7', 'Maior que 7'))

 dadoi$cat_prenatal <- sapply(1:nrow(dadoi),function(x){
                        if(is.na(dadoi[x,'mesprenat']) | dadoi[x,'mesprenat'] > 9){NA}else{
                            if(dadoi[x,'mesprenat'] <= 3){'Até 3º mês'}else{'4º mês em diante'}
                        }
 }) %>% factor(., levels = c('Até 3º mês', '4º mês em diante'))

 dadoi$cat_qtdprenatal <-  sapply(1:nrow(dadoi),function(x){
                            if(is.na(dadoi[x,'consprenat'])){NA}else{
                                if(dadoi[x,'consprenat'] < 7){return('Menos de 7 consultas')}
                                if(dadoi[x,'consprenat'] >= 7){return('7 ou mais consultas')}
                            }
 }) %>% factor(., levels = c('Menos de 7 consultas', '7 ou mais consultas'))
 

 #add em 12-nov-2024 (14:39h)
 dadoi$cat_idademae <-  cut(dadoi$idademae, breaks = c(-Inf, 14,19,24,29,34,39,44,60, Inf), 
                            labels =  c('Até 14', '15 à 19','20 à 24', '25 à 29', '30 à 34',
                            '35 à 39', '40 à 44', 'Mais de 45', 'Ignorado'), right = T)

 dadoi$ano <- x
 dadoi$mes <- substr(dadoi$dtnasc, 3,4) %>% as.numeric

 dadoi[, c('ano','mes','codestab', 'codmunnasc','codmunres','idanomal','codanomal','cat_prenatal','cat_peso','cat_gestacao',
 'cat_consultas', 'cat_apgar5', 'cat_parto', 'mesprenat', 'cat_idademae')]
 }) %>% do.call('rbind',.) %>% as.data.frame

 #salvando os dados 
 save(dados, file = 'sinasc_data.RData')
