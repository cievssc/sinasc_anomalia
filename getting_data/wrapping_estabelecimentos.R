 #wrapper dos dados de estabelecimento
 #atualizado em 26-fev-2025, 16:45h
 #notas
 #1- baixar e descompactar o arquivo "BASE_DE_DADOS_CNES_aaaamm.ZIP" em ftp.datasus.gov.br/cnes 

 temporario_dir <- tempdir()
 link <- 'ftp://ftp.datasus.gov.br/cnes'
 arquivo <- 'BASE_DE_DADOS_CNES_202507.ZIP' 
 arquivo_estab <- paste0('tbEstabelecimento202507.csv')

 options(timeout = 3600) #aumentando o tempo de timeout, em vista que o arquivo leva um certo tempo pra baixar
 download.file(file.path(link, arquivo), destfile = file.path(temporario_dir, arquivo), mode = 'wb', cacheOK = F)
 unzip(file.path(temporario_dir, arquivo) ,  files = c(arquivo_estab), exdir = temporario_dir)
 
 dado_estab <- read.csv(file.path(temporario_dir,arquivo_estab), sep = ';', header=T, stringsAsFactors=FALSE, fileEncoding="latin1")
 names(dado_estab) <- tolower(names(dado_estab))
 
 dado_estab <- subset(dado_estab, co_estado_gestor == 42) 
 #classificando o estabelecimento entre Público e Privado, a partir da natureza juridica
 dado_estab$natureza <- with(dado_estab, substr(co_natureza_jur, 1,2)) %>% as.numeric
 dado_estab$natureza <- sapply(1:nrow(dado_estab), function(x){
    if(dado_estab$natureza[x] < 20){return('Pública')}else{return('Privada')}
 }) %>% factor(., levels = c('Pública', 'Privada'))

 dado_estab$nu_latitude %<>% as.numeric
 dado_estab$nu_longitude %<>% as.numeric

 dado_estab <- dado_estab[, c('co_cnes','co_municipio_gestor','no_fantasia', 'nu_latitude','nu_longitude', 'natureza')]
 
 save(dado_estab, file = 'estab_saude.RData')
