 #cod6 para o mapa
 #municipiopoly$cod6 <- floor(as.numeric(municipiopoly$CD_GEOCMU)/10)
 municipiosf$cod6 <- floor(as.numeric(municipiosf$code_muni)/10)
 names(municipiosf)[2] <- 'Municipio'
 #municipiopoly$codigo <- as.numeric(municipiosf$CD_GEOCMU)

 centroide <- sf::st_centroid(municipiosf)
 centroide <- as.data.frame(sf::st_coordinates(centroide))
 centroide <- data.frame('cod6' = municipiosf$cod6, centroide)
              
 dados$codestab <- as.numeric(as.character(dados$codestab))
 #agregando os dados de regionais aos dados da sinasc
 dados <- dplyr::left_join(dados, tab_regioes[,c('cod6', 'reg_saude')], by = c('codmunnasc' = 'cod6')) %>%
          dplyr::left_join(., tab_regioes[,c('cod6', 'reg_saude')], by = c('codmunres' = 'cod6'), suffix = c('.nasc', '.res')) %>%
          dplyr::left_join(., dado_estab, by = c('codestab' = 'co_cnes'))

 #transformando vetor da tabela acp em factor
 tabela_acp[,2] <- factor(tabela_acp[,2], levels = unique(tabela_acp[,2]))

 #----------------------------------------------
 #funções para tratamento de dados (08-nov-2024, 13:16h)
 #detectar as linhas com acp
 func_acp <- function(x,y){ #x = vetor de códigoos de anomalia / y == vetor com os códigos da anomalia
    vetor <- stringr::str_extract_all(x, "\\w{4}")

    tabela_acp <- y
    sapply(vetor, function(z){
    any(z %in% tabela_acp)
  })
 }


 #função para obter data de atualização
 data_atual <- file.info('sinasc_data.RData')$mtime
 data_atual <- format(data_atual, '%d-%m-%Y')
