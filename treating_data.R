 #cod6 para o mapa
 #municipiopoly$cod6 <- floor(as.numeric(municipiopoly$CD_GEOCMU)/10)
 municipiosf$cod6 <- floor(as.numeric(municipiosf$CD_GEOCMU)/10)
 #municipiopoly$codigo <- as.numeric(municipiosf$CD_GEOCMU)

 centroide <- sf::st_centroid(municipiosf)
 centroide <- as.data.frame(sf::st_coordinates(centroide))
 centroide <- data.frame('cod6' = municipiosf$cod6, centroide)
              
 dados$codestab <- as.numeric(dados$codestab)
 #agregando os dados de regionais aos dados da sinasc
 dados <- dplyr::left_join(dados, tab_regioes[,c(3:4)], by = c('codmunnasc' = 'cod6')) %>%
          dplyr::left_join(., tab_regioes[,3:4], by = c('codmunres' = 'cod6'), suffix = c('.nasc', '.res')) %>%
          dplyr::left_join(., dado_estab, by = c('codestab' = 'co_cnes'))


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