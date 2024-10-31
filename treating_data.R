 #cod6 para o mapa
 #municipiopoly$cod6 <- floor(as.numeric(municipiopoly$CD_GEOCMU)/10)
 municipiosf$cod6 <- floor(as.numeric(municipiosf$CD_GEOCMU)/10)
 #municipiopoly$codigo <- as.numeric(municipiosf$CD_GEOCMU)

 centroide <- sf::st_centroid(municipiosf)
 centroide <- as.data.frame(sf::st_coordinates(centroide))
 centroide <- data.frame('cod6' = municipiosf$cod6, centroide)
              

 #agregando os dados de regionais aos dados da sinasc
 dados <- dplyr::left_join(dados, tab_regioes[,c(3:4)], by = c('codmunnasc' = 'cod6')) %>%
          dplyr::left_join(., tab_regioes[,3:4], by = c('codmunres' = 'cod6'), suffix = c('.nasc', '.res'))