 #' script para construção de mapas e regionalização
 #' necessário ter o pacote 'geobr' instalado 
 #' install.packages('geobr')
 #' install.packages('readODS')

 ## exemplo aqui está para o estado de Santa Catarina e regioes

 #lendo os dados da divisão territorial brasileira (https://www.ibge.gov.br/geociencias/organizacao-do-territorio/divisao-regional/23701-divisao-territorial-brasileira.html)
 tab_regioes <- readODS::read_ods('getting_data/RELATORIO_DTB_BRASIL_2024_MUNICIPIOS.ods', skip = 6) %>% as.data.frame() %>%
                janitor::clean_names()

 ### Selecionando a uf e a região de interesse

 tab_regioes <- tab_regioes[tab_regioes$uf == 42, c('nome_regiao_geografica_imediata', 'codigo_municipio_completo', 'nome_municipio')]
 names(tab_regioes) <- c('reg_saude', 'codigo', 'municipio')
 tab_regioes$cod6 <- floor(tab_regioes$codigo/10)

 save(tab_regioes, file = 'regioes_saude.RData')
 
 #mapa dos municípios
 municipiosf <- geobr::read_municipality(code_muni="SC", year=2024)
 #municipiosf <- municipiosf  %>% sf::st_transform(., "+proj=eqc")
 save(municipiosf, file = 'municipiosf.RData')

 #mapa das regioes
 mapa_regionais <- dplyr::left_join(municipiosf, tab_regioes[,c('codigo', 'reg_saude')], by = c('code_muni' = 'codigo')) %>%
                    group_by(reg_saude) %>%
                    summarise() # %>% sf::st_transform(., "+proj=eqc")

 save(mapa_regionais, file = 'mapa_regionais.RData')



