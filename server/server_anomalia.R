  #server para a aba de anomalias congênitas (08--nov-24, 8:55h)
  #----------------------------------------------------------------------

  #opções botões dropdown (TODO, 15-jul-24)
   output$anomalia_dropopcoes <- renderUI({
                               tagList(
                                div(style = 'z-index: 50;',
                                selectInput('anomalia_regiao', 'Região: ', choices = c('Todas', unique(tab_regioes$reg_saude )), 
                                  selected = c('Todas'), multiple = F),
                                selectInput('anomalia_peso', 'Peso:', choices = levels(dados$cat_peso), 
                                  selected = levels(dados$cat_peso), multiple = T),
                                selectInput('anomalia_prenatal', 'Primeiro pré-natal', choices = levels(dados$cat_prenatal),
                                  selected = levels(dados$cat_prenatal), multiple = T),
                                selectInput('anomalia_gestacao', 'Período de gestação', choices = levels(dados$cat_gestacao)[-7],
                                  selected = levels(dados$cat_gestacao)[-7], multiple = T),
                                selectInput('anomalia_consultas', 'Consultas realizadas', choices = levels(dados$cat_consultas)[-5],
                                  selected =  levels(dados$cat_consultas)[-5], multiple = T),
                                selectInput('anomalia_parto','Tipo de parto', choices = levels(dados$cat_parto)[-3],
                                  selected = levels(dados$cat_parto)[-3], multiple = T),
                                selectInput('anomalia_apgar5', 'APGAR5', choices = levels(dados$cat_apgar5), 
                                  selected = levels(dados$cat_apgar5), multiple = T),
                                selectInput('anomalia_meses', 'Meses do ano', choices = c(1:12), selected = c(1:12), multiple = T)
                                

                                )#         
                                        ) #end tag list
   })
  
 
  #organizando os dados
  
  dados_ac <- reactiveVal(NULL)
  
  filtro_acp <- reactive({tabela_acp}) #TODO incrementar o filtro de ACP ()

            observeEvent(dados_all(),{# input$home_atualizar,
             #req(input$anomalia_mapa_leaflet_groups)
                   dadoi <- dados_all()
                   if(input$anomalia_dropdown >0){
                      dadoi <- subset(dadoi, mes %in% input$anomalia_meses) #meses
                      
                      if(input$anomalia_regiao != 'Todas'){
                        dadoi <- subset(dadoi, reg_saude.res == input$anomalia_regiao)
                      }
                      if(length(input$anomalia_apgar5) == 1){
                        dadoi <- subset(dadoi, cat_apgar5 == input$anomalia_apgar5)
                      }
                      if(length(input$anomalia_peso) != 5){
                        dadoi <- subset(dadoi, cat_peso %in% input$anomalia_peso)
                      }
                      if(length(input$anomalia_prenatal) == 1){
                        dadoi <- subset(dadoi, cat_prenatal == input$anomalia_prenatal)
                      }
                      if(length(input$anomalia_consultas) != 4){
                        dadoi <- subset(dadoi, cat_consultas %in% input$anomalia_consultas)
                      }
                      if(length(input$anomalia_parto) == 1){
                        dadoi <- subset(dadoi, cat_parto %in% input$anomalia_parto)
                      }
                      if(length(input$anomalia_gestacao) != 6){
                        dadoi <- subset(dadoi, cat_gestacao %in% input$anomalia_gestacao)
                      }
                    }
                    
                    if(nrow(dadoi) == 0){
                      showModal(modalDialog(
                      title = NULL,
                      tagList(
                      p("Não há registros no período.")),
                      easyClose = TRUE,
                      footer = NULL
                      ))
                   NULL
                   }else{
                    dados_ac(dadoi)
                   }

                          }) #end observeEvent dados nv


 #========================================================================
  #cards
  
  #card total 
  mod_summary_card_server('anomalia_total', 
                            tagList(
                              div(class = 'card-stamp',
                              div(class = 'card-stamp-icon bg-blue',
                              HTML('<svg  xmlns="http://www.w3.org/2000/svg"  width="24"  height="24"  viewBox="0 0 24 24"  fill="none"  stroke="currentColor"  
stroke-width="2"  stroke-linecap="round"  stroke-linejoin="round"  class="icon icon-tabler icons-tabler-outline icon-tabler-hearts">
<path stroke="none" d="M0 0h24v24H0z" fill="none"/>
<path d="M14.017 18l-2.017 2l-7.5 -7.428a5 5 0 1 1 7.5 -6.566a5 5 0 0 1 8.153 5.784" />
<path d="M15.99 20l4.197 -4.223a2.81 2.81 0 0 0 0 -3.948a2.747 2.747 0 0 0 -3.91 -.007l-.28 .282l-.279
 -.283a2.747 2.747 0 0 0 -3.91 -.007a2.81 2.81 0 0 0 -.007 3.948l4.182 4.238z" />
</svg>')
                              )),
                              div(class = 'card-body',
                                h3(class = 'card-title', 'Total de NV´s com AC'),
                                 uiOutput('anomalia_total_card')
                              )
                            ) #end withTags
                          )
  
  output$anomalia_total_card <- renderUI({
                      dadoi <- dados_ac()
                      dadoi <- table(dadoi$idanomal)
                      tagList(
                      h1(dadoi[1]),
                      p(paste0('Com prevalência de ', round(dadoi[1]*100/sum(dadoi),2),'%'))
                      )
                                       #tagList(tags$div(class = 'text-center display-5 fw-bold my-3',dadoi)
                   })


 #card anomalias prioritárias
  mod_summary_card_server('anomalia_prioritaria', 
                            tagList(
                              div(class = 'card-stamp',
                              div(class = 'card-stamp-icon bg-blue',
                              HTML('<svg  xmlns="http://www.w3.org/2000/svg"  width="24"  height="24"  viewBox="0 0 24 24"  fill="none"  stroke="currentColor"  
stroke-width="2"  stroke-linecap="round"  stroke-linejoin="round"  
class="icon icon-tabler icons-tabler-outline icon-tabler-heart-plus">
<path stroke="none" d="M0 0h24v24H0z" fill="none"/><path d="M12 20l-7.5 -7.428a5 5 0 1 1 7.5 -6.566a5 5 0 1 1 7.96 6.053" />
<path d="M16 19h6" /><path d="M19 16v6" />
</svg>')
                              )),
                              div(class = 'card-body',
                                h3(class = 'card-title', 'Anomalias Congênitas Prioritárias'),
                                 uiOutput('anomalia_prioritaria_card')
                              )
                            ) #end withTags
                          )
  
  output$anomalia_prioritaria_card <- renderUI({
                      dadoi <- dados_ac()
                      tabela_acp <- filtro_acp()
                      dadoi <- subset(dadoi, idanomal == 1)
                      ac <- sum(dadoi$idanomal)
                      acp <- func_acp(dadoi$codanomal, tabela_acp[,1])
                      tagList(
                      h1(sum(acp)),
                      p(paste0('representando ', round(sum(acp)*100/ac,2),'% do total de NV´s com AC´s'))
                      )
                                       #tagList(tags$div(class = 'text-center display-5 fw-bold my-3',dadoi)
                   })


  #card anomalias letais
  mod_summary_card_server('anomalia_letal', 
                            tagList(
                              div(class = 'card-stamp',
                              div(class = 'card-stamp-icon bg-blue',
                              HTML('<svg  xmlns="http://www.w3.org/2000/svg"  width="24"  height="24"  viewBox="0 0 24 24"  fill="none"  stroke="currentColor"  
stroke-width="2"  stroke-linecap="round"  stroke-linejoin="round"  class="icon icon-tabler icons-tabler-outline icon-tabler-mood-sad">
<path stroke="none" d="M0 0h24v24H0z" fill="none"/>
<path d="M12 12m-9 0a9 9 0 1 0 18 0a9 9 0 1 0 -18 0" />
<path d="M9 10l.01 0" /><path d="M15 10l.01 0" /
><path d="M9.5 15.25a3.5 3.5 0 0 1 5 0" />
</svg>')
                              )),
                              div(class = 'card-body',
                                h3(class = 'card-title', 'Anomalias Letais ou Pot. Letal'),
                                 uiOutput('anomalia_letal_card')
                              )
                            ) #end withTags
                          )
  
  output$anomalia_letal_card <- renderUI({
                      dadoi <- dados_ac()
                      tabela_acp <- filtro_acp()
                      dadoi <- subset(dadoi, idanomal == 1)
                      ac <- sum(dadoi$idanomal)
                      ac_letal <- func_acp(dadoi$codanomal, lista_anomalia_letal[,1])
                      tagList(
                      h1(sum(ac_letal)),
                      p(paste0('representando ', round(sum(ac_letal)*100/ac,2),'% do total de NV´s com AC´s'))
                      )
                                       #tagList(tags$div(class = 'text-center display-5 fw-bold my-3',dadoi)
                   })



#============================================================================
  #mapa

  mod_summary_card_server('anomalia_mapa', 
                  tagList(
                   tags$div(class = 'card-body', style = 'display:flex;   justify-content:space-between;',
                    #tags$h1(class = 'card-title', 'Mapa'),
                    tabler_navtab_menu(
            tabler_navtab_menu_item("Mapa",tabName = "mapa2",
         selected = T),
            tabler_navtab_menu_item("Tabela",tabName = "tabela2",
         selected = F)
         )), #end div
                   tags$div(
                    tabler_tab_items(  
           tabler_tabtab_item(
              tabName = "mapa2", selected = TRUE,
                    leafletOutput('anomalia_mapa_leaflet') ),
            tabler_tab_item(tabName = 'tabela2', selected = F, 
                    reactableOutput('anomalia_tabmapa')
              )
                    )
                  )
                             )
  )
                               
  output$anomalia_mapa_leaflet <- renderLeaflet({ mapas  %>%
       addLayersControl(
            baseGroups = c("Municípios", 'Regionais'),
           # overlayGroups = 'Rede de nascimentos',
            options = layersControlOptions(collapsed = F), position = 'bottomleft' )  
        })
        
  #reactiveVal para elaboração de tabelas
  dados_ac_tabmapa <- reactiveVal()
  
  observe({
  req(!is.null(dados_ac()))
  req(input$anomalia_mapa_leaflet_groups)
   
   filtro <-filtro_acp()
   dadoi <- dados_ac()
   dadoi <- subset(dadoi, idanomal == 1)
   dadoi$ones <- 1
   dadoi$acp <- func_acp(dadoi$codanomal, filtro[,1])
   
   
   #dados
   if(input$anomalia_mapa_leaflet_groups == 'Municípios'){
    dados_mapa <- with(dadoi, as.data.frame(table(codmunres, acp), stringsAsFactors = F)) %>%
                  tidyr::spread(., value = Freq, key = acp)
    dados_mapa$Total <- apply(dados_mapa[,-1], 1, sum)
      if(ncol(dados_mapa) == 3 & any(names(dados_mapa) == 'TRUE')){
      names(dados_mapa)[2] <- c('ACP')}
      if(ncol(dados_mapa) == 3 & any(names(dados_mapa) == 'FALSE')){
      names(dados_mapa)[2] <- c('Outras anomalias')}
      if(ncol(dados_mapa) == 4){
      names(dados_mapa)[2:3] <- c('ACP','Outras anomalias')}
    #names(dados_mapa)[1] <- c('cod6'), 'Outras anomalias'
   }else{
    dados_mapa <- with(dadoi, as.data.frame(table(reg_saude.res, acp), stringsAsFactors = F)) %>%
                  tidyr::spread(., value = Freq, key = acp)
    if(ncol(dados_mapa) == 2 & any(names(dados_mapa) == 'TRUE')){
      names(dados_mapa)[2] <- c('ACP')
      dados_mapa$`Outras anomalias` <- 0
      dados_mapa$Total <- dados_mapa$ACP}
      if(ncol(dados_mapa) == 2 & any(names(dados_mapa) == 'FALSE')){
      names(dados_mapa)[2] <- c('Outras anomalias')
      dados_mapa$`ACP` <- 0
      dados_mapa$Total <- dados_mapa$`Outras anomalias`}
      if(ncol(dados_mapa) == 3){
      names(dados_mapa)[2:3] <- c('ACP','Outras anomalias')
      dados_mapa$Total <- apply(dados_mapa[,-1], 1, sum)
    }
   }

   dados_ac_tabmapa(dados_mapa) #guardando df para construção do mapa   
    
   if(input$anomalia_mapa_leaflet_groups == 'Municípios'){
    dados_mapa$codmunres <- as.numeric(dados_mapa$codmunres)
    dados_mapa <- dplyr::left_join(municipiosf, dados_mapa, by = c('cod6' = 'codmunres'))
    labellss <- sprintf(
  "<strong>%s</strong><br/> %s %s<br/> %s %s" , #  people / mi<sup>2</sup>",
   dados_mapa$Municipio, 'Total AC´s: ', dados_mapa$Total, 'ACP: ', dados_mapa$ACP) %>% lapply(htmltools::HTML)
   }else{
    dados_mapa <- dplyr::left_join(mapa_regionais, dados_mapa, by = c('reg_saude' = 'reg_saude.res'))
    labellss <- sprintf(
  "<strong>%s</strong><br/> %s %s<br/> %s %s" , #  people / mi<sup>2</sup>",
   dados_mapa$reg_saude, 'Total AC´s: ', dados_mapa$Total, 'ACP: ', dados_mapa$ACP) %>% lapply(htmltools::HTML)
   }

   
   fill_color <- function(x){
          bins <- unique(as.vector(round(quantile(x, probs = c(0,0.30,0.50,0.7,0.85,0.95,1),na.rm = T),2))) #
          if(all(bins) == 0){bins <- NA; x[x == 0] <- NA}
          if(length(bins) == 1){bins <- c(0, bins)}
          pal <- colorBin("YlOrRd", domain = x, bins = bins)
          colorData <- pal(x)
          list(pal, colorData)
        }

   texto <- 'Valor' #TODO colocar titulo legenda apropriadas

   leafletProxy('anomalia_mapa_leaflet') %>%
        #addProviderTiles("OpenStreetMap.Mapnik") %>% leaflet()
        addProviderTiles(providers$CartoDB.Positron,  options = providerTileOptions(minZoom = 7)) %>%
        setView(lat = -27.5, lng = -51, zoom = 7)  %>% clearControls() %>% clearShapes() %>%
        addPolygons(data = dados_mapa,  color = "#444444", fillColor =  fill_color(dados_mapa$Total)[[2]], 
        stroke = T, smoothFactor = 0.5, fillOpacity = 0.8, weight = 1.5,
    highlight = highlightOptions(
    weight = 5,
    color = "#666",
    fillOpacity = 0.7,
    bringToFront = TRUE),
  label = labellss,
  labelOptions = labelOptions(
    style = list("font-weight" = "normal", padding = "3px 8px"),
    textsize = "12px",
    maxWidth = '200px',
    direction = "auto")) %>% 

    
    addLegend(pal = fill_color(dados_mapa$Total)[[1]], values = fill_color(dados_mapa$Total)[[2]], opacity = 0.7,
     title = texto,
  position = "bottomright", layerId="colorLegend2",className = 'info legenda')   %>%
       addLayersControl(
            baseGroups = c('Municípios', 'Regionais'),
            #overlayGroups = 'Rede de nascimentos',
            options = layersControlOptions(collapsed = F), position = 'bottomleft' )  
                      
  }) #observwe 


  #tabela aba de mapas
  output$anomalia_tabmapa <- renderReactable(({
        dadoi <- dados_ac_tabmapa()
        if(names(dadoi)[1] == 'codmunres'){
          municipio <- as.data.frame(municipiosf[,c('cod6','Municipio')])[,-3]
          dadoi[,1] <- as.numeric(dadoi[,1])
          dadoi <- dplyr::left_join(municipio,dadoi,  by = c('cod6' = 'codmunres'))
          dadoi <- dadoi[,-1]

        }

   
  reactable(dadoi, pagination = F, height = '320px')

  }))

 #------------------------------------------------------------------------------
 #tabelas
 
 #frequência ACP
 mod_summary_card_server('anomalia_tabela_acp',
    card_large(heading = ' Frequência de anomalia congênita por grupo prioritário (ACP)',
      tagList(
        fluidRow(
          column(2,
            selectInput('anomalia_tabinput1', 'Coluna', 
            choices = c('Ano' = 'ano', 'Região da saúde (res)' = 'reg_saude.res', '1º pré-natal' = 'cat_prenatal', 'Peso ao nascer' = 'cat_peso', 
            'Idade Gestacional' = 'cat_gestacao', 'Qtde. consultas' = 'cat_consultas', 'Apgar5' = 'cat_apgar5',
            'Tipo de parto' = 'cat_parto', 'Idade da parturiente' = 'cat_idademae'), selected = 'Ano')
           
            )
          ,
          column(10,
            reactableOutput('anomalia_tabela_dinamica_acp'))          
        ),
        tags$button("Baixar como CSV", onclick = "Reactable.downloadDataCSV('anomalia_tabdinamica_acp', 'tabela_dinamica_acp.csv')")
      )
    )
  )


  output$anomalia_tabela_dinamica_acp <- renderReactable({
        dadoi <- dados_ac()
        tabela_acp <- filtro_acp()
        dadoi <- subset(dadoi, idanomal == 1)

        tabela <- purrr::map_df(split(dadoi, dadoi[,input$anomalia_tabinput1]), function(x){ #)
        if(nrow(x) == 0){NULL}else{
          vetor <- stringr::str_extract_all(x$codanomal, "\\w{4}") %>% unlist
          vetor <- as.data.frame(table(vetor), stringAsFactors = F)
          vetor <- dplyr::left_join(vetor, tabela_acp, by = c('vetor' = 'cid10'))
          vetor <- aggregate(Freq ~ grupo_anomalia_congenitas_prioritarias, data = vetor, FUN = sum)
          vetor}}, .id = 'variavel') %>%
                 tidyr::spread(.,value = Freq, key = variavel )

        if(ncol(tabela) > 2){
        tabela$Total <- apply(tabela[,-1], 1, sum, na.rm = T)}else{
          tabela$Total <- tabela[,2]
        }
        linha_total <-  c(NA, apply(tabela[,-1], 2 , sum, na.rm = T))
        tabela <- rbind(tabela, linha_total)        

        reactable(tabela, pagination = F, height = '100%', elementId = 'anomalia_tabdinamica_acp')

  })
 
  
  #Tabela anomalias letais
  mod_summary_card_server('anomalia_tabela_letal',
    card_large(heading = 'Frequência e proporção de anomalias congênitas identificadas em nascidos vivos',
      tagList(
        fluidRow(
          column(2,
            selectInput('anomalia_letal_tabinput1', 'Linha', 
            choices = c('Ano' = 'ano', 'Região da saúde (res)' = 'reg_saude.res', '1º pré-natal' = 'cat_prenatal', 'Peso ao nascer' = 'cat_peso', 
            'Idade Gestacional' = 'cat_gestacao', 'Qtde. consultas' = 'cat_consultas', 'Apgar5' = 'cat_apgar5',
            'Tipo de parto' = 'cat_parto', 'Idade da parturiente' = 'cat_idademae'), selected = 'Ano')
           
            )
          ,
          column(10,
            reactableOutput('anomalia_tabela_dinamica_letal'))          
        ),
        tags$button("Baixar como CSV", onclick = "Reactable.downloadDataCSV('anomalia_tabdinamica_letal', 'tabela_dinamica_letal.csv')")
      )
    )
  )


  output$anomalia_tabela_dinamica_letal <- renderReactable({
        dadoi <- dados_ac()
        dadoi <- subset(dadoi, idanomal == 1)
        tabela_acp <- filtro_acp()
        
        tabela <- purrr::map_df(split(dadoi, dadoi[,input$anomalia_letal_tabinput1]), function(x){ #)
        if(nrow(x) == 0){NULL}else{
          vetor <- stringr::str_extract_all(x$codanomal, "\\w{4}") %>% unlist
          vetor <- as.data.frame(table(vetor), stringAsFactors = F)
          vetor <- dplyr::left_join(vetor, tabela_acp, by = c('vetor' = 'cid10'))
          vetor$letal <- ifelse(vetor$vetor %in% lista_anomalia_letal[,1],'Letal ou Pot.letal', 'Não letal') %>%
                          factor(., levels = c('Letal ou Pot.letal', 'Não letal'))
          vetor <- aggregate(Freq ~ letal, data = vetor, FUN = sum)
          vetor}}, .id = 'variavel') %>%
                 tidyr::spread(.,value = Freq, key = letal )

        tabela$Total <- apply(tabela[,-1], 1, sum, na.rm = T)
        linha_total <-  c(NA, apply(tabela[,-1], 2 , sum, na.rm = T))
        tabela <- rbind(tabela, linha_total)
        tabela$`Letal ou Pot.letal (perc)` <- round(tabela[,2]*100/tabela$Total,2)
        tabela$`Não letal (perc)` <- round(tabela[,3]*100/tabela$Total,2)
        

        reactable(tabela[,c(1,2,5,3,6,4)], pagination = F, height = '100%', elementId = 'anomalia_tabdinamica_letal')

  })
