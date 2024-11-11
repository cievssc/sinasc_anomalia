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


 #card pre natal 
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
  
  output$anomalia_prrioritaria_card <- renderUI({
                      dadoi <- dados_ac()
                      tabela_acp <- filtro_acp()
                      dadoi <- subset(dadoi, idanomal == 1)
                      ac <- sum(dadoi$idanomal)
                      acp <- func_acp(dadoi$codanomal, tabela_acp[,1])
                      tagList(
                      h1(sum(acp)),
                      p(paste0('representando ', round(sum(acp)*100/ac,2),'% do total de AC´s)'))
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
  #req(input$anomalia_tipomapa)
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

   dados_tabmapa(dados_mapa) #guardando df para construção do mapa   
    
   if(input$anomalia_mapa_leaflet_groups == 'Municípios'){
    dados_mapa$codmunres <- as.numeric(dados_mapa$codmunres)
    dados_mapa <- dplyr::left_join(municipiosf, dados_mapa, by = c('cod6' = 'codmunres'))
    labellss <- sprintf(
  "<strong>%s</strong><br/> %s %s<br/> %s %s" , #  people / mi<sup>2</sup>",
   dados_mapa$Municipio, 'Total AC´s: ', dados_mapa$Freq, 'ACP: ', dados_mapa$ACP) %>% lapply(htmltools::HTML)
   }else{
    dados_mapa <- dplyr::left_join(mapa_regionais, dados_mapa, by = c('reg_saude' = 'reg_saude.res'))
    labellss <- sprintf(
  "<strong>%s</strong><br/> %s %s<br/> %s %s" , #  people / mi<sup>2</sup>",
   dados_mapa$reg_saude, 'Total AC´s: ', dados_mapa$Freq, 'ACP: ', dados_mapa$ACP) %>% lapply(htmltools::HTML)
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
        addPolygons(data = dados_mapa,  color = "#444444", fillColor =  fill_color(dados_mapa$Freq)[[2]], 
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

    
    addLegend(pal = fill_color(dados_mapa$Freq)[[1]], values = fill_color(dados_mapa$Freq)[[2]], opacity = 0.7,
     title = texto,
  position = "bottomright", layerId="colorLegend2",className = 'info legenda')   %>%
       addLayersControl(
            baseGroups = c('Municípios', 'Regionais'),
            #overlayGroups = 'Rede de nascimentos',
            options = layersControlOptions(collapsed = F), position = 'bottomleft' )  
                      
  }) #observwe 


  #tabela aba de mapas
  output$anomalia_tabmapa <- renderReactable(({
        dadoi <- dados_tabmapa()
        if(names(dadoi)[1] == 'codmunres'){
          municipio <- as.data.frame(municipiosf[,c('cod6','Municipio')])[,-3]
          dadoi[,1] <- as.numeric(dadoi[,1])
          dadoi <- dplyr::left_join(municipio,dadoi,  by = c('cod6' = 'codmunres'))
          dadoi <- dadoi[,-1]

        }

   
  reactable(dadoi, pagination = F, height = '320px')

  }))


 
 #=============================================================================
   #gráficos

 mod_summary_card_server('anomalia_timeserie',

      tags$div(class =  'card-tabs' ,
                   tabler_navbar_card(
                    tabler_navbar_menu_card('Série NV', 'anomalia_serienv', selected = T),
                    tabler_navbar_menu_card('Série Pré-natal', 'anomalia_serieprenatal'),
                    tabler_navbar_menu_card('Série partos cesarianos', 'anomalia_seriecesario')) ,

                     tabler_tabcard_items(
                      tabler_tabcard_item('anomalia_serienv', selected = T,
                        apexchartOutput('anomalia_serienv_chart', height = '380px')
                      )
                      ,
                      tabler_tabcard_item('anomalia_serieprenatal',selected = F,
                        apexchartOutput('anomalia_serieprenatal_chart', height = '380px')
                      )
                      ,
                      tabler_tabcard_item('anomalia_seriecesario', selected = F,
                        apexchartOutput('anomalia_seriecesario_chart', height = '380px')
                      )
                     )
      ) #end card
  )

  #dado em comum para as séries

  dado_serie <- reactive({
            dadoi <- dados_ac()
            #nascidos vivos
            dadoi_nv <- with(dadoi, as.data.frame(table(mes, ano), stringAsFactors = F))
            dadoi_nv$mes_ano <- paste0(dadoi_nv$mes,'-', dadoi_nv$ano)
            #pre natal
            dadoi_prenat <- with(dadoi, as.data.frame(table(ano, mes, cat_prenatal))) %>%
                            tidyr::spread(., value = Freq, key= cat_prenatal)
            dadoi_prenat$perc <- dadoi_prenat[,3]*100/apply(dadoi_prenat[,3:4], 1, sum, na.rm = T)
            dadoi_prenat$perc <- round(dadoi_prenat$perc,2)
            dadoi_prenat$mes_ano <- paste0(dadoi_prenat$mes,'-', dadoi_prenat$ano)
            #cesario
            dadoi_cesar <- with(dadoi, as.data.frame(table(ano, mes, cat_parto))) %>%
                            tidyr::spread(., value = Freq, key= cat_parto)
            dadoi_cesar$perc <- dadoi_cesar[,4]*100/apply(dadoi_cesar[,3:4], 1, sum, na.rm = T)
            dadoi_cesar$perc <- round(dadoi_cesar$perc,2)
            dadoi_cesar$mes_ano <- paste0(dadoi_cesar$mes,'-', dadoi_cesar$ano)
            
            list(dadoi_nv, dadoi_prenat, dadoi_cesar)
            })

 #serie nascidos vivos
 output$anomalia_serienv_chart <- renderApex({
             validate(
      need(nrow(dado_serie()[[1]]) > 1 , 'Sem dados ou meses suficiente para constituir série')
    )
            dadoi <- dado_serie()[[1]]
         
                       list(series = list(list(name = 'Nascidos vivos',
                                                data = dadoi$Freq)),

                                              chart = list(type = 'area', 
                                                       height = 380,
                                                       toolbar = list(show = TRUE)
                                                       ),
                                              xaxis = list(
                                                      categories = c(dadoi$mes_ano)
                                                      ),
                                              
                                              legend = c(show = F)
                                              )

  })

  #serie pre natal
 output$anomalia_serieprenatal_chart <- renderApex({
             validate(
      need(nrow(dado_serie()[[2]]) > 1 , 'Sem dados ou meses suficiente para constituir série')
    )
            dadoi <- dado_serie()[[2]]
         
                       list(series = list(list(name = 'Percentual',
                                                data = dadoi$perc)),
                                              chart = list(type = 'line', 
                                                       height = 380,
                                                       toolbar = list(show = TRUE)
                                                       ),
                                                     
                                              stroke = list(width = 2),
                                              xaxis = list(
                                                      categories = c(dadoi$mes_ano)
                                                      ),
                                              title = list(
                                                      text = '% Pré-natal até o 3º mês',
                                                      align = 'left'),
                                              dataLabels = list(enabled = TRUE),       
                                              
                                              legend = c(show = F)
                                              )

  })


   #serie cesarios
 output$anomalia_seriecesario_chart <- renderApex({
             validate(
      need(nrow(dado_serie()[[3]]) > 1 , 'Sem dados ou meses suficiente para constituir série')
    )
            dadoi <- dado_serie()[[3]]
         
                       list(series = list(list(name = 'Percentual',
                                                data = dadoi$perc)),
                                              chart = list(type = 'line', 
                                                       height = 380,
                                                       toolbar = list(show = TRUE)
                                                       ),
                                                     
                                              stroke = list(width = 2),
                                              xaxis = list(
                                                      categories = c(dadoi$mes_ano)
                                                      ),
                                              title = list(
                                                      text = '% Partos cesarianos',
                                                      align = 'left'),
                                              dataLabels = list(enabled = TRUE),       
                                              
                                              legend = c(show = F)
                                              )

  })


  mod_summary_card_server('anomalia_peso',
    tagList(
      div(class = 'card-header',
        h1(class = 'card-title', 'Peso ao nascer')),
      div(class = 'card-body',
      uiOutput('anomalia_peso_texto'),
        apexchartOutput('anomalia_graf_peso', height = '250px'))
    )
  )

  text_peso <- reactiveVal()
  output$anomalia_peso_texto <- renderUI({
    texto <- text_peso()
    tagList(
      div(class = 'subheader', texto)
    )
  })

  output$anomalia_graf_peso <- renderApex({
      dadoi <- dados_ac()
      dadoi <- as.data.frame(table(dadoi$cat_peso))
      if(dadoi[4,2] == 0){
        texto <- NULL
      }else{
        perc <- round(sum(dadoi[-4,2]*100)/sum(dadoi[,2]),2)
        texto <- paste0('% Total de baixo peso: ', perc,'%')
      }
      text_peso(texto)
      

      list(series = c(dadoi$Freq[-4]),
                            chart = list(type = 'donut',
                                         height = '100%'),
                            labels = levels(dadoi$Var1)[-4],
                            #plotOptions = list(
                            #  pie = list(
                            #    donut = list(
                            #        labels = list(
                            #          show = TRUE,
                            #          name = list(
                            #            show = TRUE,
                            #            label = texto[[1]])
                            #        )
                            #    )
                            #  )
                            #),
                            legend = list(position = 'bottom'))

  }) #end renderapex

   mod_summary_card_server('anomalia_gestacao',
    tagList(
      div(class = 'card-header',
        h1(class = 'card-title', 'Idade gestacional')),
      div(class = 'card-body',
        apexchartOutput('anomalia_graf_gestacao', height = '280px'))
    )
  )

output$anomalia_graf_gestacao <- renderApex({
                          dadoi <- dados_ac()
                          
                          dadoi <- with(dadoi, as.data.frame(table(cat_gestacao)))
                         
                           list(series = list(list(data = dadoi[,2],
                                                    name = 'Quantidade')
                                              ),
                                              chart = list(type = 'bar', 
                                                       #toolbar = c(show = FALSE),
                                                       height = '100%',
                                                       zoom = list(type = 'x',enabled = TRUE),
                                                       toolbar = list(autoSelected = 'zoom')),
                                              #colors = c('#008FFB', '#FF4560'),
                                              dataLabels = c(enabled = FALSE),
                                              plotOptions = list(bar = list(horizontal = T,
                                                                       barheight = '80%')),
                                              xaxis = list(#labels = c(show = FALSE),
                                                      categories =dadoi[,1]
                                                      ),
                                              grid = list(
                                                          xaxis = list(lines = c(show = FALSE))),
                                              legend = c(show = FALSE)
                                              )
                                     })  #end renderapex  


 mod_summary_card_server('anomalia_consulta',
    tagList(
      div(class = 'card-header',
        h2(class = 'card-title', 'Quantidade de pré-natais / Realização da 1ª consulta')),
      div(class = 'card-body',
        apexchartOutput('anomalia_graf_consulta', height = '280px'))
    )
  )

 output$anomalia_graf_consulta <- renderApex({
                          dadoi <- dados_ac()
                          
                          dadoi <- with(dadoi, as.data.frame(table(cat_consultas, cat_prenatal))) %>%
                                   tidyr::spread(., key = cat_prenatal, value = Freq)
                         
                           list(series = list(list(data = dadoi[,2],
                                                    name = names(dadoi)[2]),
                                              list(data = dadoi[,3],
                                                    name = names(dadoi)[3])  
                                              ),
                                              chart = list(type = 'bar', 
                                                       #toolbar = c(show = FALSE),
                                                       height = '100%',
                                                       zoom = list(type = 'x',enabled = TRUE),
                                                       toolbar = list(autoSelected = 'zoom'),
                                                       stacked = TRUE),
                                              #colors = c('#008FFB', '#FF4560'),
                                              dataLabels = c(enabled = FALSE, #ficou pequeno demais pelo tamanho do gráfico no layout
                                                            total = list(enabled = TRUE,
                                                            offsetX = 0,
                                                            style = list(fontSize = '13px'))),
                                              plotOptions = list(bar = list(horizontal = T,
                                                                       barheight = '90%')),
                                              xaxis = list(#labels = c(show = FALSE),
                                                      categories =dadoi[,1]
                                                      ),
                                              grid = list(
                                                          xaxis = list(lines = c(show = FALSE))),
                                              legend = c(show = TRUE)
                                              )
                                     })  #end renderapex 


 #------------------------------------------------------------------------
 #tabelas
 mod_summary_card_server('anomalia_tabela',
    card_large(heading = 'Tabela dinâmica',
      tagList(
        fluidRow(
          column(2,
            selectInput('anomalia_tabinput1', 'Linha', 
            choices = c('Ano' = 'ano', 'Região da saúde (res)' = 'reg_saude.res', '1º pré-natal' = 'cat_prenatal', 'Peso ao nascer' = 'cat_peso', 
            'Idade Gestacional' = 'cat_gestacao', 'Qtde. consultas' = 'cat_consultas', 'Apgar5' = 'cat_apgar5',
            'Tipo de parto' = 'cat_parto'), selected = 'Ano')
            ,
            selectInput('anomalia_tabinput2', 'Coluna', 
            choices = c('1º pré-natal' = 'cat_prenatal', 'Peso ao nascer' = 'cat_peso', 
            'Idade Gestacional' = 'cat_gestacao', 'Qtde. consultas' = 'cat_consultas', 'Apgar5' = 'cat_apgar5',
            'Tipo de parto' = 'cat_parto'), selected = 'Peso ao nascer')
            )
          ,
          column(10,
            reactableOutput('anomalia_tabela_dinamica'))          
        ),
        tags$button("Baixar como CSV", onclick = "Reactable.downloadDataCSV('anomalia_tabdinamica', 'tabela_dinamica.csv')")
      )
    )
  )


  output$anomalia_tabela_dinamica <- renderReactable({
        dadoi <- dados_ac()
        tab1 <- with(dadoi, table(get(input$anomalia_tabinput1), get(input$anomalia_tabinput2)))
        tab2 <- round(prop.table(tab1,1)*100,2) %>% as.data.frame
        tab2 <- tidyr::spread(tab2, key = Var2, value = Freq)
        tab1 <- as.data.frame(tab1) %>%
                tidyr::spread(., key = Var2, value = Freq)
        tab <- dplyr::left_join(tab1, tab2, by = 'Var1', suffix = c('',' (%)'))
        names(tab)[1] <- 'Linha'

        reactable(tab, pagination = F, height = '100%', elementId = 'anomalia_tabdinamica')

  })


  mod_summary_card_server('anomalia_tabela_gar',
    card_large(heading = 'Número de nascidos vivos e estimativa de gestação de alto risco, por região de saúde.',
            tagList(
                   tabler_navtab_menu(
              tabler_navtab_menu_item("Tabela",tabName = "tabelagar",
              selected = T)#, #sem uso (06-nov-24, 16:25h)
              #tabler_navtab_menu_item("Mapa/Rede",tabName = "mapagar",
               #selected = F)
              ),
                   tags$div(
                    tabler_tab_items(  
                    tabler_tabtab_item(
              tabName = "tabelagar", selected = TRUE,
                  tagList(
                    fluidRow(
                    column(12,style = 'float:right;',
                    tags$h5(style = 'float:right;','Região de Residência'))),
                   fluidRow(
                      column(1,
                      tags$div(style = 'vertical-align:middle;',
                      h5('Região de ocorrência'))),
                      column(11,
                      reactableOutput('anomalia_tabelagar_out'))
                    ),
        tags$button("Baixar como CSV", onclick = "Reactable.downloadDataCSV('anomalia_tabgar', 'tabela_gar.csv')"))
                    )
                    )
                  )
                    )      
    )
  )


  output$anomalia_tabelagar_out <- renderReactable({
        dadoi <- dados_ac()
        dadoi <- with(dadoi,(table(reg_saude.nasc, reg_saude.res)))# %>% addmargins(.,1)
        Total <- apply(dadoi, 2, sum, na.rm = T)
        GAR <- round(Total * .15)
        dadoi<- rbind(dadoi,Total, GAR) %>% 
                 as.data.frame# %>% tidyr::spread(., value = Freq, key = Var2)

        #dadoi <- with(dadoi,(table(reg_saude.nasc, reg_saude.res))) %>% addmargins(.,1) %>% 
         #        as.data.frame %>% tidyr::spread(., value = Freq, key = reg_saude.res)
        #names[1] <- 'Região de ocorrência'

        reactable(dadoi, pagination = F, rownames = T, height = '550px', elementId = 'anomalia_tabgar')

  })
