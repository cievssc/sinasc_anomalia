  #server para a aba de nascidos vivos
  #----------------------------------------------------------------------

  #opções botões dropdown (TODO, 15-jul-24)
   output$nascido_dropopcoes <- renderUI({
                               tagList(
                                div(style = 'z-index: 50;',
                                selectInput('nascido_regiao', 'Região: ', choices = c('Todas', unique(tab_regioes$reg_saude )), 
                                  selected = c('Todas'), multiple = F),
                                selectInput('nascido_peso', 'Peso:', choices = levels(dados$cat_peso), 
                                  selected = levels(dados$cat_peso), multiple = T),
                                selectInput('nascido_prenatal', 'Primeiro pré-natal', choices = levels(dados$cat_prenatal),
                                  selected = levels(dados$cat_prenatal), multiple = T),
                                selectInput('nascido_gestacao', 'Período de gestação', choices = levels(dados$cat_gestacao)[-7],
                                  selected = levels(dados$cat_gestacao)[-7], multiple = T),
                                selectInput('nascido_consultas', 'Consultas realizadas', choices = levels(dados$cat_consultas)[-5],
                                  selected =  levels(dados$cat_consultas)[-5], multiple = T),
                                selectInput('nascido_parto','Tipo de parto', choices = levels(dados$cat_parto)[-3],
                                  selected = levels(dados$cat_parto)[-3], multiple = T),
                                selectInput('nascido_apgar5', 'APGAR5', choices = levels(dados$cat_apgar5), 
                                  selected = levels(dados$cat_apgar5), multiple = T),
                                selectInput('nascido_meses', 'Meses do ano', choices = c(1:12), selected = c(1:12), multiple = T)
                                

                                )#         
                                        ) #end tag list
   })
  
 
  #organizando os dados
  
  dados_all <- eventReactive(input$head_atualizar, {
               subset(dados, ano >= input$head_daterange[1] & ano <= input$head_daterange[2])
  }, ignoreInit = T) #dadoscomum ás duas abas

  dados_nv <- reactiveVal(dados[which(dados$ano <= max(dados$ano) & dados$ano >= max(dados$ano) - 3),])
  

            observeEvent(dados_all(),{# input$home_atualizar,
             #req(input$nascido_mapa_leaflet_groups)
                   dadoi <- dados_all()
                   if(input$nascido_dropdown >0){
                      dadoi <- subset(dadoi, mes %in% input$nascido_meses) #meses
                      
                      if(input$nascido_regiao != 'Todas'){
                        dadoi <- subset(dadoi, reg_saude.res == input$nascido_regiao)
                      }
                      if(length(input$nascido_apgar5) == 1){
                        dadoi <- subset(dadoi, cat_apgar5 == input$nascido_apgar5)
                      }
                      if(length(input$nascido_peso) != 5){
                        dadoi <- subset(dadoi, cat_peso %in% input$nascido_peso)
                      }
                      if(length(input$nascido_prenatal) == 1){
                        dadoi <- subset(dadoi, cat_prenatal == input$nascido_prenatal)
                      }
                      if(length(input$nascido_consultas) != 4){
                        dadoi <- subset(dadoi, cat_consultas %in% input$nascido_consultas)
                      }
                      if(length(input$nascido_parto) == 1){
                        dadoi <- subset(dadoi, cat_parto %in% input$nascido_parto)
                      }
                      if(length(input$nascido_gestacao) != 6){
                        dadoi <- subset(dadoi, cat_gestacao %in% input$nascido_gestacao)
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
                    dados_nv(dadoi)
                   }

                          }) #end observeEvent dados nv


 #========================================================================
  #cards
  
  #card total 
  mod_summary_card_server('nascido_total', 
                            tagList(
                              div(class = 'card-stamp',
                              div(class = 'card-stamp-icon bg-blue',
                              HTML('<svg xmlns="http://www.w3.org/2000/svg" class="icon icon-tabler icon-tabler-align-box-center-middle" width="72" 
                              height="72" viewBox="0 0 24 24"  fill="none"  stroke="currentColor"  stroke-width="2"  stroke-linecap="round"  stroke-linejoin="round"
  class="icon icon-tabler icons-tabler-outline icon-tabler-baby-carriage">
<path stroke="none" d="M0 0h24v24H0z" fill="none"/>
<path d="M8 19m-2 0a2 2 0 1 0 4 0a2 2 0 1 0 -4 0" />
<path d="M18 19m-2 0a2 2 0 1 0 4 0a2 2 0 1 0 -4 0" />
<path d="M2 5h2.5l1.632 4.897a6 6 0 0 0 5.693 4.103h2.675a5.5 5.5 0 0 0 0 -11h-.5v6" />
<path d="M6 9h14" /><path d="M9 17l1 -3" /><path d="M16 14l1 3" />
</svg>')
                              )),
                              div(class = 'card-body',
                                h3(class = 'card-title', 'Total de nascidos vivos'),
                                 uiOutput('nascido_total_card')
                              )
                            ) #end withTags
                          )
  
  output$nascido_total_card <- renderUI({
                      dadoi <- nrow(dados_nv())
                      tagList(
                      h1(dadoi)#,
                      #p('Nascidos vivos no estado')
                      )
                                       #tagList(tags$div(class = 'text-center display-5 fw-bold my-3',dadoi)
                   })

   #card total 
  mod_summary_card_server('nascido_total', 
                            tagList(
                              div(class = 'card-stamp',
                              div(class = 'card-stamp-icon bg-blue',
                              HTML('<svg xmlns="http://www.w3.org/2000/svg" class="icon icon-tabler icon-tabler-align-box-center-middle" width="72" 
                              height="72" viewBox="0 0 24 24"  fill="none"  stroke="currentColor"  stroke-width="2"  stroke-linecap="round"  stroke-linejoin="round"
  class="icon icon-tabler icons-tabler-outline icon-tabler-baby-carriage">
<path stroke="none" d="M0 0h24v24H0z" fill="none"/>
<path d="M8 19m-2 0a2 2 0 1 0 4 0a2 2 0 1 0 -4 0" />
<path d="M18 19m-2 0a2 2 0 1 0 4 0a2 2 0 1 0 -4 0" />
<path d="M2 5h2.5l1.632 4.897a6 6 0 0 0 5.693 4.103h2.675a5.5 5.5 0 0 0 0 -11h-.5v6" />
<path d="M6 9h14" /><path d="M9 17l1 -3" /><path d="M16 14l1 3" />
</svg>')
                              )),
                              div(class = 'card-body',
                                h3(class = 'card-title', 'Total de nascidos vivos'),
                                 uiOutput('nascido_total_card')
                              )
                            ) #end withTags
                          )
  
  output$nascido_total_card <- renderUI({
                      dadoi <- nrow(dados_nv())
                      tagList(
                      h1(dadoi)#,
                      #p('Nascidos vivos no estado')
                      )
                                       #tagList(tags$div(class = 'text-center display-5 fw-bold my-3',dadoi)
                   })


 #card pre natal 
  mod_summary_card_server('nascido_total_prenatal', 
                            tagList(
                              div(class = 'card-stamp',
                              div(class = 'card-stamp-icon bg-blue',
                              HTML('<svg xmlns="http://www.w3.org/2000/svg" class="icon icon-tabler icon-tabler-align-box-center-middle" width="72" 
                              height="72" viewBox="0 0 24 24"  fill="none"  stroke="currentColor"  stroke-width="2"  stroke-linecap="round"  stroke-linejoin="round"  
class="icon icon-tabler icons-tabler-outline icon-tabler-empathize">
<path stroke="none" d="M0 0h24v24H0z" fill="none"/>
<path d="M12 5.5m-2.5 0a2.5 2.5 0 1 0 5 0a2.5 2.5 0 1 0 -5 0" />
<path d="M12 21.368l5.095 -5.096a3.088 3.088 0 1 0 -4.367 -4.367l-.728 .727l-.728 -.727a3.088 3.088 0 1 0 -4.367 4.367l5.095 5.096z" />
</svg>')
                              )),
                              div(class = 'card-body',
                                h3(class = 'card-title', 'Pré-natal'),
                                 uiOutput('nascido_prenatal_card')
                              )
                            ) #end withTags
                          )
  
  output$nascido_prenatal_card <- renderUI({
                      dadoi <- dados_nv()
                      dadoi <- list(table(dadoi$cat_prenat)[1], table(dadoi$cat_prenat)[1]/sum(table(dadoi$cat_prenat)))
                      tagList(
                      h1(dadoi[[1]]),
                      p(paste0('realizados até o 3º mês (', round(dadoi[[2]]*100,2),'% do total)'))
                      )
                                       #tagList(tags$div(class = 'text-center display-5 fw-bold my-3',dadoi)
                   })


  #card parto 
  mod_summary_card_server('nascido_total_parto', 
                            tagList(
                              div(class = 'card-stamp',
                              div(class = 'card-stamp-icon bg-blue',
                              HTML('<svg xmlns="http://www.w3.org/2000/svg" class="icon icon-tabler icon-tabler-align-box-center-middle" width="72" 
                              height="72"viewBox="0 0 24 24"  fill="none"  stroke="currentColor"  stroke-width="2"  stroke-linecap="round"  stroke-linejoin="round"  
class="icon icon-tabler icons-tabler-outline icon-tabler-accessible"><path stroke="none" d="M0 0h24v24H0z" fill="none"/>
<path d="M12 12m-9 0a9 9 0 1 0 18 0a9 9 0 1 0 -18 0" />
<path d="M10 16.5l2 -3l2 3m-2 -3v-2l3 -1m-6 0l3 1" />
<circle cx="12" cy="7.5" r=".5" fill="currentColor" />
</svg>')
                              )),
                              div(class = 'card-body',
                                h3(class = 'card-title', 'Cesarianos'),
                                 uiOutput('nascido_cesario_card')
                              )
                            ) #end withTags
                          )
  
  output$nascido_cesario_card <- renderUI({
                      dadoi <- dados_nv()
                      dadoi <- list(table(dadoi$cat_parto)[2], table(dadoi$cat_parto)[2]/sum(table(dadoi$cat_parto)))
                      tagList(
                      h1(dadoi[[1]]),
                      p(paste0('procedimentos realizados (', round(dadoi[[2]]*100,2),'% do total)'))
                      )
                                       #tagList(tags$div(class = 'text-center display-5 fw-bold my-3',dadoi)
                   })



#============================================================================
  #mapa

  mapas <- leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron,  options = providerTileOptions(minZoom = 7)) %>%
        setView(lat = -27.5, lng = -51, zoom = 7) 
    
  mod_summary_card_server('nascido_mapa', 
                  tagList(
                   tags$div(class = 'card-body', style = 'display:flex;   justify-content:space-between;',
                    #tags$h1(class = 'card-title', 'Mapa'),
                    tabler_navtab_menu(
            tabler_navtab_menu_item("Mapa",tabName = "mapa1",
         selected = T),
            tabler_navtab_menu_item("Tabela",tabName = "tabela1",
         selected = F)
         ),
                    selectInput('nascido_tipomapa', label = NULL, choices = c('Quantidade NV´s' = 1, '% Pré-natal' = 2, '% Cesarianas' = 3))),
                   tags$div(
                    tabler_tab_items(  
           tabler_tabtab_item(
              tabName = "mapa1", selected = TRUE,
                    leafletOutput('nascido_mapa_leaflet') ),
            tabler_tab_item(tabName = 'tabela1', selected = F, 
                    reactableOutput('nascido_tabmapa')
              )
                    )
                  )
                             )
  )
                               
  output$nascido_mapa_leaflet <- renderLeaflet({ mapas  %>%
       addLayersControl(
            baseGroups = c("Municípios", 'Regionais'),
           # overlayGroups = 'Rede de nascimentos',
            options = layersControlOptions(collapsed = F), position = 'bottomleft' )  
        })
        
  #reactiveVal para elaboração de tabelas
  dados_tabmapa <- reactiveVal()
  
  observe({
  req(input$nascido_tipomapa)
  req(input$nascido_mapa_leaflet_groups)
   
   dadoi <- dados_nv()
   dadoi$ones <- 1

   vetor <- switch(input$nascido_tipomapa,
              '1' = 'ones',
              '2' = 'cat_prenatal',
              '3' = 'cat_parto')
   
   #dados
   if(input$nascido_mapa_leaflet_groups == 'Municípios'){
    dados_mapa <- with(dadoi, as.data.frame(table(codmunres, get(vetor)), stringsAsFactors = F)) %>%
                  tidyr::spread(., value = Freq, key = Var2)
   }else{
    dados_mapa <- with(dadoi, as.data.frame(table(reg_saude.res, get(vetor)), stringsAsFactors = F)) %>%
                  tidyr::spread(., value = Freq, key = Var2)
   }

   dados_tabmapa(dados_mapa) #guardando df para construção do mapa
   
   if(input$nascido_tipomapa == '1'){
    dados_mapa$Freq <- dados_mapa[,2]
   }
   if(input$nascido_tipomapa == '2'){
    dados_mapa$Freq <- round(dados_mapa[,3]*100/apply(dados_mapa[,-1], 1, sum),2)
   }
   if(input$nascido_tipomapa == '3'){
    dados_mapa$Freq <- round(dados_mapa[,2]*100/apply(dados_mapa[,-1], 1, sum),2)
   }
    
   if(input$nascido_mapa_leaflet_groups == 'Municípios'){
    dados_mapa$codmunres <- as.numeric(dados_mapa$codmunres)
    dados_mapa <- dplyr::left_join(municipiosf, dados_mapa[,c('codmunres', 'Freq')], by = c('cod6' = 'codmunres'))
    labellss <- sprintf(
  "<strong>%s</strong><br/> %s %s" , #  people / mi<sup>2</sup>",
   dados_mapa$Municipio, 'Valor: ', dados_mapa$Freq) %>% lapply(htmltools::HTML)
   }else{
    dados_mapa <- dplyr::left_join(mapa_regionais, dados_mapa[,c('reg_saude.res', 'Freq')], by = c('reg_saude' = 'reg_saude.res'))
    labellss <- sprintf(
  "<strong>%s</strong><br/> %s %s" , #  people / mi<sup>2</sup>",
   dados_mapa$reg_saude, 'Valor: ', dados_mapa$Freq) %>% lapply(htmltools::HTML)
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

   leafletProxy('nascido_mapa_leaflet') %>%
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
  output$nascido_tabmapa <- renderReactable(({
        dadoi <- dados_tabmapa()
        if(names(dadoi)[1] == 'codmunres'){
          municipio <- as.data.frame(municipiosf[,c('cod6','Municipio')])[,-3]
          dadoi[,1] <- as.numeric(dadoi[,1])
          dadoi <- dplyr::left_join(municipio,dadoi,  by = c('cod6' = 'codmunres'))
          dadoi <- dadoi[,-1]

        }


if(input$nascido_tipomapa == '1'){
    names(dadoi)[2] <- 'Quantidade'
   }
if(input$nascido_tipomapa == '2'){
   sapply(2:3, function(x){
    nome <- paste0('Perc.',names(dadoi)[x])
    dadoi[,nome] <<-  round(dadoi[,x]*100/apply(dadoi[,-1], 1, sum),2)
   }) 
   }
   if(input$nascido_tipomapa == '3'){
    sapply(2:4, function(x){
    nome <- paste0('Perc.',names(dadoi)[x])
    dadoi[,nome] <<-  round(dadoi[,x]*100/apply(dadoi[,-1], 1, sum),2)
   }) 
   }   
  reactable(dadoi, pagination = F, height = '320px')

  }))


 
 #=============================================================================
   #gráficos

 mod_summary_card_server('nascido_timeserie',

      tags$div(class =  'card-tabs' ,
                   tabler_navbar_card(
                    tabler_navbar_menu_card('Série NV', 'nascido_serienv', selected = T),
                    tabler_navbar_menu_card('Série Pré-natal', 'nascido_serieprenatal'),
                    tabler_navbar_menu_card('Série partos cesarianos', 'nascido_seriecesario')) ,

                     tabler_tabcard_items(
                      tabler_tabcard_item('nascido_serienv', selected = T,
                        apexchartOutput('nascido_serienv_chart', height = '380px')
                      )
                      ,
                      tabler_tabcard_item('nascido_serieprenatal',selected = F,
                        apexchartOutput('nascido_serieprenatal_chart', height = '380px')
                      )
                      ,
                      tabler_tabcard_item('nascido_seriecesario', selected = F,
                        apexchartOutput('nascido_seriecesario_chart', height = '380px')
                      )
                     )
      ) #end card
  )

  #dado em comum para as séries

  dado_serie <- reactive({
            dadoi <- dados_nv()
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
 output$nascido_serienv_chart <- renderApex({
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
 output$nascido_serieprenatal_chart <- renderApex({
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
 output$nascido_seriecesario_chart <- renderApex({
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


  mod_summary_card_server('nascido_peso',
    tagList(
      div(class = 'card-header',
        h1(class = 'card-title', 'Peso ao nascer')),
      div(class = 'card-body',
      uiOutput('nascido_peso_texto'),
        apexchartOutput('nascido_graf_peso', height = '250px'))
    )
  )

  text_peso <- reactiveVal()
  output$nascido_peso_texto <- renderUI({
    texto <- text_peso()
    tagList(
      div(class = 'subheader', texto)
    )
  })

  output$nascido_graf_peso <- renderApex({
      dadoi <- dados_nv()
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

   mod_summary_card_server('nascido_gestacao',
    tagList(
      div(class = 'card-header',
        h1(class = 'card-title', 'Idade gestacional')),
      div(class = 'card-body',
        apexchartOutput('nascido_graf_gestacao', height = '280px'))
    )
  )

output$nascido_graf_gestacao <- renderApex({
                          dadoi <- dados_nv()
                          
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


 mod_summary_card_server('nascido_consulta',
    tagList(
      div(class = 'card-header',
        h2(class = 'card-title', 'Quantidade de pré-natais / Realização da 1ª consulta')),
      div(class = 'card-body',
        apexchartOutput('nascido_graf_consulta', height = '280px'))
    )
  )

 output$nascido_graf_consulta <- renderApex({
                          dadoi <- dados_nv()
                          
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
 mod_summary_card_server('nascido_tabela',
    card_large(heading = 'Tabela dinâmica',
      tagList(
        fluidRow(
          column(2,
            selectInput('nascido_tabinput1', 'Linha', 
            choices = c('Ano' = 'ano', 'Região da saúde (res)' = 'reg_saude.res', '1º pré-natal' = 'cat_prenatal', 'Peso ao nascer' = 'cat_peso', 
            'Idade Gestacional' = 'cat_gestacao', 'Qtde. consultas' = 'cat_consultas', 'Apgar5' = 'cat_apgar5',
            'Tipo de parto' = 'cat_parto', 'Idade da parturiente' = 'cat_idademae'), selected = 'Ano')
            ,
            selectInput('nascido_tabinput2', 'Coluna', 
            choices = c('1º pré-natal' = 'cat_prenatal', 'Peso ao nascer' = 'cat_peso', 
            'Idade Gestacional' = 'cat_gestacao', 'Qtde. consultas' = 'cat_consultas', 'Apgar5' = 'cat_apgar5',
            'Tipo de parto' = 'cat_parto', 'Idade da parturiente' = 'cat_idademae'), selected = 'Peso ao nascer')
            )
          ,
          column(10,
            reactableOutput('nascido_tabela_dinamica'))          
        ),
        tags$button("Baixar como CSV", onclick = "Reactable.downloadDataCSV('nascido_tabdinamica', 'tabela_dinamica.csv')")
      )
    )
  )


  output$nascido_tabela_dinamica <- renderReactable({
        dadoi <- dados_nv()
        tab1 <- with(dadoi, table(get(input$nascido_tabinput1), get(input$nascido_tabinput2)))
        tab2 <- round(prop.table(tab1,1)*100,2) %>% as.data.frame
        tab2 <- tidyr::spread(tab2, key = Var2, value = Freq)
        tab1 <- as.data.frame(tab1) %>%
                tidyr::spread(., key = Var2, value = Freq)
        tab <- dplyr::left_join(tab1, tab2, by = 'Var1', suffix = c('',' (%)'))
        names(tab)[1] <- 'Linha'

        reactable(tab, pagination = F, height = '100%', elementId = 'nascido_tabdinamica')

  })


  mod_summary_card_server('nascido_tabela_gar',
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
                      reactableOutput('nascido_tabelagar_out'))
                    ),
        tags$button("Baixar como CSV", onclick = "Reactable.downloadDataCSV('nascido_tabgar', 'tabela_gar.csv')"))
                    )
                    )
                  )
                    )      
    )
  )


  output$nascido_tabelagar_out <- renderReactable({
        dadoi <- dados_nv()
        dadoi <- with(dadoi,(table(reg_saude.nasc, reg_saude.res)))# %>% addmargins(.,1)
        Total <- apply(dadoi, 2, sum, na.rm = T)
        GAR <- round(Total * .15)
        dadoi<- rbind(dadoi,Total, GAR) %>% 
                 as.data.frame# %>% tidyr::spread(., value = Freq, key = Var2)

        #dadoi <- with(dadoi,(table(reg_saude.nasc, reg_saude.res))) %>% addmargins(.,1) %>% 
         #        as.data.frame %>% tidyr::spread(., value = Freq, key = reg_saude.res)
        #names[1] <- 'Região de ocorrência'

        reactable(dadoi, pagination = F, rownames = T, height = '550px', elementId = 'nascido_tabgar')

  })
