  #server para a aba de hospital (24-out-24, 14:12h)
  #----------------------------------------------------------------------

  #opções botões dropdown (TODO, 15-jul-24)
   output$hospital_dropopcoes <- renderUI({
                               tagList(
                                div(style = 'z-index: 50;',
                                selectInput('hospital_regiao', 'Região: ', choices = c('Todas', unique(tab_regioes$reg_saude )), 
                                  selected = c('Todas'), multiple = F),
                                selectInput('hospital_peso', 'Peso:', choices = levels(dados$cat_peso), 
                                  selected = levels(dados$cat_peso), multiple = T),
                                selectInput('hospital_prenatal', 'Primeiro pré-natal', choices = levels(dados$cat_prenatal),
                                  selected = levels(dados$cat_prenatal), multiple = T),
                                selectInput('hospital_gestacao', 'Período de gestação', choices = levels(dados$cat_gestacao)[-7],
                                  selected = levels(dados$cat_gestacao)[-7], multiple = T),
                                selectInput('hospital_consultas', 'Consultas realizadas', choices = levels(dados$cat_consultas)[-5],
                                  selected =  levels(dados$cat_consultas)[-5], multiple = T),
                                selectInput('hospital_parto','Tipo de parto', choices = levels(dados$cat_parto)[-3],
                                  selected = levels(dados$cat_parto)[-3], multiple = T),
                                selectInput('hospital_apgar5', 'APGAR5', choices = levels(dados$cat_apgar5), 
                                  selected = levels(dados$cat_apgar5), multiple = T),
                                selectInput('hospital_meses', 'Meses do ano', choices = c(1:12), selected = c(1:12), multiple = T)
                                

                                )#         
                                        ) #end tag list
   })
  
 
  #organizando os dados
  
  dados_hosp <- reactiveVal(NULL)
  
            observeEvent(dados_all(),{# input$home_atualizar,
             #req(input$hospital_mapa_leaflet_groups)
             #TODO javascript para input das abas
             dadoi <- dados_all()
              if(input$hospital_dropdown >0){
                      dadoi <- subset(dadoi, mes %in% input$hospital_meses) #meses
                      
                      if(input$hospital_regiao != 'Todas'){
                        dadoi <- subset(dadoi, reg_saude.nasc == input$hospital_regiao)
                      }
                      if(length(input$hospital_apgar5) == 1){
                        dadoi <- subset(dadoi, cat_apgar5 == input$hospital_apgar5)
                      }
                      if(length(input$hospital_peso) != 5){
                        dadoi <- subset(dadoi, cat_peso %in% input$hospital_peso)
                      }
                      if(length(input$hospital_prenatal) == 1){
                        dadoi <- subset(dadoi, cat_prenatal == input$hospital_prenatal)
                      }
                      if(length(input$hospital_consultas) != 4){
                        dadoi <- subset(dadoi, cat_consultas %in% input$hospital_consultas)
                      }
                      if(length(input$hospital_parto) == 1){
                        dadoi <- subset(dadoi, cat_parto %in% input$hospital_parto)
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
                    dados_hosp(dadoi)
                   }

                          }) #end observeEvent dados hosp


 #========================================================================
  #cards
  
  #card total 
  mod_summary_card_server('hospital_total', 
                            tagList(
                              div(class = 'card-stamp',
                              div(class = 'card-stamp-icon bg-blue',
                              HTML('<svg  xmlns="http://www.w3.org/2000/svg"  width="24"  height="24"  viewBox="0 0 24 24"  fill="none"  stroke="currentColor"  
stroke-width="2"  stroke-linecap="round"  stroke-linejoin="round"  
class="icon icon-tabler icons-tabler-outline icon-tabler-hospital-circle">
<path stroke="none" d="M0 0h24v24H0z" fill="none"/>
<path d="M10 16v-8" /><path d="M3 12a9 9 0 1 0 18 0a9 9 0 0 0 -18 0" />
<path d="M14 16v-8" /><path d="M10 12h4" />
</svg>')
                              )),
                              div(class = 'card-body',
                                h3(class = 'card-title', 'Total de NV em Unidades de Saúde'),
                                 uiOutput('hospital_total_card')
                              )
                            ) #end withTags
                          )
  
  output$hospital_total_card <- renderUI({
                      dadoi <- dados_hosp()
                      hosp <- sum(!is.na(dadoi$codestab))
                      perc <- round(hosp*100/nrow(dadoi),2)
                      tagList(
                      h1(hosp),
                      p(paste0('representando ',perc,'% do total de NV'))
                      )
                                       #tagList(tags$div(class = 'text-center display-5 fw-bold my-3',dadoi)
                   })

   #card total 
  mod_summary_card_server('hospital_total_pub', 
                            tagList(
                              div(class = 'card-stamp',
                              div(class = 'card-stamp-icon bg-blue',
                              HTML('<svg  xmlns="http://www.w3.org/2000/svg"  width="24"  height="24"  viewBox="0 0 24 24"  fill="none"  
stroke="currentColor"  stroke-width="2"  stroke-linecap="round"  stroke-linejoin="round" 
 class="icon icon-tabler icons-tabler-outline icon-tabler-heart">
<path stroke="none" d="M0 0h24v24H0z" fill="none"/>
<path d="M19.5 12.572l-7.5 7.428l-7.5 -7.428a5 5 0 1 1 7.5 -6.566a5 5 0 1 1 7.5 6.572" />
</svg>')
                              )),
                              div(class = 'card-body',
                                h3(class = 'card-title', 'NV em estabelecimentos públicos'),
                                 uiOutput('hospital_totalpub_card')
                              )
                            ) #end withTags
                          )
  
  output$hospital_totalpub_card <- renderUI({
                      #dadoi <- nrow(dados_nv())
                      tagList(
                      #h1(dadoi)#,
                      p(paste0('representando ','x','% do total de NV'))
                      )
                                       #tagList(tags$div(class = 'text-center display-5 fw-bold my-3',dadoi)
                   })


 #card pre natal 
  mod_summary_card_server('hospital_total_priv', 
                            tagList(
                              div(class = 'card-stamp',
                              div(class = 'card-stamp-icon bg-blue',
                              HTML('<svg  xmlns="http://www.w3.org/2000/svg"  width="24"  height="24"  viewBox="0 0 24 24"  fill="none"  stroke="currentColor" 
 stroke-width="2"  stroke-linecap="round"  stroke-linejoin="round" 
 class="icon icon-tabler icons-tabler-outline icon-tabler-health-recognition">
<path stroke="none" d="M0 0h24v24H0z" fill="none"/><path d="M4 8v-2a2 2 0 0 1 2 -2h2" />
<path d="M4 16v2a2 2 0 0 0 2 2h2" />
<path d="M16 4h2a2 2 0 0 1 2 2v2" />
<path d="M16 20h2a2 2 0 0 0 2 -2v-2" />
<path d="M8.603 9.61a2.04 2.04 0 0 1 2.912 0l.485 .39l.5 -.396a2.035 2.035 0 0 1 2.897 .007a2.104 2.104 0 0 1 0 2.949l-3.397 3.44l-3.397 -3.44a2.104 2.104 0 0 1 0 -2.95z" />
</svg>')
                              )),
                              div(class = 'card-body',
                                h3(class = 'card-title', 'NV em estabelecimentos privados'),
                                 uiOutput('hospital_totalpriv_card')
                              )
                            ) #end withTags
                          )
  
  output$hospital_totalpriv_card <- renderUI({
                     
                      tagList(
                      #h1(dadoi[[1]]),
                      p(paste0('representando ','x','% do total de NV'))
                      )
                                       #tagList(tags$div(class = 'text-center display-5 fw-bold my-3',dadoi)
                   })


#============================================================================
  #mapa

    
  mod_summary_card_server('hospital_mapa', 
                  tagList(
                   tags$div(class = 'card-body', #style = 'display:flex;   justify-content:space-between;',
                    tags$h1(class = 'card-title', 'Mapa'),
                    leafletOutput('hospital_mapa_leaflet') )
                    )
                  )
                               
  output$hospital_mapa_leaflet <- renderLeaflet({ mapas  %>%
       addLayersControl(
            #baseGroups = c("Municípios", 'Regionais'),
            overlayGroups = 'Rede de nascimentos',
            options = layersControlOptions(collapsed = F), position = 'bottomleft' )  
        })
        
  
  observe({
   req(!is.null(dados_hosp()))
   dadoi <- dados_hosp()
   dadoi$ones <- 1

   #dado_hosp <- with(dadoi, as.data.frame(table(no_fantasia, nu_latitude, nu_longitude)))
   dado_hosp <- aggregate(ones ~ no_fantasia + nu_longitude + nu_latitude, data = dadoi, FUN = sum)
   dado_vag <- aggregate(ones ~ no_fantasia, data = dadoi[which(dadoi$cat_parto == 'Vaginal'),], FUN = sum)
   dado_hosp <- dplyr::left_join(dado_hosp, dado_vag, by = 'no_fantasia', suffix = c('', '_vag'))
   dado_hosp$perc <- with(dado_hosp, round(ones_vag*100/ones,2))
   dado_hosp[is.na(dado_hosp)] <- 0

 #redes
   dadoi <- dadoi[with(dadoi, codmunres > 420000 & codmunres <= 429999),]
   rede <- dadoi[!duplicated(dadoi[c(4,5)]) & !is.na(dadoi$no_fantasia),]
   rede <- left_join(rede[,c('codmunres', 'nu_longitude','nu_latitude')], centroide, by = c('codmunres' = 'cod6'))
   rede <- rede[!is.na(rede[,2]),]
   
   linha <- lapply(1:nrow(rede), 
                    function(i){sp::Lines(list(sp::Line(data.table::rbindlist(list(rede[i,2:3], rede[i,4:5])))), as.character(i))}) %>% sp::SpatialLines(.)

    labellss <- sprintf(
  "<strong>%s</strong><br/> %s %s <br/> %s %s" , #  people / mi<sup>2</sup>",
   dado_hosp$no_fantasia, 'Total NV: ', dado_hosp$ones, '% parto Vaginal: ', dado_hosp$perc) %>% lapply(htmltools::HTML)
   
   leafletProxy('hospital_mapa_leaflet') %>%
        #addProviderTiles("OpenStreetMap.Mapnik") %>% leaflet()
        addProviderTiles(providers$CartoDB.Positron,  options = providerTileOptions(minZoom = 7)) %>%
        setView(lat = -27.5, lng = -51, zoom = 7)  %>% clearControls() %>% clearShapes() %>% clearMarkers() %>%
        hideGroup('Rede de nascimentos') %>%
        addCircleMarkers(data = dado_hosp, lng = ~nu_longitude, lat = ~nu_latitude, radius = ~scales::rescale(ones, c(5,20)), stroke = F, 
   label = labellss,
  labelOptions = labelOptions(
    style = list("font-weight" = "normal", padding = "3px 8px"),
    textsize = "12px",
    maxWidth = '200px',
    direction = "auto"))  %>%

  addPolylines(data = linha,color = "red",
        weight = 1, opacity = 0.3, group = 'Rede de nascimentos') %>%

       addLayersControl(
           overlayGroups = 'Rede de nascimentos',
            options = layersControlOptions(collapsed = F), position = 'bottomleft' )  
                      
  }) #observe 

