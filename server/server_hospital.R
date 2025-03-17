  #server para a aba de hospital (24-out-24, 14:12h)
  #----------------------------------------------------------------------


   output$hospital_dropopcoes <- renderUI({
                               tagList(
                                div(style = 'z-index: 50;',
                                selectInput('hospital_natureza', 'Adm. Unidade Saúde', choices = c('Público', 'Privado'), 
                                  selected = c('Público', 'Privado'),multiple = T),

                                selectInput('hospital_regiao', 'Região: ', choices = c('Todas', unique(tab_regioes$reg_saude)), 
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
    
            #observe({# input$home_atualizar,
             #req(input$hospital_mapa_leaflet_groups)
             #TODO javascript para input das abas
             observeEvent(c(input$head_atualizar, input$current_tab == 'hospital'),{ #
             req(input$current_tab == 'hospital')
             dadoi <- dados_all()

              if(input$hospital_dropdown >0){
                      dadoi <- subset(dadoi, mes %in% input$hospital_meses) #meses
                      dadoi <- subset(dadoi, natureza %in% input$hospital_natureza)

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
                      if(length(input$hospital_gestacao) != 6){
                        dadoi <- subset(dadoi, cat_gestacao %in% input$hospital_gestacao)
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
                      dadoi <- dados_hosp()
                      dadoi_pub <- nrow(dadoi[which(dadoi$natureza == 'Público'),])
                      tagList(
                      h1(dadoi_pub),
                      p(paste0('representando ',round(dadoi_pub*100/nrow(dadoi),2),'% do total de NV'))
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
                      dadoi <- dados_hosp()
                      dadoi_priv <- nrow(dadoi[which(dadoi$natureza == 'Privado'),])
                      tagList(
                      h1(dadoi_priv),
                      p(paste0('representando ',round(dadoi_priv*100/nrow(dadoi),2),'% do total de NV'))
                      )
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
                               
  output$hospital_mapa_leaflet <- renderLeaflet({  hospital_leaflet()
        })
        
  
  #observeEvent(dados_hosp(),{ #c(input$head_atualizar, input$current_tab == 'hospital')
   #observeEvent(c(input$head_atualizar, input$current_tab == 'hospital'),{
   hospital_leaflet <- eventReactive(c(input$head_atualizar, input$current_tab == 'hospital'),{
   #observe({
   req(input$current_tab == 'hospital')
   req(!is.null(dados_hosp()))
   dadoi <- dados_hosp()
   dadoi$ones <- 1

   #dado_hosp <- with(dadoi, as.data.frame(table(no_fantasia, nu_latitude, nu_longitude)))
   dado_hosp <- aggregate(ones ~ codestab + no_fantasia + nu_longitude + nu_latitude + natureza, data = dadoi, FUN = sum)
   dado_vag <- aggregate(ones ~ codestab, data = dadoi[which(dadoi$cat_parto == 'Vaginal'),], FUN = sum)
   dado_hosp <- dplyr::left_join(dado_hosp, dado_vag, by = 'codestab', suffix = c('', '_vag'))
   dado_hosp$perc <- with(dado_hosp, round(ones_vag*100/ones,2))
   dado_hosp[is.na(dado_hosp)] <- 0
   dado_hosp$natureza <- factor(dado_hosp$natureza, levels = c('Público', 'Privado'))

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
   
   natu_color <- colorFactor('RdBu', dado_hosp$natureza)

   #leafletProxy('hospital_mapa_leaflet') %>%
        #addProviderTiles("OpenStreetMap.Mapnik") %>% leaflet()
    leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron,  options = providerTileOptions(minZoom = 7)) %>%
        setView(lat = -27.5, lng = -51, zoom = 7)  %>% 
        clearControls() %>% clearShapes() %>% clearMarkers() %>%
       addCircleMarkers(data = dado_hosp, lng = ~nu_longitude, lat = ~nu_latitude, radius = ~scales::rescale(ones, c(5,20)), stroke = F, 
        color = ~natu_color(natureza), fillOpacity = .6,
   label = labellss,
  labelOptions = labelOptions(
    style = list("font-weight" = "normal", padding = "3px 8px"),
    textsize = "12px",
    maxWidth = '200px',
    direction = "auto"))  %>%

    leaflet::addLegend(pal = natu_color, values = dado_hosp$natureza, opacity = 0.7,
     title = 'Natureza da\nUS',
  position = "bottomright", layerId="colorLegend2",className = 'info legenda')  %>%

  addPolylines(data = linha,color = "red",
        weight = 1, opacity = 0.3, group = 'Rede de nascimentos')   %>%
 hideGroup('Rede de nascimentos') %>%
        
       addLayersControl(
           overlayGroups = 'Rede de nascimentos',
            options = layersControlOptions(collapsed = F), position = 'bottomleft' )  
                      
  })#, priority = -10, ignoreNULL = F) #observe 

 #-------------------------------------------------------------------------
 #tabelas

 mod_summary_card_server('hospital_tabela',
  tags$div(class =  'card-tabs' ,
                   tabler_navbar_card(
                    tabler_navbar_menu_card('Por unidade de saúde', 'hospital_unidade_saude', selected = T),
                    tabler_navbar_menu_card('Por natureza da instituição', 'hospital_natureza')) ,

                     tabler_tabcard_items(
                      tabler_tabcard_item('hospital_unidade_saude', selected = T,
                        h4('Síntese por estabelecimento de saúde'),
                        reactableOutput('hospital_tabsintese')
                      )
                      ,
                      tabler_tabcard_item('hospital_natureza',selected = F,
                        h4('Síntese por natureza da US'),
                        reactableOutput('hospital_natureza_sintese')
                      )
                     )
      ) #end card
  )


   
 output$hospital_tabsintese <- renderReactable({
    dadoi <- dados_hosp()
    dadoi$ones <- 1
    dadoi_cesariano <- aggregate(ones ~ codestab + reg_saude.nasc + no_fantasia + cat_parto, data = dadoi, FUN = sum, na.rm = T) %>%
    tidyr::spread(., key = cat_parto, value = ones)
    dadoi_cesariano$total <- apply(dadoi_cesariano[,4:5], 1 ,sum, na.rm = T)
    dadoi_cesariano$perc  <- with(dadoi_cesariano, round(Cesariana*100/total,2))
    dadoi_cesariano <- dadoi_cesariano[,c(1,2,3,6,7)]

    dadoi_consultas <- aggregate(ones ~ codestab , data = dadoi[which(dadoi$cat_consultas == '7 ou mais'),], sum, na.rm = T)
    dadoi_prenatal <- aggregate(ones ~ codestab , data = dadoi[which(dadoi$cat_consultas == '7 ou mais' & dadoi$cat_prenatal == 'Até 3º mês'),], sum, na.rm = T)
    dadoi_gestacao <- aggregate(ones ~ codestab + cat_gestacao, data = dadoi, FUN = sum, na.rm = T) %>%
    tidyr::spread(., key = cat_gestacao, value = ones)

    dadoi_all <- dplyr::left_join(dadoi_cesariano, dadoi_consultas, by = 'codestab') %>%
                 dplyr::left_join(., dadoi_prenatal, by = 'codestab') %>%
                 dplyr::left_join(., dadoi_gestacao, by = 'codestab')

    #TODO organizar melhor os nomes... (05-nov-24, 16:42h)
    dadoi_all[,c(6:12)] <- sapply(6:12, function(x){
      dadoi_all[,x] <- round(dadoi_all[,x]*100/dadoi_all$total,2)
    })

    dadoi_all[,c(1,13)] <- NULL
    names(dadoi_all)[c(3,4,5,6)] <- c('Total NV', '% cesariana', '7 consultas', '7 consultas até 3ª trim')

    reactable(dadoi_all, pagination = F, groupBy = "reg_saude.nasc", columns = list(`Total NV` = colDef(aggregate = 'sum')), searchable = T)

 })


 output$hospital_natureza_sintese <- renderReactable({
    dadoi <- dados_hosp()
    dadoi$ones <- 1
    dadoi_cesariano <- aggregate(ones ~ reg_saude.nasc + natureza + cat_parto, data = dadoi, FUN = sum, na.rm = T) %>%
    tidyr::spread(., key = cat_parto, value = ones)
    dadoi_cesariano$total <- apply(dadoi_cesariano[,3:4], 1 ,sum, na.rm = T)
    dadoi_cesariano$perc  <- with(dadoi_cesariano, round(Cesariana*100/total,2))
    dadoi_cesariano <- dadoi_cesariano[,c(1,2,5,6)]

    dadoi_consultas <- aggregate(ones ~ reg_saude.nasc + natureza , data = dadoi[which(dadoi$cat_consultas == '7 ou mais'),], sum, na.rm = T)
    dadoi_prenatal <- aggregate(ones ~ reg_saude.nasc + natureza , data = dadoi[which(dadoi$cat_consultas == '7 ou mais' & dadoi$cat_prenatal == 'Até 3º mês'),], sum, na.rm = T)
    dadoi_gestacao <- aggregate(ones ~ reg_saude.nasc + natureza + cat_gestacao, data = dadoi, FUN = sum, na.rm = T) %>%
    tidyr::spread(., key = cat_gestacao, value = ones)

    dadoi_all <- dplyr::left_join(dadoi_cesariano, dadoi_consultas, by = c('reg_saude.nasc', 'natureza')) %>%
                 dplyr::left_join(., dadoi_prenatal, by = c('reg_saude.nasc', 'natureza')) %>%
                 dplyr::left_join(., dadoi_gestacao, by = c('reg_saude.nasc', 'natureza'))

    #TODO organizar melhor os nomes... (05-nov-24, 16:42h)
    dadoi_all[,c(5:12)] <- sapply(5:12, function(x){
      dadoi_all[,x] <- round(dadoi_all[,x]*100/dadoi_all$total,2)
    })

    #dadoi_all[,c(1,13)] <- NULL
    names(dadoi_all)[c(3,4,5,6)] <- c('Total NV', '% cesariana', '7 consultas', '7 consultas até 3ª trim')

    reactable(dadoi_all, pagination = F, groupBy = "reg_saude.nasc", columns = list(`Total NV` = colDef(aggregate = 'sum')), searchable = T)

 })