  #server para a aba de casos (23-jul-24)
  #----------------------------------------------------------------------
 
  #opções botões dropdown (TODO, 15-jul-24)
   output$caso_dropopcoes <- renderUI({
                               tagList(
                                div(style = 'z-index: 50;',
                                checkboxInput('caso_obito', 'Somente óbitos? ', value = F), 
                                selectInput('caso_atuacao', 'Atuação do CIEVS', choices = c('Monitoramento', 'Resposta'),
                                selected = c('Monitoramento', 'Resposta'), multiple = T) ,
                                selectInput('caso_diaginicial', 'Diagnóstica inicial:', choices = c('Todos',unique(casos$caso_diagnostica_inicial))),
                                selectInput('caso_diagfinal', 'Diagnóstica final:', choices = c('Todos',unique(casos$caso_diag_final)))
                                )#         
                                        ) #end tag list
   })
  
   #output$testei <- renderPrint({})
   #organizando os dados
         dados_caso <- reactiveVal(casos)

            observeEvent(c(input$head_atualizar),{# input$home_atualizar,
            
            req(input$current_tab == 'casos')
                   dadoi <- dados_analise()[[2]]
                   if(input$caso_dropdown >0){
                       if(isTRUE(input$caso_obito)){dadoi <- dadoi[dadoi$caso_obito_notific == 'Sim',]}
                       dadoi <- dadoi[which(with(dadoi, {
                                        caso_atuacao %in% input$caso_atuacao})), ]
                       if(input$caso_diaginicial != 'Todos'){
                        dadoi <- dadoi[which(dadoi$caso_diagnostica_inicial == input$caso_diaginicial),]
                       }
                       if(input$caso_diagfinal != 'Todos'){
                        dadoi <- dadoi[which(dadoi$caso_diag_final == input$caso_diagfinal),]
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
                    dados_caso(dadoi)
                   }
                          },  ignoreInit = T, ignoreNULL = T, priority = -10) #end observeEvent

  #----------------------------------------------------------------
  #gráficos
  #piramide_etária
  mod_summary_card_server('caso_piramide',
                   card_large(heading = 'Pirâmide etária',
                      apexchartOutput('caso_grafpiramide', height = '320px')
                      ))

  
  output$caso_grafpiramide <- renderApex({
        dadoi <- dados_caso()
        dadoi <- with(dadoi, as.data.frame(table(faixa_etaria, caso_sexo_paciente)))
        dadoi <- tidyr::spread(dadoi, key = caso_sexo_paciente, value = Freq)
        
        if(ncol(dadoi) == 2){lista <- list(list(name = names(dadoi)[2], data = dadoi[,2]))}else{
        lista <- list(list(name = 'Masculino',data = dadoi[,'Masculino']*(-1)),
                                          list(name = 'Feminino',data = dadoi[,'Feminino']))
        }
        list(series = lista,
                                              chart = list(type = 'bar', 
                                                       #toolbar = c(show = FALSE),
                                                       height = '100%',
                                                       stacked = T),
                                              #colors = c('#008FFB', '#FF4560'),
                                              dataLabels = c(enabled = FALSE),
                                              plotOptions = list(bar = list(horizontal = T,
                                                                       barheight = '80%')),
                                              xaxis = list(#labels = c(show = FALSE),
                                                      categories =dadoi[,1]
                                                      ),
                                              yaxis = list(stepSize = 1),
                                              grid = list(
                                                          xaxis = list(lines = c(show = FALSE))),
                                              legend = c(show = T)
                                              )

  })

  #série temporal

  mod_summary_card_server('caso_serie',
      tags$div(class = 'card',
                    tags$div(class = 'card-header',
                    h3('Série Anual')),
                    tags$div(class = 'card-body',
                    apexchartOutput('caso_grafserie')
                    )
      )
  )

  
  output$caso_grafserie <- renderApex({
      
            dadoi <- dados_caso()[,'id']
            dadoii <- dados_analise()[[1]]
            dadoi <- dadoii[dadoii$id %in% dadoi,]

            dadoi$mes <- with(dadoi, substr(dadoi$vigilancia_dt_not,6,7)) %>% as.numeric
            dadoi$mes <- factor(dadoi$mes, levels = c(1:12))
            dadoi$ano <- with(dadoi, substr(dadoi$vigilancia_dt_not,1,4)) %>% as.numeric
            anos <- unique(dadoi$ano)

            dadoi <- with(dadoi, as.data.frame(table(mes, ano)))
            #dadoi <- tidyr::spread(dadoi, key = ano, value = Freq)
            lista <- lapply(anos, function(x){
                dadoii <- dadoi[which(dadoi$ano == x),]
                dadoii <- tidyr::spread(dadoii, key = ano, value = Freq)
                list(name = x, data = dadoii[,2])
            }) %>% unname
            
            list(series = lista,
                            chart = list(type = 'bar',
                                         toolbar = c(show = TRUE),
                                         height = 300),
                            plotOptions = list(bar = list(
                                                horizontal= FALSE,
                                                endingShape = 'rounded'
                                                        )),
                            xaxis = list(categories = c('jan','fev','mar','abr','maio','jun','jul','ago', 'set','out','nov','dez')),
                            legend = list(position = 'bottom'))

  })


  #gráfico atuação cievs (add em 23-jul-24)
  mod_summary_card_server('caso_atuacao',
      tags$div(class = 'card',
                    tags$div(class = 'card-header',
                    h3('Atuação do CIEVS')),
                    tags$div(class = 'card-body',
                    apexchartOutput('caso_grafatuacao')
                    )
      )
  )
  
  output$caso_grafatuacao <- renderApex({
      
            dadoi <- dados_caso()
            dadoi <- factor(c(dadoi[,'caso_atuacao']), levels = c('Monitoramento', 'Resposta'))
            
            dadoi <- as.data.frame(table(dadoi))
            dadoi <- dplyr::arrange(dadoi, desc(Freq))
            
            list(series = c(dadoi[,2]),
                            chart = list(type = 'donut',
                                         toolbar = c(show = TRUE),
                                         height = 300),
                            labels = dadoi[,1],
                            legend = list(position = 'bottom'))

  })

  #Origem da informacao
   mod_summary_card_server('caso_origem_info',
      tags$div(class = 'card',
                    tags$div(class = 'card-header',
                    h3('Origem da informação')),
                    tags$div(class = 'card-body',
                    apexchartOutput('caso_graforigem')
                    )
      )
  )

  output$caso_graforigem <- renderApex({
      
            dadoi <- dados_caso()[,'id']
            dadoii <- dados_analise()[[1]]
            vigilancia_origem <- unique(dadoii$vigilancia_origem)
            dadoi <- dadoii[dadoii$id %in% dadoi,]
            
            dadoi <- factor(c(dadoi[,'vigilancia_origem']), levels = vigilancia_origem)
            
            dadoi <- as.data.frame(table(dadoi))
            dadoi <- dplyr::arrange(dadoi, desc(Freq))
            
            list(series = c(dadoi[,2]),
                            chart = list(type = 'pie',
                                         toolbar = c(show = TRUE),
                                         height = 300),
                            labels = dadoi[,1],
                            legend = list(position = 'bottom'))

  })

  #Status da investigaão

   mod_summary_card_server('caso_status',
      tags$div(class = 'card',
                    tags$div(class = 'card-header',
                    h3('Status da investigação')),
                    tags$div(class = 'card-body',
                    apexchartOutput('caso_grafstatus')
                    )
      )
  )

  output$caso_grafstatus <- renderApex({
      
            dadoi <- dados_caso()[,'id']
            dadoii <- dados_analise()[[1]]
            dadoi <- dadoii[dadoii$id %in% dadoi,]

            dadoi <- factor(dadoi$vigilancia_status, levels = c('Em investigação', 'Encerrada'))
            dadoi <- as.data.frame(table(dadoi))

            list(series = list(list(name = 'Status', data = dadoi[,2])),
                            chart = list(type = 'bar',
                                         toolbar = c(show = TRUE),
                                         height = 280),
                            plotOptions = list(bar = list(
                                                horizontal= TRUE,
                                                borderRadius = 4,
                                                borderRadiusApplication = 'end'
                                                        )),
                            dataLabels = list(enabled = FALSE),
                            xaxis = list(categories = dadoi[,1]))

  })

  #nacionalidade

  mod_summary_card_server('caso_nacionalidade',
      tags$div(class = 'card',
                    tags$div(class = 'card-header',
                    h3('Nacionalidade')),
                    tags$div(class = 'card-body',
                    reactableOutput('caso_tabnacionalidade')
                    )
      )
  )

  output$caso_tabnacionalidade <- renderReactable({
                dadoi <- dados_caso()
                dadoi <- dadoi[dadoi$caso_pais != 'Brasil',]
                validate(need(nrow(dadoi) > 0, 'Não há estrangeiros no período selecionado'))

                dadoi <- as.data.frame(table(dadoi$caso_pais))
                names(dadoi) <- c('País', 'Quantidade')
                reactable(dadoi)

  })


  #equipe técnica
  mod_summary_card_server('caso_equipetecnica',
      tags$div(class = 'card',
                    tags$div(class = 'card-header',
                    h3('Equipes técnicas acionadas')),
                    tags$div(class = 'card-body',
                    apexchartOutput('caso_grafequipetecnica')
                    )
      )
  )

  output$caso_grafequipetecnica <- renderApex({
      
            dadoi <- dados_caso()
            dadoi <- dadoi[which(!is.na(dadoi$caso_equipe_tecnica)),]
             validate(need(nrow(dadoi) > 1, 'Não há dados suficientes a serem mostrados'))

            dadoi <- as.data.frame(table(dadoi$caso_equipe_tecnica))
            dadoi <- dplyr::arrange(dadoi, desc(Freq))
            if(nrow(dadoi == 1)){label <- list(dadoi[,1])} else {label <- dadoi[,1]}
            list(series = c(dadoi[,2]),
                            chart = list(type = 'pie',
                                         toolbar = c(show = TRUE),
                                         height = 300),
                            labels = label,
                            legend = list(position = 'bottom'))

  })


  #sankey graph

  mod_summary_card_server('caso_sint_achado_diagfin',
      tags$div(class = 'card',
                    tags$div(class = 'card-header',
                    h3('Fluxo (escolher nome...)')),
                    tags$div(class = 'card-body',
                    echartsOutput('caso_grafsint_achado_diagfin')
                    )
      )
  )


  output$caso_grafsint_achado_diagfin <- renderEcharts({
        dadoi <- dados_caso()[,c('caso_diagnostica_inicial','caso_achado_laboratorial','caso_diag_final')]
        dadoi[,1:3] <- sapply(dadoi, function(x){ifelse(x == '', NA, x)}) %>% as.data.frame

        nodes <- c(unique(dadoi$caso_diagnostica_inicial), unique(unlist(strsplit(dadoi$caso_achado_laboratorial, '\\|'))), unique(dadoi$caso_diag_final))
        nodes <- unique(nodes[!is.na(nodes)])
        nodes <- c(nodes, c('Sem atribuição (diagnóstica inicial)','Sem atribuição (achado)','Sem atribuição (diagnóstica final)'))
        nodes <- lapply(nodes, function(x){list(name = x)})
        
        dadoii <- purrr::map_df(seq_along(dadoi[,1]), function(x){
            dadoi <- dadoi[x,]
            if(is.na(dadoi[,1])){dadoi[,1] <- 'Sem atribuição (diagnóstica inicial)'}
            if(is.na(dadoi[,2])){dadoi[,2] <- 'Sem atribuição (achado)'}
            if(is.na(dadoi[,3])){dadoi[,3] <- 'Sem atribuição (diagnóstica final)'} 
            achado_lab <- strsplit(dadoi[,2], '\\|') %>% unlist
            data.frame(diagnostica_inicial = dadoi[,1], achado_lab, diagnostica_final = dadoi[,3])
        }) 

        sankey_1 <- as.data.frame(table(dadoii[,1], dadoii[,2]), stringsAsFactors = F) %>% .[.[,3] >0,]
        names(sankey_1) <- c('source', 'target', 'value')
        sankey_2 <- as.data.frame(table(dadoii[,2], dadoii[,3]), stringsAsFactors = F) %>% .[.[,3] >0,]
        names(sankey_2) <- c('source', 'target', 'value')

        sankey <- dplyr::bind_rows(sankey_1, sankey_2)

        links <- lapply(seq_along(sankey[,1]), function(x){
                c(sankey[x,])
        }) %>% unname

         list(
        series = list(
            type = 'sankey',
            data = nodes,
            links = links,
            lineStyle = list(color = 'gradient', curveness = .5)
        ),
        tooltip = list(trigger = 'item', triggerOn = 'mousemove')
     )

  })
