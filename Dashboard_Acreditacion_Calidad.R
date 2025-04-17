# --- Cargar librerías ---
library(shiny)       # Interfaz Shiny
library(shinydashboard)
library(plotly)      # Usamos plotly para gráficos interactivos
library(dplyr)
library(DT)
library(readxl)
library(lubridate)   # Manejo de fechas
library(text2vec)
library(stringr)
library(stopwords)
library(caret)
library(tidyr)
library(wordcloud2)


# Establecer ubicación (modifica "ruta" según corresponda)
#setwd("C:/Users/pipen/Documents/4to IES/Bussiness Inteligence/Unidad 1")
# --- Cargar datos ---
postulaciones <- read_excel("Datos Postulaciones 2.0.xlsx", sheet = "Postulaciones")
profesores <- read_excel("Datos Postulaciones 2.0.xlsx", sheet = "Simulación Profesores")

# Cargar múltiples archivos Excel (para opiniones) que cumplan el patrón
archivos_excel <- list.files(pattern = "^Universidad.*\\.xlsx$")
lista_dataframes <- lapply(archivos_excel, read_excel)
opiniones <- bind_rows(lista_dataframes)
opiniones$published_at_date <- as.Date(opiniones$published_at_date)

# Reemplazar los nombres de universidades en opiniones por sus respectivos IDs
opiniones <- opiniones %>%
  mutate(place_name = recode(place_name,
                             "Universidad de Valparaiso" = "19084",
                             "Universidad de Santiago de Chile" = "16048",
                             "Universidad de Concepcion" = "13065",
                             "Universidad del Bio-Bio" = "29018",
                             "UCM" = "35069",
                             "Pontificia Universidad Catolica de Chile - Campus San Joaquin" = "12049"))

# Diccionario de universidades (para el filtro global)
# Las claves son los nombres amigables y los valores sus IDs
universidades_dict <- c(
  "USACH" = "16048",
  "UDEC" = "13065",
  "PUC" = "12049",
  "U. Valparaiso" = "19084",
  "UCM" = "35069",
  "U. Bio-Bio" = "29018"
)

# --- UI ---
ui <- dashboardPage(
  dashboardHeader(
    title = tags$div(
      tags$style(HTML("
        .main-header .logo {
          font-size: 14px;
          text-overflow: ellipsis;
          white-space: nowrap;
          overflow: hidden;
        }
      ")),
      "Acreditación Ing. en Estadística"
    )
  ),
  dashboardSidebar(
    # Filtro global: se muestran los nombres y se usan los IDs internamente
    selectInput("universidades_seleccionadas", "Selecciona Universidad(es):",
                choices = universidades_dict,
                selected = universidades_dict,
                multiple = TRUE),
    sidebarMenu(
      menuItem("Resumen", tabName = "resumen", icon = icon("chart-line")),
      menuItem("Indicadores Académicos", tabName = "indicadores", icon = icon("graduation-cap")),
      menuItem("Profesores", tabName = "profesores", icon = icon("chalkboard-teacher")),
      menuItem("Postulaciones", tabName = "postulaciones", icon = icon("users")),
      menuItem("Opiniones Estudiantiles", tabName = "opiniones", icon = icon("comments")),
      menuItem("Análisis de Texto", tabName = "texto", icon = icon("font"))
    )
  ),
  dashboardBody(
    tabItems(
      
      #-------------------------------# 
      #------------Resumen------------#
      #-------------------------------#
      
      tabItem(tabName = "resumen",
              fluidRow(
                valueBoxOutput("total_postulaciones"),
                valueBoxOutput("promedio_puntaje"),
                valueBoxOutput("satisfaccion_profesores"),
                valueBoxOutput("valoracion_apartados_estudiantiles"),
                valueBoxOutput("mediana_orden_preferencia"),
                valueBoxOutput("satisfaccion_profesores_jefe"),
                # Aquí se coloca el gráfico radial
                box(title = "Gráfico Radar", width = 6, status = "primary", solidHeader = TRUE, plotlyOutput("grafico_radar")),
                box(title = "Diferencias respecto a UCM", width = 6, status = "warning", solidHeader = TRUE, plotlyOutput("grafico_radar_diff"))
              )
      ),
      
      
      #------------------------------# 
      #----Indicadores Académicos----#
      #------------------------------#
      
      
      tabItem(tabName = "indicadores",
              fluidRow(
                box(title = "Puntaje Promedio", width = 6, status = "primary", solidHeader = TRUE, plotlyOutput("puntaje_promedio_plot")),
                box(title = "Boxplot de Puntaje", width = 6, status = "info", solidHeader = TRUE, plotlyOutput("puntaje_boxplot_plot"))
              )
      ),
      
      
      #------------------------------# 
      #----------Profesores----------#
      #------------------------------#
      tabItem(tabName = "profesores",
              fluidRow(
                box(title = "Evaluación de Profesores (realizada por estudiantes)", width = 12, status = "info", solidHeader = TRUE, plotlyOutput("evaluacion_profesores_plot", height = "300px")),
                box(title = "Evaluación de Profesores (realizada por jefe)", width = 12, status = "info", solidHeader = TRUE, plotlyOutput("evaluacion_profesores_jefe_plot", height = "300px"))
              )
      ),
      
      
      #-------------------------------# 
      #---------Postulaciones---------#
      #-------------------------------#
      tabItem(tabName = "postulaciones",
              fluidRow(
                box(title = "Distribución de Postulaciones: Orden de Preferencia", width = 12, status = "primary", solidHeader = TRUE, plotlyOutput("postulaciones_plot"))
              )
      ),
      
      
      #-------------------------------# 
      #----Opiniones Estudiantiles----#
      #-------------------------------#
      tabItem(tabName = "opiniones",
              fluidRow(
                box(title = "Opiniones de Estudiantes (Media Ponderada)", width = 12, status = "success", solidHeader = TRUE, plotlyOutput("opiniones_plot"))
              ),
              fluidRow(
                uiOutput("labels_medias_actuales")
              )
      ),
      
      
      #-------------------------------# 
      #-------------Texto-------------#
      #-------------------------------#
      tabItem(tabName = "texto",
              fluidRow(
                box(title = "Configuración de Filtros", width = 4, status = "primary", solidHeader = TRUE,
                    fluidRow(
                      column(6, selectInput("filtro_rating", "Selecciona estrellas:", choices = 1:5, selected = 1:5, multiple = TRUE)),
                      column(6, radioButtons("tipo_grafico_texto", "Tipo de gráfico:", choices = c("Gráfico de Barras", "Nube de Palabras"), inline = TRUE))
                    )
                )
              ),
              fluidRow(
                conditionalPanel(
                  condition = "input.tipo_grafico_texto == 'Gráfico de Barras'",
                  box(title = "Top 20 Palabras Frecuentes", width = 12, status = "info", solidHeader = TRUE,
                      plotlyOutput("top_words_plot", height = "550px"))
                ),
                conditionalPanel(
                  condition = "input.tipo_grafico_texto == 'Nube de Palabras'",
                  box(title = "Nube de Palabras", width = 12, status = "info", solidHeader = TRUE,
                      wordcloud2Output("wordcloud_plot", height = "550px"))
                )
              )
      )
      

    )
  )
)

# --- Server ---
server <- function(input, output) {
  # Función reactiva para obtener directamente los IDs seleccionados (son cadenas)
  universidades_ids <- reactive({
    input$universidades_seleccionadas
  })
  
  # ---------------------- Resumen ----------------------------
  
  output$total_postulaciones <- renderValueBox({
    total <- nrow(postulaciones %>% filter(ID_UNIVERSIDAD %in% as.numeric(universidades_ids())))
    valueBox(total, "Total de Postulaciones", icon = icon("users"), color = "green")
  })
  
  output$promedio_puntaje <- renderValueBox({
    promedio <- postulaciones %>%
      filter(ID_UNIVERSIDAD %in% as.numeric(universidades_ids())) %>%
      pull(PTJE_PREF) %>%
      mean(na.rm = TRUE)
    valueBox(round(promedio, 2), "Puntaje Promedio de Postulación", icon = icon("chart-line"), color = "blue")
  })
  
  output$satisfaccion_profesores <- renderValueBox({
    datos <- profesores %>% filter(ID_UNIVERSIDAD %in% as.numeric(universidades_ids()))
    promedio <- ifelse(nrow(datos) == 0, 0, round(mean(datos$EVALUACION_DOCENTE, na.rm = TRUE), 2))
    valueBox(paste0(promedio), "Satisfacción Promedio con Profesores (estudiantes)", icon = icon("chalkboard-teacher"), color = "red")
  })
  
  output$satisfaccion_profesores_jefe <- renderValueBox({
    datos <- profesores %>% filter(ID_UNIVERSIDAD %in% as.numeric(universidades_ids()))
    promedio <- ifelse(nrow(datos) == 0, 0, round(mean(datos$EVALUACION_DOCENTE_JEFE, na.rm = TRUE), 2))
    valueBox(paste0(promedio), "Satisfacción Promedio con Profesores (jefe)", icon = icon("chalkboard-teacher"), color = "blue")
  })
  
  output$valoracion_apartados_estudiantiles <- renderValueBox({
    datos_opiniones <- opiniones %>%
      filter(place_name %in% universidades_ids()) %>%
      mutate(Universidad = names(universidades_dict)[match(place_name, universidades_dict)])
    valoracion <- datos_opiniones %>%
      group_by(Universidad) %>%
      summarise(valoracion_media = mean(rating, na.rm = TRUE)) %>%
      summarise(valoracion_final = mean(valoracion_media, na.rm = TRUE)) %>%
      pull(valoracion_final)
    valueBox(round(valoracion, 2), "Valoración Apartados Estudiantiles", icon = icon("star"), color = "blue")
  })
  
  output$mediana_orden_preferencia <- renderValueBox({
    datos_postulaciones <- postulaciones %>% filter(ID_UNIVERSIDAD %in% as.numeric(universidades_ids()))
    mediana <- datos_postulaciones %>% summarise(mediana_pref = median(ORDEN_PREF, na.rm = TRUE)) %>% pull(mediana_pref)
    valueBox(round(mediana, 2), "Mediana de Orden de Preferencia", icon = icon("sort-amount-down"), color = "orange")
  })
  
  # ---------- Gráfico Radar en pestaña "Resumen" ----------
  output$grafico_radar <- renderPlotly({
    
    # Obtener los IDs seleccionados (son cadenas)
    selected_ids <- as.numeric(universidades_ids())
    
    # Mapear cada ID al nombre usando el diccionario: 
    # Dado que universidades_dict es: Nombre -> ID, para invertir usamos:
    get_univ_name <- function(id) {
      nm <- names(universidades_dict)[which(universidades_dict == as.character(id))]
      if(length(nm) == 0) return(NA) else return(nm)
    }
    
    # Calcular indicadores por universidad (filtrados por la selección global)
    radar_data <- lapply(selected_ids, function(id) {
      univ_name <- get_univ_name(id)
      
      # Evaluación de profesores (estudiantes)
      eval_est <- profesores %>% 
        filter(ID_UNIVERSIDAD == id) %>% 
        summarise(val = mean(EVALUACION_DOCENTE, na.rm = TRUE)) %>% 
        pull(val)
      
      # Opinión estudiantes
      opin_est <- opiniones %>% 
        filter(place_name == as.character(id)) %>% 
        summarise(val = mean(rating, na.rm = TRUE)) %>% 
        pull(val)
      
      # Puntaje promedio de postulación
      puntaje <- postulaciones %>% 
        filter(ID_UNIVERSIDAD == id) %>% 
        summarise(val = mean(PTJE_PREF, na.rm = TRUE)) %>% 
        pull(val)
      
      # Cantidad de postulaciones (número de filas)
      cant_post <- postulaciones %>% 
        filter(ID_UNIVERSIDAD == id) %>% 
        nrow()
      
      # Evaluación de profesores (jefe)
      eval_jefe <- profesores %>% 
        filter(ID_UNIVERSIDAD == id) %>% 
        summarise(val = mean(EVALUACION_DOCENTE_JEFE, na.rm = TRUE)) %>% 
        pull(val)
      
      data.frame(
        Universidad = univ_name,
        EvalEst = ifelse(is.na(eval_est), 0, eval_est),
        OpinEst = ifelse(is.na(opin_est), 0, opin_est),
        Puntaje = ifelse(is.na(puntaje), 0, puntaje),
        CantPost = cant_post,
        EvalJefe = ifelse(is.na(eval_jefe), 0, eval_jefe)
      )
    })
    
    radar_data <- do.call(rbind, radar_data)
    
    # Definir máximos para escala (ajusta según la naturaleza de tus datos)
    max_eval_est  <- 7    # supongamos escala de 0 a 10
    max_opin      <- 5     # de 0 a 5
    max_puntaje   <- 1000  # de 0 a 1000 (puedes ajustar)
    max_cant_post <- nrow(postulaciones)   # supongamos máximo 500 postulaciones (ajusta según el contexto)
    max_eval_jefe <- 7    # de 0 a 10
    
    # Crear versión normalizada (valores entre 0 y 1)
    radar_data_norm <- radar_data
    radar_data_norm$EvalEst  <- radar_data$EvalEst / max_eval_est
    radar_data_norm$OpinEst  <- radar_data$OpinEst / max_opin
    radar_data_norm$Puntaje  <- radar_data$Puntaje / max_puntaje
    radar_data_norm$CantPost <- radar_data$CantPost / max_cant_post
    radar_data_norm$EvalJefe <- radar_data$EvalJefe / max_eval_jefe
    
    # Convertir a formato largo para Plotly (para cada universidad)
    library(tidyr)
    radar_long <- radar_data_norm %>%
      pivot_longer(cols = c("EvalEst", "OpinEst", "Puntaje", "CantPost", "EvalJefe"),
                   names_to = "Tópico", values_to = "Valor_norm")
    radar_long_real <- radar_data %>%
      pivot_longer(cols = c("EvalEst", "OpinEst", "Puntaje", "CantPost", "EvalJefe"),
                   names_to = "Tópico", values_to = "Valor_real")
    
    # Combinar valores reales para etiquetas
    radar_long <- bind_cols(radar_long, Valor_real = radar_long_real$Valor_real)
    
    # "Cerrar" el polígono: para cada universidad, repetir el primer tópico al final
    radar_long_closed <- radar_long %>%
      group_by(Universidad) %>%
      arrange(match(Tópico, c("EvalEst", "OpinEst", "Puntaje", "CantPost", "EvalJefe"))) %>%
      summarise(
        Tópico = c(Tópico, first(Tópico)),
        Valor_norm = c(Valor_norm, first(Valor_norm)),
        Valor_real = c(Valor_real, first(Valor_real)),
        .groups = "drop"
      )
    
    # Adjust Tópico names for the radar plot
    radar_long_closed <- radar_long_closed %>%
      mutate(
        Tópico = recode(Tópico, 
                        EvalEst = "Evaluación estudiantes",
                        OpinEst = "Opinión estudiantes",
                        Puntaje = "Puntaje",
                        CantPost = "Cantidad Postulantes",
                        EvalJefe = "Evaluación Jefe")
      )
    
    # Crear figura radar con Plotly
    fig <- plot_ly(type = 'scatterpolar', fill = 'none', mode = 'lines+markers')
    for (uni in unique(radar_long_closed$Universidad)) {
      data_uni <- radar_long_closed %>% filter(Universidad == uni)
      
      fig <- fig %>%
        add_trace(
          r = data_uni$Valor_norm,
          theta = data_uni$Tópico,
          name = uni,
          text = paste0(data_uni$Tópico, ": ", round(data_uni$Valor_real,2)),
          hoverinfo = "text",
          mode = "lines+markers"
        )
    }
    
    fig <- fig %>%
      layout(
        title = "Radar de Indicadores por Universidad",
        polar = list(
          radialaxis = list(
            visible = FALSE,
            range = c(0, 1)
          )
        ),
        showlegend = TRUE
      )
    
    fig
  })
  

  output$grafico_radar_diff <- renderPlotly({
    # Aseguramos que UCM esté incluido: ID "35069"
    selected_ids <- as.numeric(unique(c(universidades_ids(), 35069)))
    
    # Función para obtener nombre de universidad a partir del ID
    get_univ_name <- function(id) {
      nm <- names(universidades_dict)[which(universidades_dict == as.character(id))]
      if(length(nm) == 0) return(NA) else return(nm)
    }
    
    # Calcular indicadores por universidad para los IDs seleccionados
    radar_data <- lapply(selected_ids, function(id) {
      univ_name <- get_univ_name(id)
      
      # Evaluación de profesores (estudiantes)
      eval_est <- profesores %>% 
        filter(ID_UNIVERSIDAD == id) %>% 
        summarise(val = mean(EVALUACION_DOCENTE, na.rm = TRUE)) %>% 
        pull(val)
      
      # Opinión estudiantes
      opin_est <- opiniones %>% 
        filter(place_name == as.character(id)) %>% 
        summarise(val = mean(rating, na.rm = TRUE)) %>% 
        pull(val)
      
      # Puntaje promedio de postulación
      puntaje <- postulaciones %>% 
        filter(ID_UNIVERSIDAD == id) %>% 
        summarise(val = mean(PTJE_PREF, na.rm = TRUE)) %>% 
        pull(val)
      
      # Cantidad de postulaciones
      cant_post <- postulaciones %>% 
        filter(ID_UNIVERSIDAD == id) %>% 
        nrow()
      
      # Evaluación de profesores (jefe)
      eval_jefe <- profesores %>% 
        filter(ID_UNIVERSIDAD == id) %>% 
        summarise(val = mean(EVALUACION_DOCENTE_JEFE, na.rm = TRUE)) %>% 
        pull(val)
      
      data.frame(
        Universidad = univ_name,
        EvalEst = ifelse(is.na(eval_est), 0, eval_est),
        OpinEst = ifelse(is.na(opin_est), 0, opin_est),
        Puntaje = ifelse(is.na(puntaje), 0, puntaje),
        CantPost = cant_post,
        EvalJefe = ifelse(is.na(eval_jefe), 0, eval_jefe)
      )
    })
    radar_data <- do.call(rbind, radar_data)
    
    # Definir máximos para escala (ajusta estos valores según tus datos reales)
    max_eval_est  <- 7    # Escala de 0 a 10
    max_opin      <- 5     # Escala de 0 a 5
    max_puntaje   <- 1000  # Ejemplo: 0 a 1000
    max_cant_post <- nrow(postulaciones)   # Ejemplo: 0 a 500 postulaciones
    max_eval_jefe <- 7    # Escala de 0 a 10
    
    # Normalizar los indicadores (valores entre 0 y 1)
    radar_data_norm <- radar_data
    radar_data_norm$EvalEst  <- radar_data$EvalEst / max_eval_est
    radar_data_norm$OpinEst  <- radar_data$OpinEst / max_opin
    radar_data_norm$Puntaje  <- radar_data$Puntaje / max_puntaje
    radar_data_norm$CantPost <- radar_data$CantPost / max_cant_post
    radar_data_norm$EvalJefe <- radar_data$EvalJefe / max_eval_jefe
    
    # Extraer la fila de UCM (su nombre se espera sea "UCM")
    ucm_row <- radar_data_norm[radar_data_norm$Universidad == "UCM", ]
    if(nrow(ucm_row) == 0){
      # En caso de no estar, usar cero (aunque UCM debería estar siempre incluido)
      ucm_row <- data.frame(EvalEst = 0, OpinEst = 0, Puntaje = 0, CantPost = 0, EvalJefe = 0)
    }
    
    # Crear una nueva data frame con diferencias: (valor_norm - valor_UCM_norm) + 0.5
    radar_data_diff_norm <- radar_data_norm
    radar_data_diff_norm$EvalEst  <- (radar_data_norm$EvalEst  - ucm_row$EvalEst)  + 0.5
    radar_data_diff_norm$OpinEst  <- (radar_data_norm$OpinEst  - ucm_row$OpinEst)  + 0.5
    radar_data_diff_norm$Puntaje  <- (radar_data_norm$Puntaje  - ucm_row$Puntaje)  + 0.5
    radar_data_diff_norm$CantPost <- (radar_data_norm$CantPost - ucm_row$CantPost) + 0.5
    radar_data_diff_norm$EvalJefe <- (radar_data_norm$EvalJefe - ucm_row$EvalJefe) + 0.5
    
    # Convertir a formato largo para Plotly
    library(tidyr)
    radar_long <- radar_data_diff_norm %>%
      pivot_longer(cols = c("EvalEst", "OpinEst", "Puntaje", "CantPost", "EvalJefe"),
                   names_to = "Tópico", values_to = "Valor_norm")
    radar_long_real <- radar_data %>%
      pivot_longer(cols = c("EvalEst", "OpinEst", "Puntaje", "CantPost", "EvalJefe"),
                   names_to = "Tópico", values_to = "Valor_real")
    
    radar_long <- bind_cols(radar_long, Valor_real = radar_long_real$Valor_real)
    
    # Ajustar nombres de tópicos para mostrar
    radar_long <- radar_long %>%
      mutate(
        Tópico = recode(Tópico, 
                        EvalEst  = "Evaluación estudiantes",
                        OpinEst  = "Opinión estudiantes",
                        Puntaje  = "Puntaje",
                        CantPost = "Cantidad Postulantes",
                        EvalJefe = "Evaluación Jefe")
      )
    
    # "Cerrar" el polígono: repetir el primer tópico al final por cada universidad
    radar_long_closed <- radar_long %>%
      group_by(Universidad) %>%
      arrange(match(Tópico, c("Evaluación estudiantes", "Opinión estudiantes", "Puntaje", "Cantidad Postulantes", "Evaluación Jefe"))) %>%
      summarise(
        Tópico = c(Tópico, first(Tópico)),
        Valor_norm = c(Valor_norm, first(Valor_norm)),
        Valor_real = c(Valor_real, first(Valor_real)),
        .groups = "drop_last"
      )
    
    # Crear el gráfico radar para las diferencias
    fig_diff <- plot_ly(type = 'scatterpolar', fill = 'none', mode = 'lines+markers')
    
    
    # Obtener los valores reales de UCM para cada tópico
    ucm_real <- radar_data %>% filter(Universidad == "UCM") %>%
      pivot_longer(cols = c("EvalEst", "OpinEst", "Puntaje", "CantPost", "EvalJefe"),
                   names_to = "Tópico", values_to = "Valor_UCM_real") %>%
      mutate(Tópico = recode(Tópico, 
                             EvalEst  = "Evaluación estudiantes",
                             OpinEst  = "Opinión estudiantes",
                             Puntaje  = "Puntaje",
                             CantPost = "Cantidad Postulantes",
                             EvalJefe = "Evaluación Jefe"))
    
    # Al agregar las trazas, usa diferencia real
    for (uni in unique(radar_long_closed$Universidad)) {
      data_uni <- radar_long_closed %>% filter(Universidad == uni)
      
      # Juntar con valores reales de UCM por tópico
      data_uni <- left_join(data_uni, ucm_real, by = "Tópico")
      
      fig_diff <- fig_diff %>%
        add_trace(
          r = data_uni$Valor_norm,
          theta = data_uni$Tópico,
          name = uni,
          text = paste0(
            data_uni$Tópico, ": ", 
            sprintf("%.2f", data_uni$Valor_real), 
            " (Diff: ", 
            sprintf("%+.2f", data_uni$Valor_real - data_uni$Valor_UCM_real), 
            ")"
          ),
          hoverinfo = "text",
          mode = "lines+markers"
        )
    }
    
    
    fig_diff <- fig_diff %>%
      layout(
        title = "Diferencias respecto a UCM",
        polar = list(
          radialaxis = list(
            visible = FALSE,
            range = c(0, 1)
          )
        ),
        showlegend = TRUE
      )
    
    fig_diff
  })
  
  
  
  # -------------------- Indicadores Académicos -------------------------
  
  output$puntaje_promedio_plot <- renderPlotly({
    data <- postulaciones %>%
      filter(ID_UNIVERSIDAD %in% as.numeric(universidades_ids())) %>%
      mutate(Universidad = names(universidades_dict)[match(as.character(ID_UNIVERSIDAD), universidades_dict)]) %>%
      group_by(Universidad) %>%
      summarise(Promedio = mean(PTJE_PREF, na.rm = TRUE))
    
    colores_universidades <- c(
      "PUC" = "#66C2A5",
      "U. Bio-Bio" = "#FF7F00",
      "U. Valparaiso" = "#8DA0CB",
      "UCM" = "#E78AC3",
      "UDEC" = "#A6D854",
      "USACH" = "#FFEE42"
    )
    
    plot_ly(data = data, x = ~Universidad, y = ~Promedio, type = 'bar', 
            color = ~Universidad, colors = colores_universidades) %>%
      layout(
        xaxis = list(title = "Universidades"),
        yaxis = list(title = "Puntaje", range = c(100, 1000)),
        barmode = 'stack'
      )
  })
  
  output$puntaje_boxplot_plot <- renderPlotly({
    data <- postulaciones %>%
      filter(ID_UNIVERSIDAD %in% as.numeric(universidades_ids())) %>%
      mutate(Universidad = names(universidades_dict)[match(as.character(ID_UNIVERSIDAD), universidades_dict)],
             AÑO = factor(AÑO))
    
    data <- data %>%
      mutate(Universidad_Año = paste(Universidad, AÑO, sep = "-"))
    
    colores_universidades <- c(
      "USACH" = "#FFEE42",
      "UDEC" = "#5FB348",
      "UCM" = "#F39AC0",
      "U. Valparaiso" = "#A9A9F5",
      "U. Bio-Bio" = "#FF7F00",
      "PUC" = "#66C2A5"
    )
    
    plot_ly(data = data, x = ~Universidad_Año, y = ~PTJE_PREF, type = 'box', color = ~Universidad, 
            colors = colores_universidades, boxmean = "sd") %>%
      layout(
        xaxis = list(title = "Universidades y Año", tickmode = 'array', tickvals = unique(data$Universidad_Año), ticktext = unique(data$Universidad_Año)),
        yaxis = list(title = "Puntaje"),
        barmode = 'group',
        legend = list(title = list(text = "Universidad"))
      )
  })
  
  # ---------------------------- Profesores ----------------------------
  
  output$evaluacion_profesores_plot <- renderPlotly({
    profesores %>%
      filter(ID_UNIVERSIDAD %in% as.numeric(universidades_ids())) %>%
      mutate(Universidad = names(universidades_dict)[match(as.character(ID_UNIVERSIDAD), universidades_dict)]) %>%
      ggplot(aes(x = EVALUACION_DOCENTE, fill = Universidad)) +
      geom_density(alpha = 0.5) +
      labs(x = "Evaluación", y = "Densidad") +
      theme_minimal() -> p
    ggplotly(p)
  })
  
  output$evaluacion_profesores_jefe_plot <- renderPlotly({
    profesores %>%
      filter(ID_UNIVERSIDAD %in% as.numeric(universidades_ids())) %>%
      mutate(Universidad = names(universidades_dict)[match(as.character(ID_UNIVERSIDAD), universidades_dict)]) %>%
      ggplot(aes(x = EVALUACION_DOCENTE_JEFE, fill = Universidad)) +
      geom_density(alpha = 0.5) +
      labs(x = "Evaluación", y = "Densidad") +
      theme_minimal() -> p
    ggplotly(p)
  })
  
  # ----------------------- Postulaciones ----------------------------
  
  output$postulaciones_plot <- renderPlotly({
    data <- postulaciones %>%
      filter(ID_UNIVERSIDAD %in% as.numeric(universidades_ids())) %>%
      mutate(Universidad = names(universidades_dict)[match(as.character(ID_UNIVERSIDAD), universidades_dict)])
    
    plot_ly(data = data, x = ~ORDEN_PREF, type = 'histogram', color = ~Universidad) %>%
      layout(
        xaxis = list(title = "Orden de Preferencia"),
        yaxis = list(title = "Frecuencia"),
        barmode = 'dodge'
      )
  })
  
  # ----------------------- Opiniones Estudiantiles -------------------
  
  output$opiniones_plot <- renderPlotly({
    data <- opiniones %>%
      filter(place_name %in% universidades_ids()) %>%
      mutate(anio = year(published_at_date),
             Universidad = names(universidades_dict)[match(place_name, universidades_dict)]) %>%
      group_by(Universidad) %>%
      arrange(anio) %>%
      mutate(media_movil = cummean(rating)) %>%
      ungroup() %>%
      group_by(Universidad, anio) %>%
      summarise(media_movil = mean(media_movil, na.rm = TRUE), .groups = "drop")
    
    plot_ly(data = data, x = ~anio, y = ~media_movil, type = 'scatter', mode = 'lines+markers', color = ~Universidad) %>%
      layout(
        xaxis = list(title = "Año"),
        yaxis = list(title = "Media Móvil del Rating")
      )
  })
  
  output$labels_medias_actuales <- renderUI({
    medias <- opiniones %>%
      filter(place_name %in% universidades_ids()) %>%
      mutate(anio = year(published_at_date)) %>%
      group_by(place_name) %>%
      arrange(anio) %>%
      mutate(media_movil = cummean(rating)) %>%
      filter(anio == max(anio)) %>%
      summarise(Media_Actual = round(mean(media_movil, na.rm = TRUE), 2)) %>%
      ungroup() %>%
      mutate(Universidad = names(universidades_dict)[match(place_name, universidades_dict)])
    
    valueBoxes <- lapply(1:nrow(medias), function(i) {
      valueBox(
        value = medias$Media_Actual[i],
        subtitle = medias$Universidad[i],
        icon = icon("star"),
        color = "green"
      )
    })
    
    fluidRow(valueBoxes)
  })
  
  # ----------------------- Análisis de Texto ------------------------
  
  output$top_words_plot <- renderPlotly({
    datos_filtrados <- opiniones %>%
      filter(place_name %in% universidades_ids(),
             rating %in% input$filtro_rating,
             !is.na(review_translated_text),
             review_translated_text != "")
    
    if (nrow(datos_filtrados) == 0) {
      return(NULL)
    }
    
    it <- itoken(datos_filtrados$review_translated_text,
                 preprocessor = tolower,
                 tokenizer = word_tokenizer,
                 progressbar = FALSE)
    
    vocab <- create_vocabulary(it,
                               stopwords = stopwords::stopwords("es"),
                               ngram = c(1L, 1L)) %>%
      prune_vocabulary(
        term_count_min = 2,
        doc_proportion_max = 0.95,
        vocab_term_max = Inf
      ) %>%
      filter(str_detect(term, "^[a-zA-Z]{4,}$"))
    
    vectorizer <- vocab_vectorizer(vocab)
    dtm <- create_dtm(it, vectorizer)
    bow_df <- as.data.frame(as.matrix(dtm))
    
    word_freq <- colSums(bow_df)
    word_freq_df <- data.frame(
      word = names(word_freq),
      freq = as.numeric(word_freq)
    )
    
    top_20_words <- word_freq_df %>%
      arrange(desc(freq)) %>%
      slice_head(n = 20)
    
    plot_ly(top_20_words,
            x = ~freq,
            y = ~reorder(word, freq),
            type = 'bar',
            orientation = 'h',
            text = ~freq,
            textposition = "outside",  # <- fuerza la posición del texto
            textangle = 0,             # <- ángulo horizontal fijo
            hoverinfo = 'text') %>%
      layout(
        xaxis = list(title = "Frecuencia"),
        yaxis = list(title = "Palabras"),
        margin = list(l = 100)  # Aumenta margen izquierdo si palabras son largas
      )
    
  })
  
  output$wordcloud_plot <- renderWordcloud2({
    datos_filtrados <- opiniones %>%
      filter(place_name %in% universidades_ids(),
             rating %in% input$filtro_rating,
             !is.na(review_translated_text),
             review_translated_text != "")
    
    if (nrow(datos_filtrados) == 0) {
      return(NULL)
    }
    
    it <- itoken(datos_filtrados$review_translated_text,
                 preprocessor = tolower,
                 tokenizer = word_tokenizer,
                 progressbar = FALSE)
    
    vocab <- create_vocabulary(it,
                               stopwords = stopwords::stopwords("es"),
                               ngram = c(1L, 1L)) %>%
      prune_vocabulary(term_count_min = 2,
                       doc_proportion_max = 0.95) %>%
      filter(str_detect(term, "^[a-zA-Z]{4,}$"))
    
    vectorizer <- vocab_vectorizer(vocab)
    dtm <- create_dtm(it, vectorizer)
    bow_df <- as.data.frame(as.matrix(dtm))
    
    word_freq <- colSums(bow_df)
    word_freq_df <- data.frame(
      word = names(word_freq),
      freq = as.numeric(word_freq)
    )
    
    top_20_words <- word_freq_df %>%
      arrange(desc(freq)) %>%
      slice_head(n = 20)
    
    wordcloud2(top_20_words, size = 1.2, color = 'random-dark', backgroundColor = "white")
  })
  
  
  
}

# Ejecutar la app
shinyApp(ui = ui, server = server)

