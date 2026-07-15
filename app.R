# 7-15-26 Update

# TODO: Edit gene table so Zfp gene names appear in search

library(shiny)
library(dplyr)
library(tidyr)
library(plotly)
library(ggplot2)
library(shinycssloaders)

# =======================================================
# Load datasets once at startup
# =======================================================

# Species-based clusters
df_label <- read.csv(
  "data/df_wide.csv",
  check.names = FALSE,
  stringsAsFactors = FALSE
)

df_label <- df_label %>%
  mutate(
    PlotGroup = case_when(
      Order == "Coelacanthiformes" ~ "Coelacanth",
      Order == "Anura" ~ "Amphibia",
      Class == "Reptilia" & Order != "Anura" ~ "Reptiles",
      Class == "Aves" ~ "Birds",
      Order == "Monotremata" ~ "Monotremes",
      Class == "Marsupial" ~ "Marsupials",
      Class == "Mammalia" & Order != "Primate" ~ "Eutheria",
      Order == "Primate" ~ "Primates",
      TRUE ~ NA_character_
    )
  )

df_label$PlotGroup <- factor(
  df_label$PlotGroup,
  levels = c(
    "Coelacanth",
    "Amphibia",
    "Reptiles",
    "Birds",
    "Monotremes",
    "Marsupials",
    "Eutheria",
    "Primates"
  )
)

df_label <- df_label %>%
  relocate(PlotGroup, .after = Species)


print("Printing head(df_label)")
print(head(df_label))

df_pairs <- read.csv(
  "data/gene_cluster_pairs.csv",
  check.names = FALSE,
  stringsAsFactors = FALSE
)

df_gnomAD <- read.csv(
  "data/gnomAD_pli_oe_KZFP_genes.csv",
  check.names = FALSE,
  stringsAsFactors = FALSE
)

df_human_to_mouse <- read.csv(
  "data/df_collapsed_human_to_mouse_genes.csv",
  check.names = FALSE,
  stringsAsFactors = FALSE
)

df_table <- read.csv(
  "data/df_collapsed_stringified.csv",
  check.names = FALSE,
  stringsAsFactors = FALSE
)

bool_cols2 <- 7:ncol(df_label)
df_label[bool_cols2] <- lapply(df_label[bool_cols2], function(x) x == "True")

# Shared setup
species_choices <- df_label %>%
  mutate(label = paste(Species, "—", CommonName)) %>%
  select(Species, label)

label_choices <- colnames(df_label)[7:ncol(df_label)]

# Attempt to add gene_choices using df_pairs
gene_choices <- sort(unique(df_pairs$Label), decreasing = TRUE)

# =======================================================
# UI
# =======================================================
ui = tagList(

  navbarPage(
    "KRAB-ZFP Conservation Viewer",
    tabPanel("View by Species",
             sidebarPanel(
               width=3,
               tags$h5("Data revisualized from Imbeault et al. (2017)."),
               tags$h5(
                 "DOI: ",
                 tags$a(
                   href = "https://doi.org/10.1038/nature21683",
                   target = "_blank",
                   "10.1038/nature21683"
                 )
               ),

               # Selectize input
               selectizeInput(
                 "selected_species",
                 "Select or type species name or common name:",
                 selected = "Mus musculus",
                 choices = setNames(species_choices$Species, species_choices$label),
                 multiple = FALSE,
                 options = list(
                   placeholder = 'Type species or common name...',
                   maxItems = 1
                 ),
               ),

               # Species image
               uiOutput("speciesImagePanel"),

               uiOutput("speciesInfoPanel"),
             ),

             mainPanel(

               tabsetPanel(
                 id = "tabs",
                 type = "tabs",

                 # ---------------------------------------------------
                 # TAB 1: Table
                 # ---------------------------------------------------
                 tabPanel(
                   title = "Conservation Plot",
                   # Add checkbox option for "Compact Plot" mode
                   checkboxInput("show_compact_clustFig", "Show condensed plot (uncheck to show full)", value = FALSE),
                   # include text legend here color: violetred4

                   tags$h5(
                             tags$span(
    style = "display:inline-block; width:12px; height:12px; background-color:#8B2252; margin-right:5px; vertical-align:top; border-radius:2px;"
                   ),
                   "Orthologues of KRAB-ZFPs (from the selected species) present among 191 vertebrate species"),

                   fluidRow(
                     column(
                       width = 12,
                       withSpinner(
                         plotlyOutput("combinedPlot", height="600")
                       )

                     )
                   )
                 ),

                 # ---------------------------------------------------
                 # TAB 2: View by Species Plot
                 # ---------------------------------------------------
                 tabPanel(
                   title = "Gene Table",

                   fluidRow(
                     column(
                       width = 12,
                       uiOutput("dynamicClusterTableUI")
                     )
                   )
                 ),
               ),
             )
    ),
    tabPanel("View by Gene",
             sidebarPanel(
               width=3,
               tags$h5("Data revisualized from Imbeault et al. (2017)."),
               tags$h5(
                 "DOI: ",
                 tags$a(
                   href = "https://doi.org/10.1038/nature21683",
                   target = "_blank",
                   "10.1038/nature21683"
                 )
               ),
               selectizeInput(
                 "selected_genes",
                 "Select one or more Gene(s):",
                 choices = gene_choices,
                 selected = "ZNF777",
                 multiple = TRUE,
                 options = list(
                   placeholder = "Type to search for genes...",
                   maxItems = NULL
                 ),
                 # width = '1500px'  # or '100%', '50%', etc.
               ),

             ),

             mainPanel(

               tabsetPanel(
                 id = "tabs",
                 type = "tabs",

                 # ---------------------------------------------------
                 # TAB 1: View by Label / Gene
                 # ---------------------------------------------------
                 tabPanel(
                   title = "Gene Cluster Plot",
                   tags$h5(
                     tags$span(
                       style = "display:inline-block; width:12px; height:12px; background-color:#8B2252; margin-right:5px; vertical-align:top; border-radius:2px;"
                     ),
                     "Orthologues of selected KRAB-ZFPs present among 191 vertebrate species"),
                   withSpinner(
                    plotlyOutput("labelPlot", height = "800px")
                   )
                 )
               ),
             )
           ),
  ),

  tags$style(HTML("
    #labelPlot .ytick text,
    #labelPlot .xtick text,
    #labelPlot .textpoint {
      user-select: text !important;
      -webkit-user-select: text !important;
      -moz-user-select: text !important;
      -ms-user-select: text !important;
      pointer-events: auto !important;
    }
  ")),

  tags$style(HTML("
    /* Make DT horizontal scrollbar thicker and more visible */
    div.dataTables_scrollBody::-webkit-scrollbar {
      height: 16px !important;        /* scrollbar thickness */
    }

    div.dataTables_scrollBody::-webkit-scrollbar-track {
      background: #e0e0e0 !important; /* track color */
      border-radius: 8px;
    }

    div.dataTables_scrollBody::-webkit-scrollbar-thumb {
      background-color: #888 !important;  /* thumb color */
      border-radius: 8px;
      border: 3px solid #e0e0e0;          /* gap padding */
    }

    div.dataTables_scrollBody::-webkit-scrollbar-thumb:hover {
      background-color: #555 !important;  /* darker on hover */
    }

    /* Firefox scrollbar */
    div.dataTables_scrollBody {
      scrollbar-width: thick;
      scrollbar-color: #888 #e0e0e0;
    }

    /* Make scrollbar always visible */
    div.dataTables_scrollBody {
      overflow-x: scroll !important;
    }
  ")),
)

# =======================================================
# SERVER
# =======================================================
server <- function(input, output, session) {

  # -------------------------------
  # Tab 1: View by Species
  # -------------------------------
  filtered_species_data <- reactive({
    req(input$selected_species)

    species_row <- df_label %>% filter(Species == input$selected_species)
    label_cols <- colnames(df_label)[7:ncol(df_label)]
    labels_true <- label_cols[as.logical(species_row[1, label_cols])]

    # If no labels are TRUE, return NULL
    if(length(labels_true) == 0) return(NULL)

    sub <- df_label %>%
      select(Species, PlotGroup, Order, Class, CommonName, timeFromHuman_MY, all_of(labels_true))

    df_long <- sub %>%
      pivot_longer(
        cols = all_of(labels_true),
        names_to = "Label",
        values_to = "present"
      )
    
    print("Printing head(df_long)")
    print(head(df_long))

    label_freq <- df_long %>%
      filter(present == TRUE) %>%
      count(Label, name = "Frequency_T") %>%
      arrange(desc(Frequency_T), Label)

    df_sorted <- df_long %>%
      arrange(PlotGroup, desc(timeFromHuman_MY), Species, Label)
    df_sorted$Label <- factor(df_sorted$Label, levels = rev(label_freq$Label), ordered = TRUE)
    df_sorted$Species <- factor(df_sorted$Species, levels = unique(df_sorted$Species), ordered = TRUE)
    
    df_sorted
  })

  output$speciesInfoPanel <- renderUI({
    df <- filtered_species_data()
    req(input$selected_species)

    species_row <- df[df$Species == input$selected_species, ]

    validate(
      need(nrow(species_row) > 0, paste("No labeled KZFP genes found for", input$selected_species))
    )

    div(
      # style = "width: 600px; max-width: 90%; margin: 0 auto;",  # fixed width + responsive max + centered
      tagList(
        # h5(HTML(paste("KZFP Orthologs for <i>", species_row$Species[1], "</i> — ", species_row$CommonName[1]))),
        h5(strong("Class:"), species_row$Class[1], strong("| Order:"), species_row$Order[1], strong("| Time from Human:"), species_row$timeFromHuman_MY[1], "million years")
      )
    )
  })

  output$speciesImagePanel <- renderUI({
    req(input$selected_species)
    tags$img(
      src = paste0(input$selected_species, ".png"),  # file inside www/
      style = "width: 100%; height: auto;",
      style = "display: block; margin: 10px auto;"
    )
  })

  # Dynamic UI for species table height
  output$dynamicClusterTableUI <- renderUI({
    table_height <- 700  # 20px per row, min 200px
    div(
      style = paste0("height:", table_height, "px; overflow-y:auto;"),
      DT::dataTableOutput("clusterTable", width = "100%")
    )
  })

  # Species-specific Gene Table
  output$clusterTable <- DT::renderDataTable({
    df_table <- df_table %>%
      mutate(
        gnomad_link = paste0(
          "https://gnomad.broadinstitute.org/gene/",
          `gene_id`,
          "?dataset=gnomad_r4"
        )
      )
    
    # Add mouse gene names
    df_table <- df_table %>%
      mutate(
        mouse_gene = df_human_to_mouse$Mouse_gene_name[
          match(gene, df_human_to_mouse$Human_gene_name)
        ]
      )

    df_table <- df_table %>%
      mutate(
        mgi_link = paste0(
          "https://www.informatics.jax.org/quicksearch/summary?queryType=exactPhrase&query=",
          `gene`,
          "&submit=Quick%0D%0ASearch"
        )
      )

    df_table <- df_table %>%
      mutate(
        impc_link = paste0(
          "https://www.mousephenotype.org/data/search?term=",
          `gene`
        )
      )

    df_table <- df_table %>%
      mutate(
        percent_conserved = num_species_w_cluster_associated_with_gene / 191
        )

    # --- Reorder/select columns ---
    df_display <- df_table %>%
      dplyr::select(
        `Gene` = gene,
        `Species with a KRAB-ZFP Cluster Associated with Gene` = num_species_w_cluster_associated_with_gene,
        `Percent Conserved - All Species` = percent_conserved,
        `Mouse Gene` = mouse_gene,
        `Gene ID` = gene_id,
        `pLI` = pLI,
        `o/e` = oe,
        `GnomAD Link` = gnomad_link,
        `MGI Link` = mgi_link,
        `IMPC Link` = impc_link,
        `Clusters Associated with Gene as defined by zinc finger array similarity in Imbeault 2017` = Cluster_str
      )

    df_display$`GnomAD Link` <- paste0(
      "<a href='", df_display$`GnomAD Link`,
      "' target='_blank'>View in gnomAD (", df_display$Gene, ")</a>"
    )

    df_display$`MGI Link` <- paste0(
      "<a href='", df_display$`MGI Link`,
      "' target='_blank'>Search for Mouse Orthologs in MGI (", df_display$Gene, ") </a>"
    )

    df_display$`IMPC Link` <- paste0(
      "<a href='", df_display$`IMPC Link`,
      "' target='_blank'>Search for Mouse Phenotypes in IMPC (", df_display$Gene, ")</a>"
    )

    df_display$`Percent Conserved - All Species` <-
      scales::percent(df_display$`Percent Conserved - All Species`, accuracy = 0.1)

    df_display$pLI <- sprintf("%.3f", df_display$pLI)
    df_display$`o/e` <- sprintf("%.3f", df_display$`o/e`)

    colnames(df_display) <- c(
      "<img src='gene_icon.jpg' height='20'> Gene",
      "Species with a KRAB-ZFP Cluster Associated with Gene",
      "Percent of Species with Cluster Ortholog",
      "Mouse Gene",
      "Gene ID",
      "<img src='gnomAD.svg' height='20'> pLI",
      "<img src='gnomAD.svg' height='20'> o/e",
      "<img src='gnomAD.svg' height='20'> Link",
      "<img src='mgi_logo.png' height='20'> Link",
      "<img src='impc_logo.svg' height='20'> Link",
      "Clusters Associated with Gene as defined by zinc finger array similarity in Imbeault 2017"
    )

    n_rows <- nrow(df_display)
    print(paste0("Rows of df_display: ",n_rows))

    df <- filtered_species_data()
    print("head(df):")
    print(head(df))

    test <- df %>%
      filter(present==TRUE) %>%
      pull(Label)

    labelsForSpecies <- unique(test)

    df_filtered <- df_display[
      sapply(strsplit(df_display$"Clusters Associated with Gene as defined by zinc finger array similarity in Imbeault 2017", ","), function(x) {
        any(as.integer(x) %in% labelsForSpecies)
      }),
    ]

    print(head(df_filtered))

    # --- Display as a datatable ---
    DT::datatable(
      df_filtered,
      escape = FALSE,
      rownames = FALSE,
      options = list(
        scrollY = TRUE,
        scrollX = TRUE,            
        pageLength = n_rows,
        autoWidth = TRUE,
        dom = 'fTip',
        order = list(list(1, 'desc'), list(0, 'asc')),
        columnDefs = list(
          # Gene
          list(className = 'dt-center', width = '80px',  targets = 0),
          # Species with a KRAB-ZFP Cluster Associated with Gene
          list(className = 'dt-center', width = '140px', targets = 1),
          # Percent Conserved - All Species
          list(className = 'dt-center', width = '110px', targets = 2),
          # Mouse Gene
          list(className = 'dt-center', width = '80px',  targets = 3),
          # Gene ID
          list(className = 'dt-center', width = '110px', targets = 4),
          # pLI
          list(className = 'dt-center', width = '60px',  targets = 5),
          # o/e
          list(className = 'dt-center', width = '60px',  targets = 6),
          # GnomAD Link
          list(className = 'dt-center', width = '140px', targets = 7),
          # MGI Link
          list(className = 'dt-center', width = '140px', targets = 8),
          # IMPC Link
          list(className = 'dt-center', width = '140px', targets = 9),
          # Clusters Associated with Gene
          list(className = 'dt-center', width = '260px', targets = 10)
        )
      )
    )
  })

  output$combinedPlot <- renderPlotly({
    compact_mode <- isTRUE(input$show_compact_clustFig)

    row_heights <- if (compact_mode) c(0.2, 0.8) else c(0.023, 0.977)

    df_pairs <- df_pairs %>%
      dplyr::rename(
        Gene = Label
      )

    df_pairs <- df_pairs %>%
      dplyr::rename(
        Label = `Cluster #`
      )

    df_pairs <- df_pairs %>%
      mutate(Label = as.character(Label))

    df <- filtered_species_data()
    req(df, input$selected_species)

    # Display message if no genes/clusters for this species
    validate(
      need(nrow(df) > 0, paste("No labeled KZFP genes found for", input$selected_species))
    )

    df$present_num <- as.numeric(df$present)

    # ---- consistent label order ----
    label_levels <- if (is.factor(df$Label)) levels(df$Label) else unique(df$Label)
    df$Label <- factor(df$Label, levels = label_levels, ordered = TRUE)

    # 1) Collapse all Gene values per Label into a single string
    df_pairs_collapsed <- df_pairs %>%
      dplyr::group_by(Label) %>%
      dplyr::summarise(
        Gene_all = paste(unique(Gene), collapse = ", "),
        .groups = "drop"
      )

    # 2) Join that onto your label_levels and build the text
    labels_df <- data.frame(Label = label_levels) %>%
      dplyr::left_join(
        df_pairs_collapsed,   # <- use collapsed table
        by = "Label"
      ) %>%
      dplyr::mutate(
        Label_text = dplyr::if_else(
          is.na(Gene_all),
          as.character(Label),    # fallback: show Label if no genes
          as.character(Gene_all)  # all Gene matches in one long string
        )
      )
    
    # Attempt to prep the df for correct plotting
    labels_long <- labels_df %>%
      mutate(Label_text = ifelse(is.na(Label_text), Label, Label_text)) %>%
      separate_rows(Label_text, sep = ",\\s*")
    
    labels_long <- labels_long %>%
      group_by(Label) %>%
      mutate(
        x_pos = (row_number() - 1)*7
        ) %>%   # 0,1,2,...
      ungroup()
    
    print("Printing head(labels_long):")
    print(head(labels_long))

    print("Printing head(labels_df):")
    print(head(labels_df))

    nrows_labels_df <- nrow(labels_df)

    plot_height  <- if (compact_mode) 500 else 1100*(nrows_labels_df**(1/3))

    fig1_text  <- if (compact_mode) "" else ~Label_text
    
    fig1 <- plot_ly(
      data = labels_long,
      x = ~x_pos,
      y = ~Label,
      type = "scatter",
      mode = "text",
      text = fig1_text,
      textposition = "middle right",
      hoverinfo = "text",
      cliponaxis = FALSE
    ) %>%
      layout(
        dragmode = "pan",
        xaxis = list(
          showgrid = FALSE,
          showticklabels = FALSE,
          zeroline = FALSE,
          title = "",
          range = c(-2, 10)   # adjust window
        ),
        yaxis = list(
          range = c(0, length(label_levels)),
          categoryorder = "array",
          categoryarray = label_levels,
          title=""
        )
      )

    fig2_axes  <- if (compact_mode) FALSE else TRUE
    species_row <- df[df$Species == input$selected_species, ]
    fig2_yTitle  <- if (compact_mode) paste0(species_row$CommonName[1]," KRAB-ZFPs") else ""
    fig2_anno <- if (compact_mode) "" else ""

    # -------- RIGHT PLOT: your existing heatmap --------
    fig2 <- plot_ly(
      data = df,
      x = ~Species,
      y = ~Label,
      z = ~present_num,
      type = "heatmap",
      colors = c("lightgrey", "violetred4"),
      opacity = 1,
      text = ~paste(
        "Species:", Species,
        "<br>Order:", Order,
        "<br>Class:", Class,
        "<br>Common Name:", CommonName,
        "<br>Label:", Label,
        "<br>Present:", present,
        "<br>Time from Human (MY):", timeFromHuman_MY
      ),
      hoverinfo = "text",
      showscale = FALSE
    ) %>%
      layout(
        xaxis = list(
          visible = fig2_axes,
          showgrid = FALSE,
          title = "Species",
          tickangle = 60,
          tickfont = list(size = 5),
          automargin = TRUE
        ),
        yaxis = list(
          range = c(0, length(label_levels) + 0),
          title = fig2_yTitle,
          tickfont = list(size = 10),
          showticklabels = fig2_axes,
          ticks="",
          showgrid = FALSE,
          automargin = TRUE,
          categoryorder = "array",
          categoryarray = label_levels
        ),
        margin = list(l = 0, r = 0, b = 0, t = 0),
        annotations = list(
          list(
            text = fig2_anno,
            x = 0,              # far left
            y = 1,              # very top of plotting area
            xref = "paper",
            yref = "paper",
            showarrow = FALSE,
            textangle = 0,      # horizontal
            xanchor = "left",
            yanchor = "bottom"  # anchor text ABOVE this point
          )
        )
      )

    fig3_visible  <- if (compact_mode) FALSE else TRUE
    fig3 <- plot_ly(
      visible = fig3_visible,
      type = "scatter",
      mode = "text",
      x = 0, y = 0,
      text = "<b>KRAB-ZFP<br>orthologue<br>clusters<br>defined by<br>Imbeault<br>2017</b><br><br>◀ Scroll Genes ▶︎", ###############
      textposition = "middle center",
      hoverinfo = "none"
    ) %>%
      layout(
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE),
        ticks="",
        margin = list(t = 0, b = 0, l = 0, r = 0),
        paper_bgcolor = "rgba(255,255,255,1)",
        plot_bgcolor  = "rgba(255,255,255,1)"
      )

    fig4 <- plot_ly() %>%
      layout(
        images = list(
          list(
            source = base64enc::dataURI(file = "www/kzfp_phylogeny.png"),
            xref = "paper", yref = "paper",
            x = 0, y = 1,
            sizex = 1, sizey = 1,
            xanchor = "left", yanchor = "top"
          )
        ),
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE),
        margin = list(t = 0, b = 0, l = 0, r = 0)
      )

    # -------- COMBINE SIDE BY SIDE, SHARE Y --------

    subplot(
      fig3,
      fig4,
      fig1,
      fig2,
      nrows  = 2,
      shareY = TRUE,
      heights = row_heights,
      widths = c(0.2, 0.8),
      margin = 0.00
    ) %>%
  
      config(
        displayModeBar = TRUE,
        displaylogo = FALSE,
        
        modeBarButtonsToRemove = c(
          'zoomIn',
          'zoomOut',
          'autoScale',
          'select',
          'lasso2d',
          'hoverClosestCartesian',
          'hoverCompareCartesian'
          )
        )%>%
      layout(
        height = plot_height,
        yaxis=list(
          title=""
        ),

        plot_bgcolor = "#fff",
        showlegend = FALSE
      )
  })

  # -------------------------------
  # Tab 2: View by Label / Gene
  # -------------------------------

  # Attempt to clear the issue
  filtered_label_data <- reactive({
    req(input$selected_genes)

    cat("\n---- filtered_label_data START ----\n")

    selected_gene_list <- input$selected_genes
    cat("selected_gene_list:\n")
    print(selected_gene_list)


    # 1. Get cluster numbers for the selected genes
    clusters <- unique(df_pairs$`Cluster #`[df_pairs$Label %in% selected_gene_list])
    cat("\nClusters found:\n")
    print(clusters)

    # 2. Convert clusters to character column names
    cluster_cols <- as.character(clusters)

    cat("\nCluster column names to extract from df_label:\n")
    print(cluster_cols)

    # 3. Check which exist
    valid_cluster_cols <- intersect(cluster_cols, colnames(df_label))

    cat("\nValid cluster columns in df_label:\n")
    print(valid_cluster_cols)

    req(length(valid_cluster_cols) > 0)

    # 4. Subset df_label by cluster columns
    sub <- df_label %>%
      dplyr::select(
        Species, PlotGroup, Order, Class, CommonName, timeFromHuman_MY,
        dplyr::all_of(valid_cluster_cols)
      )
    
    print("Printing head(sub)")
    print(head(sub))

    cat("\nSub dataframe columns:\n")
    print(colnames(sub))

    # 5. Pivot longer
    df_long <- sub %>%
      tidyr::pivot_longer(
        cols = dplyr::all_of(valid_cluster_cols),
        names_to = "Cluster",
        values_to = "present"
      )

    cat("\npivot_longer result preview:\n")
    print(head(df_long))

    # 6. Frequency table
    cluster_freq <- df_long %>%
      dplyr::filter(present == TRUE) %>%
      dplyr::count(Cluster, name = "Frequency_T") %>%
      dplyr::arrange(dplyr::desc(Frequency_T), Cluster)

    cat("\nCluster frequency table:\n")
    print(cluster_freq)

    # 7. Sort df
    # df_sorted <- df_long %>%
    #   arrange(PlotGroup, desc(timeFromHuman_MY), Species, Label)
    
    df_sorted <- df_long %>%
      arrange(PlotGroup,desc(timeFromHuman_MY), Species, Cluster)

    cat("\nSorted df preview:\n")
    print(head(df_sorted))

    # 8. Factor ordering
    df_sorted$Cluster <- factor(df_sorted$Cluster, levels = rev(cluster_freq$Cluster), ordered = TRUE)
    df_sorted$Species <- factor(df_sorted$Species, levels = unique(df_sorted$Species), ordered = TRUE)

    cat("\n---- filtered_label_data END ----\n")

    df_sorted
  })

  output$labelPlot <- renderPlotly({
    # attempt to adapt combinedPlot here
    # Start of previous code for labelPlot
    df <- filtered_label_data()
    req(df)
    # df$present_num <- as.numeric(df$present)

    df_pairs <- df_pairs %>%
      dplyr::rename(
        Gene = Label
      )

    df_pairs <- df_pairs %>%
      dplyr::rename(
        Label = `Cluster #`
      )

    df <- df %>%
      dplyr::rename(
        Label = Cluster
      )

    df_pairs <- df_pairs %>%
      mutate(Label = as.character(Label))
    #
    # df <- filtered_species_data()
    # req(df, input$selected_species)

    # Display message if no genes/clusters for this species
    validate(
      need(nrow(df) > 0, paste("No labeled KZFP genes found for", input$selected_species))
    )

    df$present_num <- as.numeric(df$present)

    # ---- consistent label order ----
    label_levels <- if (is.factor(df$Label)) levels(df$Label) else unique(df$Label)
    df$Label <- factor(df$Label, levels = label_levels, ordered = TRUE)

    # 1) Collapse all Gene values per Label into a single string
    df_pairs_collapsed <- df_pairs %>%
      dplyr::group_by(Label) %>%
      dplyr::summarise(
        Gene_all = paste(unique(Gene), collapse = ", "),
        .groups = "drop"
      )

    # 2) Join that onto your label_levels and build the text
    labels_df <- data.frame(Label = label_levels) %>%
      dplyr::left_join(
        df_pairs_collapsed,   # <- use collapsed table
        by = "Label"
      ) %>%
      dplyr::mutate(
        Label_text = dplyr::if_else(
          is.na(Gene_all),
          as.character(Label),    # fallback: show Label if no genes
          as.character(Gene_all)  # all Gene matches in one long string
        )
      )
    
    labels_long <- labels_df %>%
      mutate(Label_text = ifelse(is.na(Label_text), Label, Label_text)) %>%
      separate_rows(Label_text, sep = ",\\s*")
    
    labels_long <- labels_long %>%
      group_by(Label) %>%
      mutate(
        x_pos = (row_number() - 1)*7
      ) %>%   # 0,1,2,...
      ungroup()
    
    print("Printing head(labels_long):")
    print(head(labels_long))
    
    fig1 <- plot_ly(
      data = labels_long,
      x = ~x_pos,
      y = ~Label,
      type = "scatter",
      mode = "text",
      text = ~Label_text,
      textposition = "middle right",
      hoverinfo = "text",
      cliponaxis = FALSE
    ) %>%
      layout(
        dragmode = "pan",
        xaxis = list(
          showgrid = FALSE,
          showticklabels = FALSE,
          zeroline = FALSE,
          title = "",
          range = c(-2, 10)   # adjust window
        ),
        yaxis = list(
          range = c(0, length(label_levels)),
          categoryorder = "array",
          categoryarray = label_levels,
          title=""
        )
      )
 
    # -------- RIGHT PLOT: your existing heatmap --------
    fig2 <- plot_ly(
      data = df,
      x = ~Species,
      y = ~Label,
      z = ~present_num,
      type = "heatmap",
      colors = c("lightgrey", "violetred4"),
      opacity = 1,
      text = ~paste(
        "Species:", Species,
        "<br>Order:", Order,
        "<br>Class:", Class,
        "<br>Common Name:", CommonName,
        "<br>Label:", Label,
        "<br>Present:", present,
        "<br>Time from Human (MY):", timeFromHuman_MY
      ),
      hoverinfo = "text",
      showscale = FALSE
    ) %>%
      layout(
        xaxis = list(
          title = "Species",
          tickangle = 60,
          tickfont = list(size = 5),
          showgrid = FALSE,
          automargin = TRUE
        ),
        yaxis = list(
          range = c(0, length(label_levels) + 0),
          title = "",
          tickfont = list(size = 10),
          automargin = TRUE,
          showgrid = FALSE,
          categoryorder = "array",
          categoryarray = label_levels
        ),
        margin = list(l = 0, r = 0, b = 0, t = 0)
      )

    fig3 <- plot_ly(
      visible = TRUE,
      type = "scatter",
      mode = "text",
      x = 0, y = 0,
      text = "<b>KRAB-ZFP<br>orthologue<br>clusters<br>defined by<br>Imbeault<br>2017</b><br><br>◀ Scroll Genes ▶︎", ###############
      textposition = "middle center",
      hoverinfo = "none"
    ) %>%
      layout(
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE),
        ticks="",
        margin = list(t = 0, b = 0, l = 0, r = 0),
        paper_bgcolor = "rgba(255,255,255,1)",
        plot_bgcolor  = "rgba(255,255,255,1)"
      )

    fig4 <- plot_ly() %>%
      layout(
        images = list(
          list(
            source = base64enc::dataURI(file = "www/kzfp_phylogeny.png"),
            xref = "paper", yref = "paper",
            x = 0, y = 1,
            sizex = 1, sizey = 1,
            xanchor = "left", yanchor = "top"
          )
        ),
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE),
        margin = list(t = 0, b = 0, l = 0, r = 0)
      )

    # -------- COMBINE SIDE BY SIDE, SHARE Y --------
    subplot(
      fig3,
      fig4,
      fig1,
      fig2,
      nrows  = 2,
      shareY = TRUE,          # y-axes aligned
      heights = c(0.2, 0.8),   
      widths = c(0.2, 0.8), 
      margin = 0.00
      # config(displayModeBar = TRUE)

    ) %>%
      config(displayModeBar = TRUE)%>%
      layout(

        plot_bgcolor = "#fff",
        showlegend = FALSE
      )
  })
}

# =======================================================
# Run App
# =======================================================
shinyApp(ui, server)