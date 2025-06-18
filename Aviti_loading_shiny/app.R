library(shiny)
library(ggplot2)
library(png)
library(zoo)
library(DT)
options(shiny.maxRequestSize = 100 * 1024^2)

ui <- fluidPage(
  titlePanel("Overlayed RGB Histogram and Summary from Aviti loading PNG Images"),
  sidebarLayout(
    sidebarPanel(
      fileInput("pngs", "Upload PNG files", multiple = TRUE, accept = ".png"),
      uiOutput("nameInputs"),
      numericInput("cutoff1", "Cutoff 1 (for % below)", value = 10, min = 0, max = 255),
      numericInput("cutoff2", "Cutoff 2 (for % above)", value = 50, min = 0, max = 255),
      numericInput("smooth_steps", "Smoothing steps (before/after X)", value = 0, min = 0, max = 40, step = 1),
      uiOutput("plotSelect"),
      actionButton("go", "Update Plot")
    ),
    mainPanel(
      plotOutput("histPlot", height = "600px"),
      downloadButton("downloadPlot", "Download plot as PDF"),
      DTOutput("summaryTable")
    )
  )
)

server <- function(input, output, session) {
  output$nameInputs <- renderUI({
    req(input$pngs)
    lapply(seq_len(nrow(input$pngs)), function(i) {
      textInput(
        paste0("name", i),
        label = paste0(i, ". ", input$pngs$name[i]),
        value = input$pngs$name[i]
      )
    })
  })
  
  output$plotSelect <- renderUI({
    req(input$pngs)
    checkboxGroupInput("to_plot", "Select datasets to plot", 
                       choices = seq_len(nrow(input$pngs)),
                       selected = seq_len(nrow(input$pngs)),
                       inline = TRUE,
                       choiceNames = input$pngs$name,
                       choiceValues = seq_len(nrow(input$pngs)))
  })
  
  processed <- eventReactive(input$go, {
    req(input$pngs)
    files <- input$pngs$datapath
    file_names <- input$pngs$name
    file_indices <- seq_len(nrow(input$pngs))
    user_labels <- sapply(file_indices, function(i) {
      if (is.null(input[[paste0("name", i)]])) input$pngs$name[i] else input[[paste0("name", i)]]
    })
    to_plot <- as.numeric(file_indices %in% as.numeric(input$to_plot))
    files_sel <- files[as.logical(to_plot)]
    file_names_sel <- file_names[as.logical(to_plot)]
    file_indices_sel <- file_indices[as.logical(to_plot)]
    user_labels_sel <- user_labels[as.logical(to_plot)]
    cutoff1 <- input$cutoff1
    cutoff2 <- input$cutoff2
    smooth_steps <- input$smooth_steps
    
    get_rgb_hist <- function(img_path, label, file_name, file_index, cutoff1, cutoff2, smooth_steps) {
      img <- readPNG(img_path)
      if (dim(img)[3] == 4) img <- img[,,1:3]
      if (max(img) <= 1) img <- img * 255
      all_rgb_vals <- c(as.integer(img[,,1]), as.integer(img[,,2]), as.integer(img[,,3]))
      df <- as.data.frame(table(factor(all_rgb_vals, levels = 0:255)))
      colnames(df) <- c("RGB_value", "Pixel_count")
      df$RGB_value <- as.integer(as.character(df$RGB_value))
      df$Pixel_count <- as.integer(df$Pixel_count)
      df$Image <- label
      df$FileName <- file_name
      df$FileIndex <- file_index
      total <- sum(df$Pixel_count)
      pct_low <- sum(df$Pixel_count[df$RGB_value < cutoff1]) / total * 100
      pct_high <- sum(df$Pixel_count[df$RGB_value > cutoff2]) / total * 100
      cumulative <- cumsum(df$Pixel_count)
      idx <- which(cumulative >= total/2)[1]
      n50_value <- df$RGB_value[idx]
      window_size <- 2 * smooth_steps + 1
      df$Smoothed <- zoo::rollmean(df$Pixel_count, k = window_size, fill = NA, align = "center")
      df$Smoothed[is.na(df$Smoothed)] <- df$Pixel_count[is.na(df$Smoothed)]
      list(hist = df, pct_low = pct_low, pct_high = pct_high, n50 = n50_value, total = total)
    }
    
    hist_list <- mapply(
      get_rgb_hist,
      files_sel,
      user_labels_sel,
      file_names_sel,
      file_indices_sel,
      MoreArgs = list(
        cutoff1 = cutoff1,
        cutoff2 = cutoff2,
        smooth_steps = smooth_steps
      ),
      SIMPLIFY = FALSE
    )
    hist_all <- do.call(rbind, lapply(hist_list, function(x) x$hist))
    summary_df <- data.frame(
      FileIndex = file_indices_sel,
      FileName = file_names_sel,
      Percent_below_cutoff1 = sapply(hist_list, function(x) x$pct_low),
      Percent_above_cutoff2 = sapply(hist_list, function(x) x$pct_high),
      N50 = sapply(hist_list, function(x) x$n50),
      Total_pixels = sapply(hist_list, function(x) x$total)
    )
    list(hist_all = hist_all, summary_df = summary_df)
  })
  
  output$histPlot <- renderPlot({
    req(processed())
    hist_all <- processed()$hist_all
    # Create a factor from FileIndex for unique legend entries
    hist_all$IndexLabel <- factor(paste0(hist_all$FileIndex, ": ", hist_all$Image))
    ggplot(hist_all, aes(x = RGB_value, y = Smoothed, color = IndexLabel)) +
      geom_point(size = 1.5, alpha = 0.8) +
      labs(
        title = sprintf("Overlayed Smoothed Histogram (window=%d)", 2*input$smooth_steps+1),
        x = "RGB Decimal Value (0-255)",
        y = "Smoothed Pixel Count"
      ) +
      scale_color_brewer(
        name = "Files",
        palette = "Dark2"
      ) +
      theme_bw() +
      guides(color = guide_legend(
        override.aes = list(size = 4),
        ncol = 1,
        byrow = TRUE,
        keyheight = unit(1.2, "lines"),
        label.position = "right"
      )) +
      theme(
        legend.position = c(0.95, 0.95),
        legend.justification = c(1, 1),
        legend.direction = "vertical",
        legend.title = element_text(face = "bold", color = "darkblue", size = 16),
        legend.text = element_text(face = "bold", color = "darkblue", size = 14),
        legend.spacing.y = unit(0.8, "lines"),
        legend.background = element_blank(),
        legend.key = element_rect(fill = "white", color = NA),
        legend.key.size = unit(1.8, "lines"),
        plot.background = element_rect(fill = "white"),
        legend.box.margin = margin(0, 0, 0, 0, "pt"),
        legend.margin = margin(0, 5, 0, 5, "pt")
      )
  })
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste0("rgb_histogram_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      hist_all <- processed()$hist_all
      hist_all$IndexLabel <- factor(paste0(hist_all$FileIndex, ": ", hist_all$Image))
      p <- ggplot(hist_all, aes(x = RGB_value, y = Smoothed, color = IndexLabel)) +
        geom_point(size = 1.5, alpha = 0.8) +
        labs(
          title = sprintf("Overlayed Smoothed Histogram (window=%d)", 2*input$smooth_steps+1),
          x = "RGB Decimal Value (0-255)",
          y = "Smoothed Pixel Count"
        ) +
        scale_color_brewer(
          name = "Files",
          palette = "Dark2"
        ) +
        theme_bw() +
        guides(color = guide_legend(
          override.aes = list(size = 4),
          ncol = 1,
          byrow = TRUE,
          keyheight = unit(1.2, "lines"),
          label.position = "right"
        )) +
        theme(
          legend.position = c(0.95, 0.95),
          legend.justification = c(1, 1),
          legend.direction = "vertical",
          legend.title = element_text(face = "bold", color = "darkblue", size = 16),
          legend.text = element_text(face = "bold", color = "darkblue", size = 14),
          legend.spacing.y = unit(0.8, "lines"),
          legend.background = element_blank(),
          legend.key = element_rect(fill = "white", color = NA),
          legend.key.size = unit(1.8, "lines"),
          plot.background = element_rect(fill = "white"),
          legend.box.margin = margin(0, 0, 0, 0, "pt"),
          legend.margin = margin(0, 5, 0, 5, "pt")
        )
      ggsave(file, p, device = "pdf", width = 10, height = 7)
    }
  )
  
  output$summaryTable <- renderDT({
    req(processed())
    summary_df <- processed()$summary_df
    cols_to_round <- c("Percent_below_cutoff1", "Percent_above_cutoff2", "N50")
    summary_df[cols_to_round] <- lapply(summary_df[cols_to_round], function(x) round(x, 1))
    summary_df <- subset(summary_df, select = -Total_pixels)  # Remove the column here
    datatable(summary_df, rownames = FALSE, options = list(pageLength = 10))
  })
  
}

shinyApp(ui, server)
