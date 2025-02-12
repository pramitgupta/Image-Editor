# Install required packages if not already installed
# install.packages(c("shiny", "magick", "plotly", "base64enc", "colourpicker"))

if(!require("shiny")) {install.packages("shiny")}
if(!require("shinyjs")) {install.packages("shinyjs")}
if(!require("magick")) {install.packages("magick")}
if(!require("plotly")) {install.packages("plotly")}
if(!require("base64enc")) {install.packages("base64enc")}
if(!require("colourpicker")) {install.packages("colourpicker")}

library(shiny)
library(magick)
library(plotly)
library(base64enc)
library(colourpicker)

ui <- fluidPage(
  titlePanel("Image Editor"),
  
  sidebarLayout(
    sidebarPanel(
      # File upload
      fileInput("imageFile", "Choose an Image",
                accept = c('image/png', 'image/jpeg', 'image/jpg')),
      
      # Image filters
      h4("Image Filters"),
      fluidRow(
        column(6, actionButton("reset", "Reset", class = "btn-block btn-default")),
        column(6, actionButton("grayscale", "Grayscale", class = "btn-block btn-info"))
      ),
      br(),
      fluidRow(
        column(6, actionButton("sepia", "Sepia", class = "btn-block btn-warning")),
        column(6, actionButton("negate", "Negative", class = "btn-block btn-danger"))
      ),
      br(),
      fluidRow(
        column(6, actionButton("coolTone", "Cool Tone", class = "btn-block btn-primary")),
        column(6, actionButton("warmTone", "Warm Tone", class = "btn-block btn-warning"))
      ),
      br(),
      fluidRow(
        column(6, actionButton("vintage", "Vintage", class = "btn-block btn-info")),
        column(6, actionButton("dramatic", "Dramatic", class = "btn-block btn-danger"))
      ),
      br(),
      fluidRow(
        column(6, actionButton("edgeDetect", "Modern Art", class = "btn-block btn-primary")),
        column(6, actionButton("rotate", "Rotate 90°", class = "btn-block btn-info"))
      ),
      br(),
      actionButton("crop", "Crop (Centered)", class = "btn-block btn-primary"),
      
      hr(),
      
      # Adjustments
      h4("Fine-tune Adjustments"),
      sliderInput("brightness", "Brightness",
                  min = -100, max = 100, value = 0),
      sliderInput("contrast", "Contrast",
                  min = -100, max = 100, value = 0),
      sliderInput("blur", "Blur",
                  min = 0, max = 10, value = 0),
      sliderInput("saturation", "Saturation",
                  min = 0, max = 200, value = 100),
      
      # Image size controls
      h4("Image Size"),
      numericInput("width", "Width (px)", value = 400, min = 100, max = 1000),
      numericInput("height", "Height (px)", value = 400, min = 100, max = 1000),
      
      hr(),
      
      # Text overlay
      h4("Add Text Overlay"),
      textInput("overlayText", "Text", value = "Sample Text"),
      colourInput("textColor", "Text Color", value = "#FFFFFF"),
      sliderInput("textSize", "Text Size", min = 10, max = 100, value = 20),
      numericInput("textX", "X Position", value = 10, min = 0),
      numericInput("textY", "Y Position", value = 10, min = 0),
      
      hr(),
      
      # Download button
      downloadButton("downloadImage", "Download Edited Image")
    ),
    
    mainPanel(
      imageOutput("imageContainer", width = "100%")
    )
  )
)

server <- function(input, output, session) {
  values <- reactiveValues(
    img = NULL,
    activeFilter = "none"
  )
  
  observeEvent(input$reset, {
    values$activeFilter <- "none"
  })
  
  # Filter observers
  observeEvent(input$grayscale, { values$activeFilter <- "grayscale" })
  observeEvent(input$sepia, { values$activeFilter <- "sepia" })
  observeEvent(input$negate, { values$activeFilter <- "negate" })
  observeEvent(input$coolTone, { values$activeFilter <- "coolTone" })
  observeEvent(input$warmTone, { values$activeFilter <- "warmTone" })
  observeEvent(input$vintage, { values$activeFilter <- "vintage" })
  observeEvent(input$dramatic, { values$activeFilter <- "dramatic" })
  observeEvent(input$edgeDetect, { values$activeFilter <- "edgeDetect" })
  observeEvent(input$rotate, {
    req(values$img)
    values$img <- image_rotate(values$img, 90)
  })
  observeEvent(input$crop, {
    req(values$img)
    values$img <- image_crop(values$img, "300x300")  # Adjust crop dimensions as needed
  })
  
  processImage <- function(img) {
    if (is.null(img)) return(NULL)
    processed <- img
    
    # Apply filters
    switch(values$activeFilter,
           "grayscale" = processed <- image_convert(processed, colorspace = "gray"),
           "sepia" = {
             processed <- image_convert(processed, colorspace = "gray")
             processed <- image_modulate(processed, brightness = 108, saturation = 0)
             processed <- image_colorize(processed, opacity = 35, color = "sienna")
           },
           "negate" = processed <- image_negate(processed),
           "coolTone" = {
             processed <- image_modulate(processed, brightness = 100, saturation = 120)
             processed <- image_colorize(processed, opacity = 20, color = "darkblue")
           },
           "warmTone" = {
             processed <- image_modulate(processed, brightness = 105, saturation = 120)
             processed <- image_colorize(processed, opacity = 20, color = "orange")
           },
           "vintage" = {
             processed <- image_modulate(processed, brightness = 100, saturation = 60)
             processed <- image_colorize(processed, opacity = 30, color = "wheat")
             processed <- image_contrast(processed, 1.2)
           },
           "dramatic" = {
             processed <- image_modulate(processed, brightness = 90, saturation = 150)
             processed <- image_contrast(processed, 1.5)
             processed <- image_colorize(processed, opacity = 20, color = "dimgray")
           },
           "edgeDetect" = processed <- image_edge(processed, radius = 1)
    )
    
    # Apply adjustments
    processed <- image_modulate(processed, brightness = 100 + input$brightness, saturation = input$saturation)
    if (input$contrast != 0) processed <- image_contrast(processed, input$contrast / 100 + 1)
    if (input$blur > 0) processed <- image_blur(processed, sigma = input$blur)
    processed <- image_scale(processed, paste0(input$width, "x", input$height, "!"))
    
    # Add text overlay
    if (!is.null(input$overlayText) && input$overlayText != "") {
      processed <- image_annotate(processed,
                                  text = input$overlayText,
                                  color = input$textColor,
                                  size = input$textSize,
                                  location = paste0("+", input$textX, "+", input$textY))
    }
    
    return(processed)
  }
  
  observeEvent(input$imageFile, {
    req(input$imageFile)
    values$img <- image_read(input$imageFile$datapath)
    values$activeFilter <- "none"
  })
  
  output$imageContainer <- renderImage({
    req(values$img)
    processed_img <- processImage(values$img)
    outfile <- tempfile(fileext = '.png')
    image_write(processed_img, outfile)
    list(src = outfile, contentType = "image/png", alt = "Processed Image")
  }, deleteFile = TRUE)
  
  output$downloadImage <- downloadHandler(
    filename = function() { paste0("edited_image_", Sys.Date(), ".png") },
    content = function(file) {
      req(values$img)
      processed_img <- processImage(values$img)
      image_write(processed_img, file)
    }
  )
}

shinyApp(ui = ui, server = server)
