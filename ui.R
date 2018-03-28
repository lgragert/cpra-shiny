library(shiny)

shinyUI(fluidPage(
  fluidRow(
    h2("CPRA Allele (Test Version)", align = "center"),
    hr()
  ), #row 
  fluidRow(
    column(6, offset=3,
      h5("INSTRUCTIONS", align = "center"),
      h6("Input should take the form of:", align = "left"),
      h6("1. UNOS defined antigens  (e.g. A1, B7, C1, DQ1, DR1)", align = "left"),
      h6("2. NMDP defined alleles (e.g A*01:01, DRB1*01:01)", align = "left"),
      h6("Notes:", align = "left"),
      h6("-Calculator uses UNOS equivalencies prior to 09.12.17 update", align = "left"),
      h6("-Bw4/Bw6 should be entered as B4 and B6, respectively", align = "left"),
      hr()
    ) #col
  ), #row
  fluidRow(
    column(9,
      textInput("string", label = h5("HLA input"), value = "", width = "75%"),
      h6("Enter HLA data as a string separated by space, comma, or semicolon", align = "left")
    ), #col
    column(3,
      h1("   "),   
      actionButton("run", label = "Run"),
      actionButton("reset", label = "Reset")      
    ) #col
  ), #row
  fluidRow(
    hr()
  ), #row
  fluidRow(
    column(6,
      h5("CPRA UNOS"),
      verbatimTextOutput("o.unos")
    ), #col
    column(6,
      h5("CPRA NMDP"),
      verbatimTextOutput("o.nmdp")           
    ) #col 
  ), #row
  fluidRow(
    hr()
  ), #row
  fluidRow(
    column(6,
      h5("Input Antigens & Equivalents"),
      DT::dataTableOutput("agtable")
    ), #col
    column(6,
      h5("Input Alleles"),
      DT::dataTableOutput("alltable")
    ) #col 
  ) #row
)) #Shiny

