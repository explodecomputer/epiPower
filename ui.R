library(shiny)

shinyUI(bootstrapPage(

  sidebarPanel(
    numericInput(
      inputID = "pvar", 
      label = "Proportion of variance explained:", 
      0.01, min=0.00001, max=0.99999),

    numericInput(
      inputID = "nsnp", 
      label = "Number of SNPs in SNP chip:", 
      500000, min=50000),

    numericInput(
      inputID = "grp", 
      label = "Number of groups (e.g. A+D+I = 9):", 
      9, min=2),

    tableOutput("values")


))