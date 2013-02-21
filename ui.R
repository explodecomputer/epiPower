library(shiny)

shinyUI(
	pageWithSidebar(

		headerPanel("Calculate power for 2D epistatic scans"),

		sidebarPanel(
			numericInput(
				inputId = "pvar", 
				label = "Proportion of variance explained:", 
				value=0.01, min=0.00001, max=0.99999),

			numericInput(
				inputId = "nsnp", 
				label = "Number of SNPs in SNP chip:", 
				value=500000, min=50000),

			numericInput(
				inputId = "grp", 
				label = "Number of groups (e.g. A+D+I = 9):", 
				value=9, min=2)
		),

		mainPanel(
			tableOutput("values")
		)
	)
)