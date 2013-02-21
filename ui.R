library(shiny)

shinyUI(
	pageWithSidebar(

		headerPanel("Calculate power for 2D epistatic scans"),

		sidebarPanel(

			numericInput(
				inputId = "nsnp", 
				label = "Number of SNPs in SNP chip:", 
				value=500000, min=50000),

			h5("For threshold calculation please supply:"),

			numericInput(
				inputId = "nid", 
				label = "Sample size", 
				value=1000, min=1),

			h5("or for sample size calculation please supply:"),

			numericInput(
				inputId = "pvar", 
				label = "Proportion of variance explained:", 
				value=0.01, min=0.00001, max=0.99999),

			numericInput(
				inputId = "grp", 
				label = "Number of parameters (e.g. A+D+I=9):", 
				value=9, min=1),

			submitButton("Calculate")			

		),

		mainPanel(
			tabsetPanel(
				tabPanel("Threshold calculator", 
					tableOutput("threshold")
				),

				tabPanel("Sample sizes", 
					tableOutput("values")
				)
			)
		)
	)
)