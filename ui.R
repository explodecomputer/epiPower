library(shiny)

shinyUI(
	pageWithSidebar(

		headerPanel("Calculate thresholds and power for 2D epistatic scans"),

		sidebarPanel(

			h5("Number of SNPs"),
			helpText("This is for human studies using common SNPs, the minimum number of SNPs should be 300,000"),
			h5("Sample size"),
			helpText("How many genotyped and phenotyped individuals are in the study"),
			h5("Number of parameters"),
			helpText("This relates to the model complexity. A standard GWAS has 2 parameters (the additive effect and the mean). The joint effect of two SNPs can have up to 9 parameters (A1 + A2 + D1 + D2 + A1A2 + A1D2 + A2D1 + D1D2 + mean)."),
			h5("Effect size"),
			helpText("This is the proportion of the phenotypic variance explained by the SNP pair"),
			h5("Power"),
			helpText("The statistical power is the probability that the test will reject the null hypothesis when it is false. So")


		),

		mainPanel(
			tabsetPanel(
				tabPanel("Threshold calculator", 

					wellPanel(
						h3("Inputs"),

						numericInput(
							inputId = "nsnp1", 
							label = "Number of SNPs in SNP chip:", 
							value=1000000, min=300000),

						numericInput(
							inputId = "nid1", 
							label = "Sample size (required for permutation-based threshold)", 
							value=1000, min=10)
					),

					wellPanel(
						h3("Outputs"),

						helpText("These are the estimated thresholds for experiment false discovery rate of 0.05:"),

						tableOutput("threshold"),

						h5("Note:"),
						helpText("Bonferroni correction is considered overly stringent because it assumes independence between tests."),
						helpText("The permutation threshold assumes all SNPs are common (MAF > 0.01)")
					)
				),

				tabPanel("Sample size calculator", 

					helpText("This section calculates the minimum sample size required to get significant results for specified SNP density, effect size, statistical power, and model complexity"),

					wellPanel(
						h3("Inputs"),

						numericInput(
							inputId = "nsnp2", 
							label = "Number of SNPs in SNP chip:", 
							value=1000000, min=300000),
						numericInput(
							inputId = "pvar2", 
							label = "Proportion of variance explained:", 
							value=0.01, min=0.00001, max=0.99999),

						numericInput(
							inputId = "grp2", 
							label = "Number of parameters (e.g. A+D+I=9):", 
							value=9, min=1),

						sliderInput(
							inputId = "upow2",
							label = "Power",
							min = 0.01,
							max = 0.99,
							value = 0.5,
							step = 0.01,
							animate = FALSE)
					),

					wellPanel(
						h3("Outputs"),

						tableOutput("values")
					)
				),

				tabPanel("Power calculator",

					helpText("Here you can calculate the power of a 2D scan given specified sample size, SNP density, model complexity and effect size."),

					wellPanel(
						h3("Inputs"),

						numericInput(
							inputId = "nsnp3", 
							label = "Number of SNPs in SNP chip:", 
							value=1000000, min=300000),

						numericInput(
							inputId = "nid3", 
							label = "Sample size", 
							value=1000, min=1),

						numericInput(
							inputId = "grp3", 
							label = "Number of parameters:", 
							value=9, min=1),

						numericInput(
							inputId = "pvar3", 
							label = "Effect size:", 
							value=0.01, min=1e-5, max=0.99)
					),

					wellPanel(
						h3("Outputs"),

						helpText("This table compares the power for the input values based on the two threshold methods:"),

						tableOutput("powerlines"),


						helpText("You can visualise the range of detectable effect sizes in the graph below. You can use the slider to control where the lines intersect the power curves:"),

						sliderInput(
							inputId = "upow3",
							label = "Power",
							min = 0.01,
							max = 0.99,
							value = 0.5,
							step = 0.01,
							animate = TRUE),
					plotOutput("powerplot")
					)
				)
			)
		)
	)
)
