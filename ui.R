library(shiny)
library(knitr)

shinyUI(
	pageWithSidebar(

		headerPanel("epiPower"),

		sidebarPanel(
			HTML(knit2html(text=
"Genome-wide association studies can be extended to two dimensionals (test each SNP against each other SNP) to exhaustively mine for genetic interactions (**epistasis**). For example, see [epiGPU](http://sourceforge.net/projects/epigpu/).

This app can be used to calculate appropriate thresholds and various power calculations for such analyses. It is based on the work outlined in:

**Statistical thresholds for epistatic searches**. Hemani G, Wei W, Powell JE, Knott SA, Haley CS. *Under review*"
			)),
			h4("Guide to parameters:"),
			wellPanel(
				h5("Number of SNPs"),
				helpText("This is for human studies using common SNPs, the minimum number of SNPs should be 300,000")
			),
			wellPanel(
				h5("Sample size"),
				helpText("How many genotyped and phenotyped individuals are in the study")
			),
			wellPanel(
				h5("Number of parameters"),
				helpText("This relates to the model complexity. A standard GWAS has 2 parameters (the additive A effect and the mean). The joint effect of two SNPs can have up to 9 parameters (A1 + A2 + D1 + D2 + A1A2 + A1D2 + A2D1 + D1D2 + mean, where D represents the dominance effect).")
			),
			wellPanel(
				h5("Effect size"),
				helpText("This is the proportion of the phenotypic variance explained by the SNP pair")
			),
			wellPanel(
				h5("Power"),
				helpText("The statistical power is the probability that the test will reject the null hypothesis when it is false.")
			),
			wellPanel(
				h5("Number of traits"),
				helpText("The number of independent traits being analysed.")
			)

		),

		mainPanel(
			tabsetPanel(
				tabPanel("Threshold calculator", 

					h4("Input"),
					wellPanel(

						numericInput(
							inputId = "nsnp1", 
							label = "Number of SNPs:", 
							value=1000000, min=300000),

						numericInput(
							inputId = "nid1", 
							label = "Sample size (required for permutation-based threshold):", 
							value=1000, min=10),

						numericInput(
							inputId = "ntrait1", 
							label = "Number of traits:", 
							value=1, min=1)
					),

					h4("Output"),
					wellPanel(

						HTML(knit2html(text=
"These are the estimated $-\\log_{10}p$ thresholds for experiment-wide false discovere rate of $\\alpha_{F}=0.05$"
							)),

						tableOutput("threshold"),

						wellPanel(
							h5("Note:"),
							helpText("The Bonferroni correction is considered overly stringent because it assumes independence between tests. The permutation threshold assumes all SNPs are common (MAF > 0.01)")
						)
					)
				),

				tabPanel("Sample size calculator", 

					helpText("This section calculates the minimum sample size required to get significant results for specified SNP density, effect size, statistical power, and model complexity"),

					h4("Input"),
					wellPanel(
						wellPanel(
							numericInput(
								inputId = "nsnp2", 
								label = "Number of SNPs in SNP chip:", 
								value=1000000, min=300000),
							numericInput(
								inputId = "pvar2", 
								label = "Effect size:", 
								value=0.01, min=0.00001, max=0.99999),
							numericInput(
								inputId = "grp2", 
								label = "Number of parameters (e.g. A+D+I=9):", 
								value=9, min=1),
							numericInput(
								inputId = "ntrait2", 
								label = "Number of traits:", 
								value=1, min=1)
						),
						wellPanel(
							sliderInput(
								inputId = "upow2",
								label = "Power",
								min = 0.01,
								max = 0.99,
								value = 0.5,
								step = 0.01,
								animate = FALSE)
						)
					),

					h4("Output"),
					wellPanel(
						helpText("This table shows the minimum sample size required to detect the SNP effect at the specified power:"),
						tableOutput("values")
					)
				),

				tabPanel("Power calculator",

					helpText("Here you can calculate the power of a 2D scan given specified sample size, SNP density, model complexity and effect size."),

					h4("Input"),
					wellPanel(

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
							value=0.01, min=1e-5, max=0.99),

						numericInput(
							inputId = "ntrait3", 
							label = "Number of traits:", 
							value=1, min=1)
					),

					h4("Output"),
					wellPanel(

						helpText("This table compares the power for the input values based on the two threshold methods:"),

						tableOutput("powerlines"),


						helpText("You can visualise the range of detectable effect sizes in the graph below. Use the slider to control where the lines intersect the power curves:"),
						wellPanel(
							sliderInput(
								inputId = "upow3",
								label = "Power",
								min = 0.01,
								max = 0.99,
								value = 0.5,
								step = 0.01,
								animate = TRUE)
						),
						plotOutput("powerplot")
					)
				)
			)
		)
	)
)
