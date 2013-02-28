library(shiny)
library(knitr)

shinyUI(
	pageWithSidebar(

		headerPanel("epiPower"),

		sidebarPanel(
			h3("Guide to parameters"),
			HTML(knit2html(text=
"**Number of SNPs**: This is for human studies using common SNPs, the minimum number of SNPs should be 300,000

**Sample size**: How many genotyped and phenotyped individuals are in the study

**Number of parameters**: This relates to the model complexity. A standard GWAS has 2 parameters (the additive A effect and the mean). The joint effect of two SNPs can have up to 9 parameters (A1 + A2 + D1 + D2 + A1A2 + A1D2 + A2D1 + D1D2 + mean, where D represents the dominance effect).

**Effect size**: This is the proportion of the phenotypic variance explained by the SNP pair

**Power**: The statistical power is the probability that the test will reject the null hypothesis when it is false.

**Number of traits**: The number of independent traits being analysed."
			))
		),

		mainPanel(
			tabsetPanel(
				tabPanel("About",
					wellPanel(
						HTML(knit2html(text=
"### Summary

Genome-wide association studies can be extended to two dimensions (test each SNP against each other SNP) to exhaustively mine for genetic interactions (**epistasis**). For example, see [epiGPU](http://sourceforge.net/projects/epigpu/) for software that can perform this kind of analysis.

Where should statistical thresholds be set to control false discovery rates for 2D scans? Because SNPs are correlated, correcting for the exact number of tests performed (*i.e.* using Bonferroni) is overly stringent.

Use this app to calculate appropriate thresholds and various power calculations for such analyses. It is based on the work outlined in:

**Statistical thresholds for epistatic searches**. Hemani G, Wei W, Powell JE, Knott SA, Haley CS. *Under review*"
						))
					),
					wellPanel(
						HTML(knit2html(text=
"The source code for this app is maintained at [https://github.com/explodecomputer/epiPower](https://github.com/explodecomputer/epiPower)"
						))
					)
				),

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
