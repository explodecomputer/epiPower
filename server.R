library(shiny)
library(ggplot2)
source("functions.R")

shinyServer(function(input, output) {


	thresholdValues <- reactive(
	{
		# Calculate permutation and bonferroni thresholds
		t_perm <- -log10(pthresh(input$nid1, input$nsnp1, input$ntrait1))
		t_bonf <- -log10(bthresh(input$nsnp1, input$ntrait1))

		nom <- c("Bonferroni", "Permutation")
		val <- as.character(c(round(t_bonf, 2), round(t_perm, 2)))

		return(data.frame(Method = nom, Threshold = val))		
	})


	samplesizeValues <- reactive(
	{
		# Assume only want to know minimum sample size...
		n_perm <- perm.minsamplesize(
			pvar = input$pvar2, 
			nsnp = input$nsnp2, 
			pow  = input$upow2,
			grp  = input$grp2,
			ntrait = input$ntrait2)

		n_bonf <- bonf.minsamplesize(
			pvar = input$pvar2, 
			nsnp = input$nsnp2,
			pow  = input$upow2, 
			grp  = input$grp2,
			ntrait = input$ntrait2)

		t_perm <- -log10(pthresh(n_perm, input$nsnp2, input$ntrait2))
		t_bonf <- -log10(bthresh(input$nsnp2, input$ntrait2))

		ret <- data.frame(
			Method = c("Bonferroni", "Permutation"),
			Threshold = as.character(signif(c(t_bonf, t_perm), 4)),
			SampleSize = as.character(round(c(n_bonf, n_perm)))
		)
		return(ret)
	})


	powerLinesTab <- reactive(
	{
		nsnp = input$nsnp3
		n    = input$nid3
		grp  = input$grp3
		upow = input$upow3
		pvar = input$pvar3
		ntrait = input$ntrait3

		brange <- c(
			bonf.pvar(nsnp, n, grp, 0.01),
			bonf.pvar(nsnp, n, grp, 0.99))
		prange <- c(
			perm.pvar(nsnp, n, grp, 0.01),
			perm.pvar(nsnp, n, grp, 0.99))
		arange <- c(
			min(c(brange, prange)), 
			max(c(brange, prange)))

		eff <- seq(arange[1], arange[2], length.out=100)

		dat <- data.frame(
			eff=c(eff, eff), 
			pow=c(
				bonf.power(eff, nsnp, n, grp, ntrait),
				perm.power(eff, nsnp, n, grp, ntrait)), 
			thresh=rep(c("Bonferroni", "Permutation"), each=length(eff))
		)

		powerlines <- data.frame(
			Method=c("Bonferroni", "Permutation"),
			Threshold=as.character(signif(-log10(c(bthresh(nsnp, ntrait), pthresh(n, nsnp, ntrait))), 4)),
			Effect = as.character(signif(pvar, 2)),
			Power = as.character(signif(c(
				with(subset(dat, thresh=="Bonferroni"),
					approx(
						x=eff, 
						y=pow, 
						xout=pvar, 
						yleft=0, yright=1)$y),
				with(subset(dat, thresh=="Permutation"),
					approx(
						x=eff, 
						y=pow, 
						xout=pvar,
						yleft=0, yright=1)$y)), 3))
		)

		return(powerlines)
	})

	powerPlot <- reactive(
	{
		nsnp = input$nsnp3
		n    = input$nid3
		grp  = input$grp3
		upow = input$upow3
		ntrait = input$ntrait3

		brange <- c(
			bonf.pvar(nsnp, n, grp, 0.01, ntrait),
			bonf.pvar(nsnp, n, grp, 0.99, ntrait))
		prange <- c(
			perm.pvar(nsnp, n, grp, 0.01, ntrait),
			perm.pvar(nsnp, n, grp, 0.99, ntrait))
		arange <- c(
			min(c(brange, prange)), 
			max(c(brange, prange)))

		eff <- seq(arange[1], arange[2], length.out=100)

		dat <- data.frame(
			eff=c(eff, eff), 
			pow=c(
				bonf.power(eff, nsnp, n, grp, ntrait),
				perm.power(eff, nsnp, n, grp, ntrait)), 
			thresh=rep(c("Bonferroni", "Permutation"), each=length(eff))
		)

		powerlines <- data.frame(
			eff=c(
				with(subset(dat, thresh=="Bonferroni"),
					approx(x=pow, y=eff, xout=upow)$y),
				with(subset(dat, thresh=="Permutation"),
					approx(x=pow, y=eff, xout=upow)$y)),
			thresh=c("Bonferroni", "Permutation")
		)

		d <- ggplot(dat, aes(y=pow, x=eff)) +
		geom_line(aes(colour=thresh)) +
		labs(
			x=expression(V[G]/V[P]), 
			y="Power", 
			colour="Threshold method") +
		geom_hline(yintercept=upow) +
		geom_vline(linetype=2, aes(xintercept=eff, colour=thresh), data=powerlines)
		print(d)
	})


	output$values <- renderTable(
	{
		samplesizeValues()
	})

	output$threshold <- renderTable(
	{
		thresholdValues()
	})

	output$powerlines <- renderTable(
	{
		powerLinesTab()
	})

	output$powerplot <- renderPlot(
	{
		powerPlot()
	})

})

