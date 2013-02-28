library(shiny)
library(ggplot2)

#######################
# Threshold functions #
#######################

# Permutation
pthresh <- function(n, m, ntrait=1)
{
	s1 <- 0.0258
	s2 <- 1.72
	s3 <- 0.0379
	s4 <- 2.33
	a <- 12.68

	thresh <- a - a/(s1 * exp(log(n)/s2) + s3 * exp(log(m)/s4))

	# effective number of tests:
	et <- 0.05 / 10^-thresh
	thresh <- 0.05 / (et*ntrait)

	return(thresh)
}

# Bonferroni
bthresh <- function(m, ntrait=1)
{
	0.05/(ntrait*m*(m-1)/2)
}

########################
# f distribution calcs #
########################


# pf and qf will revert to p/qchisq when df2>400000
# How unstable is this at large n?
qf.beta <- function(threshold, df1, df2)
{
	f <- (1 / qbeta(threshold, df2/2, df1/2, lower.tail=TRUE) - 1) * (df2/df1)
	return(f)
}

pf.beta <- function(x, df1, df2, ncp)
{
	y <- df1/df2*x
	p <- pbeta(y/(1+y), df1/2, df2/2, ncp, lower.tail=FALSE)
	return(p)
}

# This is modified from power.anova.test
# Changes:
# Uses beta functions directly to calculate F distribution
# This is to avoid built in approximations to chisq
# Also modifies tolerance to allow df2 up to 1e7
# This assumes balanced group sizes, but it is a reasonable approximation.
power2 <- function (groups = NULL, n = NULL, between.var = NULL, within.var = NULL, 
	sig.level = 0.05, power = NULL) 
{
	if (sum(sapply(list(groups, n, between.var, within.var, power, 
		sig.level), is.null)) != 1) 
		stop("exactly one of 'groups', 'n', 'between.var', 'within.var', 'power', and 'sig.level' must be NULL")
	if (!is.null(groups) && groups < 2) 
		stop("number of groups must be at least 2")
	if (!is.null(n) && n < 2) 
		stop("number of observations in each group must be at least 2")
	if (!is.null(sig.level) && !is.numeric(sig.level) || any(0 > 
		sig.level | sig.level > 1)) 
		stop("'sig.level' must be numeric in [0, 1]")
	p.body <- quote({
		lambda <- (groups - 1) * n * (between.var/within.var)
		pf.beta(qf.beta(sig.level, groups-1, (n-1)*groups), groups - 1, (n - 1) * groups, lambda)
	})
	if (is.null(power)) 
		power <- eval(p.body)
	else if (is.null(groups)) 
		groups <- uniroot(function(groups) eval(p.body) - power, 
			c(2, 100))$root
	else if (is.null(n)) 
		n <- uniroot(function(n) eval(p.body) - power, c(2, 1e+07))$root
	else if (is.null(within.var)) 
		within.var <- uniroot(function(within.var) eval(p.body) - 
			power, between.var * c(1e-07, 1e+07))$root
	else if (is.null(between.var)) 
		between.var <- uniroot(function(between.var) eval(p.body) - 
			power, within.var * c(1e-07, 1e+07))$root
	else if (is.null(sig.level)) 
		sig.level <- uniroot(function(sig.level) eval(p.body) - 
			power, c(1e-10, 1 - 1e-10))$root
	else stop("internal error")
	NOTE <- "n is number in each group"
	METHOD <- "Balanced one-way analysis of variance power calculation"
	structure(list(groups = groups, n = n, between.var = between.var, 
		within.var = within.var, sig.level = sig.level, power = power, 
		note = NOTE, method = METHOD), class = "power.htest")
}


###############
# Power calcs #
###############

# Calculate power for range of variances and given nsnp, n, grp
bonf.power <- function(pvar, nsnp, n, grp, ntrait=1) {
	thresh <- bthresh(nsnp, ntrait)
	a <- power.anova.test(groups=grp, n=n/grp, between.var=pvar, within.var=1, sig.level=thresh, power=NULL)
	return(a$power)
}

perm.power <- function(pvar, nsnp, n, grp, ntrait=1)
{
	thresh <- pthresh(n, nsnp, ntrait)
	a <- power.anova.test(groups=grp, n=n/grp, between.var=pvar, within.var=1, sig.level=thresh, power
		=NULL)
	return(a$power)
}


# EM algorithm to iterate through power2 to match threshold N with f-test N
perm.minsamplesize <- function(pvar, nsnp, pow, grp, n1=1000, ntrait=1)
{
	oldn <- n1
	thresh <- pthresh(oldn, nsnp, ntrait)
	newn <- power2(groups=grp, n=NULL, between.var=pvar, within.var=1, sig.level=thresh, power=pow)$n*grp
	return(
		if(round(newn) == round(oldn))
		{
			newn
		} else {
			perm.minsamplesize(pvar, nsnp, pow, grp, newn, ntrait)
		}
	)
}


# Calculate thresholds and sample sizes based on bonferroni correction
# No iteration required because thresholds are independent of sample size
bonf.minsamplesize <- function(pvar, nsnp, pow, grp, n1=NULL, ntrait=1)
{
	thresh <- bthresh(nsnp, ntrait)
	newn <- power2(groups=grp, n=NULL, between.var=pvar, within.var=1, sig.level=thresh, power=pow)$n*grp
	return(newn)
}


bonf.pvar <- function(nsnp, n, grp, pow, ntrait=1)
{
	thresh <- bthresh(nsnp, ntrait)
	pvar <- power2(groups=grp, n=n/grp, between.var=NULL, within.var=1, sig.level=thresh, power=pow)$between.var
	return(pvar)
}

perm.pvar <- function(nsnp, n, grp, pow, ntrait=1)
{
	thresh <- pthresh(n, nsnp, ntrait)
	pvar <- power2(groups=grp, n=n/grp, between.var=NULL, within.var=1, sig.level=thresh, power=pow)$between.var
	return(pvar)
}

# bonf.pvar(60000000, 100000, 8, 0.8)

# (a <- bonf.pvar(200, 50, 8, 0.9))

# bonf.power(a, 200, 50, 8)

# power.anova.test(8, 50000, NULL, 1, pthresh(50000,2000000), 0.99)
# power.anova.test(8, 50000, 0.00039, 1, pthresh(50000,2000000), NULL)

############
# Graphics #
############

# power.plot(20000000, 5000000, 8, 0.8)

shinyServer(function(input, output) {


	thresholdValues <- reactive(function() {

		# Calculate permutation and bonferroni thresholds

		t_perm <- -log10(pthresh(input$nid1, input$nsnp1, input$ntrait1))
		t_bonf <- -log10(bthresh(input$nsnp1, input$ntrait1))

		nom <- c("Bonferroni", "Permutation")
		val <- as.character(c(round(t_bonf, 2), round(t_perm, 2)))

		return(data.frame(Method = nom, Threshold = val))		

	})


	samplesizeValues <- reactive(function() {
	
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


	output$powerlines <- reactiveTable(function()
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


	output$powerplot <- reactivePlot(function()
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



	output$values <- reactiveTable(function() {
		samplesizeValues()
	})

	output$threshold <- reactiveTable(function() {
		thresholdValues()
	})



})

