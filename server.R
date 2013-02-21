library(shiny)

pthresh <- function(n, m)
{
	s1 <- 0.0258
	s2 <- 1.72
	s3 <- 0.0379
	s4 <- 2.33
	a <- 12.68

	thresh <- a - a/(s1 * exp(log(n)/s2) + s3 * exp(log(m)/s4))
	return(10^-thresh)
}

# To calculate relationship between power, threshold, density, sample size, will have to be iterative 
# Deprecated - this requires evenly sized groups
calc.power.it <- function(pvar, nsnp, pow, n1, grp)
{
	oldn <- n1
	a <- power.anova.test(groups=grp, n=NULL, between.var=pvar, within.var=1, sig.level=pthresh(oldn, nsnp), power=pow)
	newn <- round(a$n*grp)
	return(
		if(newn == oldn)
		{
			newn
		} else {
			print(c(newn, oldn))
			calc.power.it(pvar, nsnp, pow, newn, grp)
		}
	)
}


# This function calculates the expected Fval for a set of parameters
# In other words, it calculates the non-centrality parameter, so can be used when 1-beta=0.5
calc.fval <- function(
	pvar,        # Proportion of variance explained [0, 1]
	n,           # Sample size
	nparam       # Number of parameters (9 for epistasis)
){
	SSW <- pvar*(n-1)
	SSB <- (1-pvar)*(n-1)
	df1 <- nparam - 1
	df2 <- n - nparam

	Fval <- (SSW / df1) / (SSB / df2)
	pval <- -log10(pf(Fval, df1, df2, lower.tail=FALSE))
	return(list(F=Fval, p=pval))
}

# This function calculates the minimum sample size required to meet a threshold with 50% power
# For a pre-specified sample size in generating the threshold
calc.minsamplesize <- function(
	threshold, # -log10 p value required for significance
	npar,      # number of parameters
	pvar,      # proportion of variance required
	nrange = seq(100, 1000, by=1)
){
	f <- qf(10^-threshold, npar-1, nrange-npar, lower.tail=FALSE)
	x <- pvar
	p <- npar
	n <- (f * (1 - x) * (p - 1)) / x + p
	b <- calc.fval(pvar, n, npar)
	d <- abs(b$p - threshold)
	pos <- which(d == min(d))[1]

	return(
		if(pos == length(nrange))
		{
			calc.minsamplesize(threshold,
			npar, 
			pvar, 
			nrange+max(nrange)-min(nrange))
		} else {
			nrange[pos]
		}
	)
}

# EM algorithm to iterate through calc.minsamplesize to match threshold N with f-test N
perm.minsamplesize <- function(pvar, nsnp, n1=1000, grp)
{
	oldn <- n1
	newn <- calc.minsamplesize(-log10(pthresh(n1, nsnp)), grp, pvar)
	return(
		if(newn == oldn)
		{
			newn
		} else {
			#print(c(newn, oldn))
			perm.minsamplesize(pvar, nsnp, newn, grp)
		}
	)
}

# Calculate thresholds and sample sizes based on bonferroni correction
# No iteration required because thresholds are independent of sample size
bonf.minsamplesize <- function(pvar, nsnp, n=NULL, grp)
{
	calc.minsamplesize(threshold=-log10(0.05/(nsnp*(nsnp-1)/2)), grp, pvar)
}



shinyServer(function(input, output) {

	powerValues <- reactive(function() {
	
		# Assume only want to know minimum sample size...
		n_perm <- perm.minsamplesize(
			pvar = input$pvar, 
			nsnp = input$nsnp, 
			grp  = input$grp)

		n_bonf <- bonf.minsamplesize(
			pvar = input$pvar, 
			nsnp = input$nsnp, 
			grp  = input$grp)

		t_perm <- -log10(pthresh(n_perm, input$nsnp))
		t_bonf <- -log10(0.05/(input$nsnp*(input$nsnp-1)*0.5))

		Parameters <- c("Number of SNPs", "Vg/Vp", "Power", "-log10(p) threshold", "Required sample size"),

		v_bonf <- as.character(c(input$nsnp, input$pvar, 0.5, t_bonf, n_bonf))
		v_perm <- as.character(c(input$nsnp, input$pvar, 0.5, t_perm, n_perm))

		return(data.frame(Parameters = Parameters, Bonferroni = v_bond, Permutation = v_perm))

	})

	output$values <- reactiveTable(function() {
		powerValues()
	})

})


































