epiPower
========

Summary
-------

This is an app built using the [shiny][0] package in R that performs the calculations for thresholds and sample sizes outlined in the paper:

*Hemani G, Wei W, Powell JE, Knott SA, Haley CS*, **Significance thresholds for epistasic searches**, *Under submission*

It is hosted on the beta server provided by [RStudio][3]:
[glimmer.rstudio.com/gibhemani/epiPower][4]


Manual instructions
-------------------

This can be run locally. You will need R, download it from [here][1]. You will also need the [shiny][0] package, in the R console run the following command:

    install.packages("shiny")
    library(shiny)

Now you are ready to run the app:

    runGitHub("epiPower", "explodecomputer")


Acknowledgements
----------------

Thanks to [Kostya][2] for help! 

[0]:(http://www.rstudio.com/shiny/)
[1]:(http://cran.r-project.org)
[2]:(https://github.com/kn3in)
[3]:(http://rstudio.org)
[4]:(glimmer.rstudio.com/gibhemani/epiPower)
