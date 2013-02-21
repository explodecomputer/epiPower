epiPower
========

Summary
-------

This is an app built using the [shiny][0] package in R that performs the calculations for thresholds and sample sizes outlined in the paper:

*Hemani G, Powell JE, Knott SA, Haley CS*, **Significance thresholds for epistasic searches**, *Under submission*


Instructions
------------

We're in the process of deploying this to a server so that it can be used as a regular app on the web. In the meantime, please run it from your local machine. You will need R, download it from [here][1]. You will also need the [shiny][0] package, in the `R` console run the following command:

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
