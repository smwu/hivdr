# hivdr
Shiny app tool for sample size calculations for WHO lab-based HIVDR protocol

The code in the "Design process" folder allows the user to toggle the prevalence, precision, population size, significance level, and genotyping failure input parameters. It is intended for use in determining assumed parameter values at the survey design stage. Two plots are shown, one displaying the effect of the finite population correction on the required sample size, and one displaying confidence bands for difference prevalence misspecifications. 

The code in the "Sample size calculator" folder is meant for easy use by those implementing the lab-based HIVDR survey. The user specifies the population size, clicks "Submit", and the required sample size as well as fixed parameter values are then displayed. The default sample size calculation uses an infinite population.

The code in the "Activity 2 calculator" folder is the sample size calculator to be used for countries implementing Activity 2, where viral load testing is at least 80% but there is less than 60% coverage of required survey variables. The default sample size calculation uses an infinite population.
