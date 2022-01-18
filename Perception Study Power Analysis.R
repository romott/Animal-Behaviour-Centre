# Load Packages
library(pwr)

# clear memory

rm(list = ls())

# pwr.f2.test	general linear model
# pwr.f2.test(u =, v = , f2 = , sig.level = , power = )

# where u and v are the numerator and denominator degrees of freedom. We use f2 as the effect size measure.

pwr.f2.test(u =1, f2 = 0.70 , sig.level = 0.05 , power = 0.95 )
