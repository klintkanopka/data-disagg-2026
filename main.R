library(tidyverse)
library(stringr)
library(readxl)
library(viridis)
library(see)
library(ggrepel)

setwd('~/projects/data-disagg-2026')

source('category-defs.R')
source('category-names.R')
source('preproc-admissions.R')
source('preproc-enrollment.R')

source('single-identity.R')

##############
# Admissions #
##############

source('admit-rate.R')
source('dup-admit.R')
source('pooled-admit.R')
source('single-admit.R')

source('aa-admit.R')
source('nhpi-admit.R')

source('yield-rate.R')
source('dup-yield.R')
source('pooled-yield.R')
source('single-yield.R')

source('aa-yield.R')
source('nhpi-yield.R')

##############
# Enrollment #
##############

source('dup-enrolled.R')
source('pooled-enrolled.R')
source('single-enrolled.R')
source('aa-enrollment.R')
source('nhpi-enrollment.R')

source('filipino-enrollment.R')
source('hawaiian-enrollment.R')

source('p-latinx.R')

source('cat-breakdowns.R')


####################
# Final ER Figures #
####################

source('final-group-1.R')
source('final-group-2.R')
source('final-group-3.R')
source('final-group-4.R')
