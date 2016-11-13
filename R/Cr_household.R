#################################################################################

# LSHTM MSc Epidmeiology thesis.
# Originally performed in STATA 14 and submitted on 09/07/16.

# code repository available at Github.com/brooke-watson/thesis.
# dataset access restricted for ethical reasons.

#################################################################################

# clearing workspace
rm(list=ls())

# loading required packages
pkgs = (c('readxl', 'dplyr', 'janitor', 'lubridate', 'data.table', 'devtools'))
lapply(pkgs, library, character.only = TRUE)

# load my specific package
if (!require(thesis)) {install_github('brooke-watson/thesis')}
library(thesis)


# # sourcing homemade functions i'll need later - planning to turn this into a package.
# setwd("~/Desktop/git/thesis/R")
# source('getseason.R')
# source('munge.R')

# reading in the data

setwd("~/Desktop/MAPSAN")
hh_raw = read_excel(paste0(getwd(), '/datafiles/HH.xlsx'))

hh = hh_raw = clean_names(hh_raw)
hh = hh %>% select(end_record_date, bairid, quartnr, control_or_intervention, compid, hhid,
                   is_there_a_child_5, respondent, caretaker_edu_level, x_ppl_in_hh, x_bedrooms,
                   x_beds, car, energy, iron, fridge, clock, cassete, sofa_in_house,
                   kitchen_inside_or_outside_the_house, hh_floor_material, hh_wall_material,
                   grated_windows, grated_doors, drinkwat, hhsan, drophole_covered,
                   ventilation_pipe_on_latrine, pedestral_masonry_in_latrine) %>%
  mutate(doe = dmy_hms(end_record_date)) %>%
  mutate(season = getseason(doe)) %>% # generate a season variable
  thesis::munge() %>% # trim white space and make all lowercase
  mutate(control_or_intervention = as.factor(control_or_intervention)) %>%
  filter(is_there_a_child_5 == "sim") # drop all households without a child under five

#recode education variable
hh$caretaker_edu_level = recode(hh$caretaker_edu_level, 'ensino_superior_conclu_do' = "primary completed",
                             'ensino_t_cnico_conclu_do' = "primary completed", 'nenhuma' = "primary not completed",
                             'primaria_concluida' = "primary completed", 'primaria_nao_concluida' = 'primary not completed',
                             'secundaria_concluida' = "primary completed", 'secundaria_nao_concluida' = "primary completed",
                              'superior' = "primary completed",
                             .default = NA_character_)
# crowding variable
hh$x_bedrooms = recode(hh$x_bedrooms, '0'=1)
hh = mutate(hh, crowding = x_ppl_in_hh/x_bedrooms)
hh$crowding2 = cut(hh$crowding, c(0,3,5,100)) # this one has to go first
hh$crowding = cut(hh$crowding, c(0,4,100))

# encode all the other stuff.
hh[] lapply(hh[], as.factor)
colnames = data.table(1:length(hh), names(hh))
nums = c(27, 28, 29, 25, )

#
#
#
# *encode source of drinking water
# encode drinkwat, gen(water)
# label drop water
# recode water 5=. 4=3
# tab water
# label define drinkwat 1 "inside the house" 2 "in the yard" 3 "neighbor's yard/public"
# label values water drinkwat
# tab water
# drop drinkwat
#
#
# * binary water variable 1: inside/outside
# gen wat2 = water
# recode wat2 2/3=0
# tab wat2
# label variable wat2 "water source"
# label define wat2 1 "inside house" 0 "outside house"
# label values wat2 wat2
# tab wat2
#
#
# *binary water variable 2: own/neighbor's'
# gen wat3 = water
# recode wat3 2=1 3=0
# tab wat3
# label define wat3 1 "own home/yard" 0 "neighbor's yard/public"
# label values wat3 wat3
# label variable wat3 "water source: own or neighbor's"
#
# ///////////               POVERTY SCORECARD               ////////////////
# ////          *if it aint about the money*         ////
#
# /* documentation:
# http://www.simplepovertyscorecard.com/MOZ_2008_ENG.pdf
#
# This document and related tools are at
# microfinance.com/#Mozambique
# .
# Uma versão em Português está disponível em
# microfinance.com/Portugues
#
# note - numeric values do not correspond to actual numbers but to
# points associated with a category. for corresponding brackets,
# see http://www.simplepovertyscorecard.com/MOZ_2008_ENG.pdf.
# */
#
# *q1 - how many members in household?
# tab pplinHH
# gen ps1 = pplinHH
# recode ps1 8/21=0 2=30 7=2 6=7 5=9 4=15 3=23
# tab ps1
# rename ps1 pplinHHscore
#
# *q2 - what is the main material fo the floor of the res?
# tab HHfloormat
# encode HHfloormat, gen(ps2)
# tab ps2, nolab
# recode ps2 1=0 2=6
# rename ps2 HHfloormatscore
#
# *q3 - what is the main material of the walls of the res?
# tab HHwall
# encode HHwall, gen(ps3)
# tab ps3, nolab
# tab ps3
# recode ps3 1=7 2=0
# rename ps3 HHwallscore
#
# *q4 - what toilet arrangement does the HH have in its residence?
# tab hhsan
# tab hhsan, nolab
# encode hhsan, gen(ps4)
# tab ps4
# tab ps4, nolab
# recode ps4 1=0 6=14 2=6 3=0 4=0 5=0 7=6 8=14
# tab ps4
# label drop ps4
# tab ps4
# rename ps4 hhsanscore
# label define hhsanscore 0 "None, or other" 6 "Pit slab or toilet to surface drain" 14 "Toilet to sewer or underground tank drain"
# label values hhsanscore hhsanlab
# drop hhsan
#
# //also, regrouping hhsan for use as an exposure variable:
# gen hhsan = hhsanscore
# recode hhsan 6=1 14=2
# label define hhsanlab 0 "None, or other" 1 "Latrine of any kind" 2 "Toilet connected to septic tank or sewer"
# label values hhsan hhsanlab
#
#
# *q5 - what is the main source of energy for lighting?
#
# tab energy
# encode energy, gen(energyscore)
# tab energyscore, nolab
# recode energyscore 1=5 2=0 3=3 4=1
# tab energyscore
# label drop energyscore
#
# *q6 - does the hh have a non electric or electric clothes iron?
# tab iron
# encode iron, gen(ps6)
# tab ps6, nolab
# label drop ps6
# recode ps6 1=0 2=3
# tab ps6
# rename ps6 ironscore
#
# *q7 - does the household have a clock?
# tab clock
# encode clock, gen(ps7)
# tab ps7, nolab
# label drop ps7
# recode ps7 1=0 2=4
# tab ps7
# rename ps7 clockscore
#
# *q8 - does the household have a radio, stereo system, or cassette player?
# tab cass
# encode cass, gen(ps8)
# tab ps8, nolab
# label drop ps8
# recode ps8 1=0 2=5 3=7
# tab ps8
# rename ps8 cassettescore
#
# *q9 - does the HH have a bike, motorcycle or car?
# tab car
# encode car, gen(ps9)
# tab ps9, nolab
# label drop ps9
# recode ps9 1=0 2=5 3=15
# tab ps9
# rename ps9 carscore
#
# *q10 - how many beds does the household have?
# tab beds
# encode beds, gen(ps10)
# tab ps10, nolab
# label drop ps10
# recode ps10 1=5 2=0 3=2
# tab ps10
# rename ps10 bedscore
#
# * generate total poverty score
# egen povscore=rowtotal(pplinHHscore - bedscore)
# tab povscore
# recode povscore 0=.
#
#
#
#
# // Final labelling
# ******************************************************************************/
# label variable hhsan "Latrine or system most used by family members"
# label variable water "Water source most used by family members"
# label variable vent "covered ventilation pipe on household drophole"
# label variable pede "Masonry or tile  slab, or pedestal on household drophole"
# label variable drophole "Household drophole is covered"
#
#
#
#
# // Create
# ******************************************************************************/
# cd h:/mapsan/datafiles
# save hh_for_merge, replace
# cd h:/mapsan/dofiles
