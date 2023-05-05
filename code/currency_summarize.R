########################################################
#########################################################

#clear workspace
rm(list=ls())

#load packages
require(stringr)
require(plyr)
require(dplyr)
require(zoo)
require(data.table)
require(tidyr)
require(ggplot2)
require(rprojroot)

homedir<-find_root(
  criterion=has_file('racialequality.Rproj')
)
datadir<-file.path(
  homedir,"data"
)
filesdir<-file.path(
  homedir,"files"
)
outputdir<-file.path(
  homedir,"output"
)
codedir<-file.path(
  homedir,"code"
)

setwd(codedir); dir()
source('getTaxRates.R')

########################################################
########################################################

#load the SCF data
setwd(datadir); dir()
require(haven)
fulldf<-read_dta('rscfp2019.dta') %>%
  data.table
names(fulldf)

oldnames<-c(
  'networth',
  'income',
  'racecl4',
  'edcl',
  'wgt'
)
newnames<-c(
  'networth',
  'income',
  'race',
  'educ',
  'weight'
)

fulldf <- fulldf[,oldnames,with=F]
names(fulldf)<-newnames

# #recodings
# tmplevels <- c(1:4)
# tmplabels <- c('white','black','hispanic','other')
# fulldf$race <- factor(fulldf$race,tmplevels,tmplabels)

#for setup, we want to know people's location in the distribution
require(spatstat)
tmpf<-ewcdf(fulldf$networth,fulldf$weight)
fulldf$networth_q <- 100 * tmpf(fulldf$networth)
#also generate race-specific percentile
tmpfunction<-function(x,w) {
  tmpf<-ewcdf(x,w)
  100 * tmpf(x)
}
fulldf[
  ,
  networth_q_race := tmpfunction(networth,weight)
  ,
  by=c('race')
]

#also order everyone from poorest to richest
fulldf$rank<-rank(fulldf$networth,ties.method='random')
#also rank within race
fulldf[
  ,
  rank_race := rank(networth,ties.method='random')
  ,
  by=c('race')
]

########################################################
########################################################

#FIGURE 1
#align the curves

plotdf<-fulldf[race%in%c(1,2),]

#add equality
tmpdf<-data.frame(
  networth_q_race=1:100,
  networth_q=1:100,
  race=3
)
plotdf<-rbind.fill(
  plotdf,
  tmpdf
) %>% data.table

plotdf$race<-factor(
  plotdf$race,
  levels=c(3,2,1),
  labels=c('Equality','Black','White')
)
tmpcolors<-c('black','blue','red')
tmptypes<-c('dashed','solid','solid')
names(tmptypes)<-names(tmpcolors)<-levels(plotdf$race)


g.tmp<-ggplot(
  plotdf,
  aes(
    x=networth_q_race,
    y=networth_q,
    group=race,
    color=race,
    linetype=race
  )
) +
  geom_line() +
  scale_color_manual(
    name="",
    values=tmpcolors
  ) +
  scale_linetype_manual(
    name="",
    values=tmptypes
  ) +
  theme_bw() +
  xlab("\nRace-Specific Wealth Quantile") +
  ylab("Overall Wealth Quantile\n")

setwd(outputdir); dir()
ggsave(
  plot=g.tmp,
  'fig_alignthecurves.png',
  width=6,
  height=4
)

#numbers for writing
plotdf[race=='Black']

