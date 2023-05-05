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
source('theme_black.R')


########################################################
########################################################

#load the sims
setwd(filesdir); dir()
simsdf <- fread('equalitydf.csv')

########################################################
########################################################

#SUMMARIZE

require(acid)
require(reldist)

loopdf<-expand.grid(
  race=c(0,1,2,3,4),
  scenario=unique(simsdf$scenario),
  stringsAsFactors=F
)

sumdf <- lapply(1:nrow(loopdf),function(ii) {
  #ii<-1
  
  #get thisdf
  mydf<-simsdf[scenario==loopdf$scenario[ii],]
  
  print(ii)
  if(loopdf$race[[ii]]!=0) {
    tmp<-mydf$race==loopdf$race[[ii]]
  } else {
    tmp<-rep(T,nrow(mydf))
  }
  gini<-weighted.gini(mydf$networth[tmp],w=mydf$weight[tmp])$Gini %>%
    as.vector
  total<-sum(mydf$networth[tmp]*mydf$weight[tmp])
  mean<-weighted.mean(mydf$networth[tmp],mydf$weight[tmp])
  median<-modi::weighted.quantile(mydf$networth[tmp],mydf$weight[tmp],0.5)
  data.frame(
    i=loopdf$scenario[ii],
    N=sum(mydf$weight[tmp]),
    gini,
    total,
    mean,
    median
  )
}) %>% rbind.fill 
sumdf <- cbind(loopdf,sumdf) %>% data.table

########################################################
########################################################

#PLOT

require(scales)

#summarize racial inequality
plotdf<-sumdf[
  scenario=='status quo by patterned, status quo racial inequality' & 
    race%in%c(1,2)
]

plotdf$race<-factor(
  plotdf$race,
  levels=c(2,1),
  labels=c('Black','White')
)
tmpcolors<-c('blue','red')
names(tmpcolors)<-levels(plotdf$race)

g.tmp <- ggplot(
  plotdf,
  aes(
    x=race,
    y=mean,
    fill=race
  )
) +
  geom_bar(
    stat='identity',
    color='grey',
    width=0.3
  ) +
  geom_text(
    data=plotdf,
    aes(
      x=race,
      y=mean+50000,
      label=paste0("$",prettyNum(round(mean),big.mark = ','))
    ),
    color='white'
  ) +
  scale_fill_manual(
    name="",
    values=tmpcolors
  ) + 
  scale_y_continuous(label=dollar_format()) +
  theme_black() +
  xlab("") + ylab("Average Household Wealth")


setwd(outputdir)
ggsave(
  plot=g.tmp,
  'fig_equalitysims_statusquo_describe.png',
  width=6,
  height=4
)


########################################################
########################################################

#PLOT THE GAP AFTER OUR TWO INTERVENTIONS

#summarize racial inequality
plotdf<-sumdf[
  scenario%in%c(
    'better by patterned, racial equality',
    'status quo by patterned, racial equality'
  )
    & 
    race%in%c(1,2)
]


plotdf$race<-factor(
  plotdf$race,
  levels=c(2,1),
  labels=c('Black','White')
)
tmpcolors<-c('blue','red')
names(tmpcolors)<-levels(plotdf$race)

tmplevels<-c(
  'better by patterned, racial equality',
  'status quo by patterned, racial equality'
)
tmplabels<-c("I.","II.")
plotdf$scenario <- factor(
  plotdf$scenario,
  tmplevels,
  tmplabels
)

g.tmp <- ggplot(
  plotdf,
  aes(
    x=race,
    y=mean,
    fill=race
  )
) +
  geom_bar(
    stat='identity',
    color='white',
    width=0.3
  ) +
  geom_text(
    data=plotdf,
    aes(
      x=race,
      y=mean+50000,
      label=paste0("$",prettyNum(round(mean),big.mark = ','))
    ),
    color='white'
  ) +
  scale_fill_manual(
    name="",
    values=tmpcolors
  ) + 
  scale_y_continuous(label=dollar_format()) +
  theme_black() +
  facet_wrap(
    ~ scenario
  ) +
  xlab("") + ylab("Average Household Wealth\n")


setwd(outputdir)
ggsave(
  plot=g.tmp,
  'fig_equalitysims_interventions_describe.png',
  width=8,
  height=4
)


########################################################
########################################################

#PLOT
#plot the CDFs

#fix scenario, for plotting
# simsdf$scenario %>% unique
# simsdf$racialequality<-c(
#   'Racial Inequality'
# )
# simsdf$racialequality[str_detect(simsdf$scenario,'racial\\sequality')]<-'Racial Equality'
# 
# simsdf$scenario %>% unique
# simsdf$principle<-c(
#   'Status Quo'
# )
# simsdf$principle[str_detect(simsdf$scenario,'better')]<-'Redistribution'

plotdf<-simsdf[
  race%in%c(1,2) &
    scenario%in%c(
      'status quo by patterned, status quo racial inequality',
      'better by patterned, racial equality',
      'status quo by patterned, racial equality'
    )
  ,
  c('scenario','race','networth_q_race','networth')
]

tmplevels<-c(
  'status quo by patterned, status quo racial inequality',
  'better by patterned, racial equality',
  'status quo by patterned, racial equality'
)
tmplabels<-c("Status Quo","I. The Wealth Tax","II. Kleptocratic Redistribution")
plotdf$scenario <- factor(
  plotdf$scenario,
  tmplevels,
  tmplabels
)

plotdf$race<-factor(
  plotdf$race,
  levels=c(2,1),
  labels=c('Black','White')
)
tmpcolors<-c('blue','red')
names(tmpcolors)<-levels(plotdf$race)


g.tmp <- ggplot(
  plotdf,
  aes(
    x=networth_q_race,
    y=networth,
    color=race
  )
) + 
  geom_line(
    size=1.5
  ) +
  scale_color_manual(
    name="",
    values=tmpcolors
  ) + 
  scale_y_log10(
    breaks = 10^(0:9),
    labels = c(
      "$1 (and below)",
      "$10",
      "$100",
      "$1000",
      "$10,000",
      "$100,000",
      "$1 million",
      "$10 million",
      "$100 million",
      "$1 billion"
    )
  ) +
  theme_black() +
  ylab("Household Wealth\n") +
  xlab("\nWithin-Race Quantile") +
  facet_wrap(
    ~ scenario
  ) +
  theme(
    legend.position='bottom',
    legend.direction='horizontal',
    panel.grid.minor=element_blank(),
    panel.grid.major=element_line(size=0.15)
  )

setwd(outputdir)
ggsave(
  plot=g.tmp,
  'fig_equalitysims_cdfs.png',
  width=12*0.95,
  height=4*0.95
)




