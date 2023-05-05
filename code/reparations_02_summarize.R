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

#load the reparations
setwd(filesdir); dir()
reparationsdf <- fread('reparationsdf.csv')

########################################################
########################################################

#get some summary stats

require(acid)
require(reldist)
loopdf<-expand.grid(
  race=c(0,1,2,3,4),
  type=c('networth','networth_reparations'),
  world=unique(reparationsdf$i),
  stringsAsFactors=F
)

sumdf <- lapply(1:nrow(loopdf),function(ii) {
  #ii<-6
  
  #get thisdf
  mydf<-reparationsdf[i==loopdf$world[ii],]
  
  print(ii)
  if(loopdf$race[[ii]]!=0) {
    tmp<-mydf$race==loopdf$race[[ii]]
  } else {
    tmp<-rep(T,nrow(mydf))
  }
  gini<-weighted.gini(mydf[[loopdf$type[ii]]][tmp],w=mydf$weight[tmp])$Gini %>%
    as.vector
  total<-sum(mydf[[loopdf$type[ii]]][tmp]*mydf$weight[tmp])
  mean<-weighted.mean(mydf[[loopdf$type[ii]]][tmp],mydf$weight[tmp])
  median<-modi::weighted.quantile(mydf[[loopdf$type[[ii]]]][tmp],mydf$weight[tmp],0.5)
  data.frame(
    i=loopdf$world[ii],
    N=sum(mydf$weight[tmp]),
    gini,
    total,
    mean,
    median
  )
}) %>% rbind.fill 
sumdf <- cbind(loopdf,sumdf) %>% data.table

sumdf<-merge(
  sumdf,
  unique(reparationsdf[,c('i','based','tax','transfer')]),
  by='i'
)

########################################################
########################################################

#TABLE 1

#plot tables of relevant stats

plotdf<-sumdf[
  based=='gap' & 
    tax=='progressive' & 
    transfer=='flat'
]
plotdf<-gather(
  plotdf,
  var,
  val,
  N:median
) %>% data.table
plotdf<-plotdf[race%in%c(0,1,2) & var%in%c('gini','median','mean')]

plotdf$race<-factor(
  plotdf$race,
  levels=c(0,1,2,3,4),
  labels=c('Overall','White','Black','Hispanic','Other')
)

plotdf$type<-factor(
  plotdf$type,
  levels=c('networth','networth_reparations') %>% rev,
  labels=c('Before Reparations','After Reparations') %>% rev
)

#round these numbers to the nearest 1000
tmp<-plotdf$var%in%c('mean','median')
plotdf$val[tmp] <- 10^3 *round(plotdf$val[tmp]/10^3)

#make them pretty
plotdf$val <- prettyNum(plotdf$val,digits=2,big.mark = ',')
plotdf$val[plotdf$var%in%c('mean','median')]<-
  paste0("$",plotdf$val[plotdf$var%in%c('mean','median')])

plotdf$var <- factor(
  plotdf$var,
  levels=c('mean','median','gini'),
  labels=c('Mean','Median','Gini')
)

tmpcolors<-c('red','black')
names(tmpcolors)<-levels(plotdf$type)

g.tmp <- ggplot(
  plotdf,
  aes(
    x=race,
    y=type,
    label=val,
    color=type
  )
) +
  geom_tile(color='black',fill='white') +
  geom_text() +
  scale_color_manual(
    guide='none',
    values=tmpcolors
  ) +
  scale_x_discrete(position='top') + 
  xlab("") + ylab("") +
  facet_wrap(
    ~ var,
    ncol=1
  ) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.line= element_blank(),
    panel.border=element_blank(),
    axis.ticks = element_blank()
  )

setwd(outputdir)
ggsave(
  plot=g.tmp,
  'tab_reparations.png',
  width=4,
  height=4
)


########################################################
########################################################

#make a summary plot in the progressive world

plotdf<-sumdf[based=='gap' & tax=='progressive' & transfer=='progressive']
plotdf<-gather(
  plotdf,
  var,
  val,
  N:median
) %>% data.table
plotdf<-plotdf[race%in%c(0,1,2) & var%in%c('gini','median','mean')]

# plotdf$var<-factor(
#   plotdf$var,
#   levels=c('mean','median'),
#   labels=c('Mean','Median')
# )

plotdf$race<-factor(
  plotdf$race,
  levels=c(0,1,2,3,4),
  labels=c('Overall','White','Black','Hispanic','Other')
)

plotdf$type<-factor(
  plotdf$type,
  levels=c('networth','networth_reparations') %>% rev,
  labels=c('Before Reparations','After Reparations') %>% rev
)

plotdf$val <- prettyNum(plotdf$val,digits=2,big.mark = ',')
plotdf$val[plotdf$var%in%c('mean','median')]<-
  paste0("$",plotdf$val[plotdf$var%in%c('mean','median')])

plotdf$var <- factor(
  plotdf$var,
  levels=c('mean','median','gini'),
  labels=c('Mean','Median','Gini')
)

tmpcolors<-c('red','black')
names(tmpcolors)<-levels(plotdf$type)

g.tmp <- ggplot(
  plotdf,
  aes(
    x=race,
    y=type,
    label=val,
    color=type
  )
) +
  geom_tile(color='black',fill='white') +
  geom_text() +
  scale_color_manual(
    guide='none',
    values=tmpcolors
  ) +
  scale_x_discrete(position='top') + 
  xlab("") + ylab("") +
  facet_wrap(
    ~ var,
    ncol=1
  ) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.line= element_blank(),
    panel.border=element_blank(),
    axis.ticks = element_blank()
  )

setwd(outputdir)
ggsave(
  plot=g.tmp,
  'tab_reparations_progressive.png',
  width=4,
  height=4
)


########################################################
########################################################

#plot the CDFs

plotdf<-reparationsdf[
  race%in%c(1,2,3,4) & 
    based=='gap' & 
    tax=='progressive' & 
    transfer=='progressive',
  c('race','networth_q_race','networth','networth_reparations')
]

#poorest blacks
plotdf[race==2,min(networth_reparations)]
plotdf[race==2,summary(networth_reparations)]
plotdf[race!=2,summary(networth_reparations)]

plotdf<-plotdf[race%in%c(1,2),]
plotdf<-gather(
  plotdf,
  var,
  val,
  networth:networth_reparations
)

plotdf$race<-factor(
  plotdf$race,
  levels=c(2,1),
  labels=c('Black','White')
)
tmpcolors<-c('blue','red')
names(tmpcolors)<-levels(plotdf$race)

plotdf$var<-factor(
  plotdf$var,
  levels=c('networth','networth_reparations'),
  labels=c('Before','After')
)

#make the graph
g.tmp<-ggplot(
  plotdf,
  aes(
    x=networth_q_race,
    y=val,
    color=race
  )
) + 
  geom_line(
    size=1
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
  theme_bw() +
  ylab("Net Worth\n") +
  xlab("\nWithin-Race Quantile") +
  facet_wrap(
    ~ var,
    ncol=1
  ) +
  theme(
    legend.position='top',
    legend.direction='horizontal'
  )

setwd(outputdir)
ggsave(
  plot=g.tmp,
  'fig_reparations_progressive_cdfs.png',
  width=6,
  height=8
)


# #highlight median gap
# geom_point(
#   data=tmpdf,
#   color='grey',
#   size=4
# ) +
# annotate(
#   "text",
#   x=tmpdf$wealth_q[tmpdf$race_m=='Black'] - 2.5,
#   y=tmpdf$wealth[tmpdf$race_m=='Black'] + 500,
#   label='A',
#   size=7.5
# ) +
# annotate(
#   "text",
#   x=tmpdf$wealth_q[tmpdf$race_m=='White'] - 2.5,
#   y=tmpdf$wealth[tmpdf$race_m=='White'] + 2500,
#   label='B',
#   size=7.5
# ) +
# # geom_line(
# #   data=tmpdf,
# #   linetype='solid',
# #   color='black',
# #   size=1
# # ) +
# #highlight 10th,90th percentile black
# geom_point(
#   data=tmpdf2,
#   color='grey',
#   size=4
# ) +
# annotate(
#   "text",
#   x=tmpdf2$wealth_q[tmpdf2$wealth_q==85],
#   y=tmpdf2$wealth[tmpdf2$wealth_q==85] - 20000,
#   label='C',
#   size=7.5
# ) +
# geom_hline(
#   yintercept=tmpdf2$wealth,
#   linetype='dashed'
# ) +
#this governs the tick marks





