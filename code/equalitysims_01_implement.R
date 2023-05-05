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

#SIMULATE

#interventions:
#we have a 2x2, with three interesting squares relative to status quo
#more racial equality, more patterned principle (1)
#more racial equality, null patterned principle (2)
#null racial equality, more patterend principle (3)

#we want to show that (1) >>> (2). 
#(2) is not likely to be very normatively meaningful

#we also want to show that (3) is almost = (1)
#this suggests that focusing on racial equality
#will miss things that are quite significant

#set up
#(1) take wealth from millionaires up to a million, fill in the bucket
#(2) take wealth from white millionaires, give it to black millionaires
#(3) like (1), except that the redistribution is biased in a way that preserves the wealth gap

scenarios<-c(
  'better by patterned, racial equality',
  'better by patterned, status quo racial inequality',
  'status quo by patterned, racial equality',
  'status quo by patterned, status quo racial inequality'
)
simsdf <- lapply(scenarios,function(x) {
  
  #x<-scenarios[1]
  tmpdf<-fulldf
  
  
  if(x=='better by patterned, racial equality') { #(1)

    #identify millionaires of all races 
    tmp <- tmpdf$networth>=10^6 
    #this is their stock of wealth 
    tot_wealth <- sum(tmpdf$networth[tmp] * tmpdf$weight[tmp]) 
    tot_wealth #76 trillion dollars
    
    #if we leave each of the millionaires with exactly a million
    res_wealth <- tot_wealth - 10^6 * sum(tmpdf$weight[tmp])
    #we are left with
    res_wealth #60 trillion dollars 
    
    #now, fill in the bucket
    #COMMENTED CODE HELPED LOCATE THE POINT BELOW WHICH 
    #WE CAN USE THE ABOVE WEALTH TO FILL THE BUCKET
    # tmpranks<-19000:2000
    # tmpcalc <- sapply(tmpranks,function(i) {
    #   startrank<-i #ranking of person below which we will fill
    #     startworth <- tmpdf$networth[tmpdf$rank==startrank] #this is their net worth
    #   wealth_needed <- sum(
    #     #difference between that worth and the worth of each person below this in the distribution
    #     (startworth - tmpdf$networth[tmpdf$rank<=startrank]) * 
    #       #the number of people who have this worth in the United States
    #       tmpdf$weight[tmpdf$rank<=startrank]
    #   ) #the sum of this gives us the amount of wealth we would need to fill in the bucket
    #   wealth_needed - res_wealth #this is how much we need minus how much we have, after stripping millionaires
    # })
    # names(tmpcalc)<-tmpranks
    # tmpcalc<-tmpcalc[tmpcalc>0]
    # #this is the highest ranked person with whom we can fill the bucket below
    # #and still have some or zero net wealth left over
    # myrank<-(names(which(tmpcalc==min(tmpcalc))) %>% as.numeric) - 1
    
    #strip all millionaires of anything they have above a million
    tmpdf$networth[tmp]<-10^6
    
    #we start at this position
    startrank<-19024
    startworth <- tmpdf$networth[tmpdf$rank==startrank] #this is their net worth
    wealth_needed <- sum(
      #difference between that worth and the worth of each person below this in the distribution
      (startworth - tmpdf$networth[tmpdf$rank<startrank]) *
        #the number of people who have this worth in the United States
        tmpdf$weight[tmpdf$rank<startrank]
    ) #the sum of this gives us the amount of wealth we would need to fill in the bucket
    #we invariably have a little leftover, so we redistribute this leftover as well
    leftover <- res_wealth - wealth_needed  
    #this is the number of people to whom we're redistributing
    num_below <- sum(tmpdf$weight[tmpdf$rank<=startrank])
    leftover/num_below #we can add this to startworth
    
    #this is what everyone with less than startworth gets
    endowment <- startworth + leftover/num_below
    tmpdf$networth[tmpdf$rank<startrank] <- endowment
    
  } else if ( x=='better by patterned, status quo racial inequality') { #(2) 
    
    #here we want a transfer from the rich to the poor
    #but one which preserves the size of the racial gap
    #to do this, we will again strip rich of all races
    #but then redistribute within race
    
    #identify millionaires
    tmp <- tmpdf$networth>=10^6 
    
    #what rank in the white income distribution are the millionaires? 
    cutoff_white <- 10^6
    cutoff_white_rank<-tmpdf[networth==10^6 & race==1,min(round(networth_q_race))] #85
    #equivalent rank in black income disribution is about 220000
    cutoff_white_gap<-min(tmpdf[race==2,abs(networth_q_race-cutoff_white_rank)]) #eyeball, got +
    cutoff_black <- tmpdf[networth_q_race==(cutoff_white_rank+cutoff_white_gap) & race==2,min(networth)] 
    
    
    #now, redistribute the white stock of wealth to white people
    #and then, below, redistribute the black stock of wealth to black people
    
    #whites
    tmp_w <- tmpdf$networth>=cutoff_white & tmpdf$race==1
    #the stock of wealth
    tot_wealth_w <- sum(tmpdf$networth[tmp_w] * tmpdf$weight[tmp_w]) 
    #if we leave each of the millionaires with exactly a million
    res_wealth_w <- tot_wealth_w - cutoff_white * sum(tmpdf$weight[tmp_w])
    
    #blacks
    tmp_b <- tmpdf$networth>=cutoff_black & tmpdf$race==2
    #the stock of wealth
    tot_wealth_b <- sum(tmpdf$networth[tmp_b] * tmpdf$weight[tmp_b]) 
    #if we leave each of the millionaires with exactly a million
    res_wealth_b <- tot_wealth_b - cutoff_black * sum(tmpdf$weight[tmp_b])
    

    #now, fill in the bucket
    #COMMENTED CODE HELPED LOCATE THE POINTS BELOW WHICH 
    #WE CAN USE THE ABOVE WEALTH TO FILL THE BUCKET
    #whites
    # tmpranks<-11000:12399
    # tmpcalc <- sapply(tmpranks,function(i) {
    #   print(i)
    #   #startrank<-11001 
    #   startrank <- i #ranking of person below which we will fill
    #   startworth <- tmpdf$networth[tmpdf$rank_race==startrank & tmpdf$race==1] #this is their net worth
    #   wealth_needed <- sum(
    #     #difference between that worth and the worth of each person below this in the distribution
    #     (startworth - tmpdf$networth[tmpdf$rank_race<=startrank & tmpdf$race==1]) *
    #       #the number of people who have this worth in the United States
    #       tmpdf$weight[tmpdf$rank_race<=startrank  & tmpdf$race==1]
    #   ) #the sum of this gives us the amount of wealth we would need to fill in the bucket
    #   wealth_needed - res_wealth_w #this is how much we need minus how much we have, after stripping millionaires
    # })
    # names(tmpcalc)<-tmpranks
    # tmpcalc<-tmpcalc[tmpcalc>0]
    # #this is the highest ranked person with whom we can fill the bucket below
    # #and still have some or zero net wealth left over
    # myrank<-min((names(which(tmpcalc==min(tmpcalc))) %>% as.numeric) - 1)
    # #blacks
    # tmpranks<-1:tmpdf[race==2 & networth<cutoff_black,max(rank_race)]
    # tmpcalc <- sapply(tmpranks,function(i) {
    #   print(i)
    #   #startrank<-11001
    #   startrank <- i #ranking of person below which we will fill
    #   startworth <- tmpdf$networth[tmpdf$rank_race==startrank & tmpdf$race==2] #this is their net worth
    #   wealth_needed <- sum(
    #     #difference between that worth and the worth of each person below this in the distribution
    #     (startworth - tmpdf$networth[tmpdf$rank_race<=startrank & tmpdf$race==2]) *
    #       #the number of people who have this worth in the United States
    #       tmpdf$weight[tmpdf$rank_race<=startrank  & tmpdf$race==2]
    #   ) #the sum of this gives us the amount of wealth we would need to fill in the bucket
    #   wealth_needed - res_wealth_b #this is how much we need minus how much we have, after stripping millionaires
    # })
    # names(tmpcalc)<-tmpranks
    # tmpcalc<-tmpcalc[tmpcalc>0]
    # #this is the highest ranked person with whom we can fill the bucket below
    # #and still have some or zero net wealth left over
    # myrank<-min((names(which(tmpcalc==min(tmpcalc))) %>% as.numeric) - 1)
    
    
    #strip everyone above rich cutoff to the level of the cutoff
    tmpdf$networth[tmp_w]<-cutoff_white
    tmpdf$networth[tmp_b]<-cutoff_black

    #these are the ranks below which we fill in buckets
    #calculated using the commented-out code above
    myrank_whites <- 12306
    myrank_blacks <- 2547

    #do whites first
    startrank<-myrank_whites
    startworth <- tmpdf$networth[tmpdf$rank_race==startrank & tmpdf$race==1] #this is their net worth
    wealth_needed <- sum(
      #difference between that worth and the worth of each person below this in the distribution
      (startworth - tmpdf$networth[tmpdf$rank_race<startrank & tmpdf$race==1]) *
        #the number of people who have this worth in the United States
        tmpdf$weight[tmpdf$rank_race<startrank & tmpdf$race==1]
    ) #the sum of this gives us the amount of wealth we would need to fill in the bucket
    #we invariably have a little leftover, so we redistribute this leftover as well
    leftover <- res_wealth_w - wealth_needed  
    #this is the number of people to whom we're redistributing
    num_below <- sum(tmpdf$weight[tmpdf$rank_race<startrank & tmpdf$race==1])
    leftover/num_below #we can add this to startworth
    #this is what every white person with less than startworth gets
    endowment_w <- startworth + leftover/num_below
    tmpdf$networth[tmpdf$rank_race<startrank & tmpdf$race==1] <- endowment_w
    

    #do blacks next 
    startrank<-myrank_blacks
    startworth <- tmpdf$networth[tmpdf$rank_race==startrank & tmpdf$race==2] #this is their net worth
    wealth_needed <- sum(
      #difference between that worth and the worth of each person below this in the distribution
      (startworth - tmpdf$networth[tmpdf$rank_race<startrank & tmpdf$race==2]) *
        #the number of people who have this worth in the United States
        tmpdf$weight[tmpdf$rank_race<startrank & tmpdf$race==2]
    ) #the sum of this gives us the amount of wealth we would need to fill in the bucket
    #we invariably have a little leftover, so we redistribute this leftover as well
    leftover <- res_wealth_b - wealth_needed  
    #this is the number of people to whom we're redistributing
    num_below <- sum(tmpdf$weight[tmpdf$rank_race<startrank & tmpdf$race==2])
    leftover/num_below #we can add this to startworth
    #this is what every white person with less than startworth gets
    endowment_b <- startworth + leftover/num_below
    tmpdf$networth[tmpdf$rank_race<startrank & tmpdf$race==2] <- endowment_b
    

  } else if ( x=='status quo by patterned, racial equality') {
    
    #here, we want a white-black transfer among the rich
    #to which most patterned principles will be basically indifferent
    #but which will generate perfect racial equality

    #this defines the uber rich
    #defined by what would be needed to close the white-black wealth gap
    rich_cutoff <- 21 * 10^6 
    #identify white multi uber rich 
    tmp <- tmpdf$networth>(rich_cutoff) & tmpdf$race==1 
    num_whiterich <- sum(tmpdf$weight[tmp])
    #this is the stock of wealth they hold
    tot_wealth <- sum(tmpdf$networth[tmp] * tmpdf$weight[tmp]) 
    tot_wealth 
    
    #strip all white super rich of their wealth, up to the cutoff
    tmpdf$networth[tmp]<-rich_cutoff
    
    #we are left with, for redistribution
    res_wealth <- tot_wealth - (rich_cutoff) * sum(tmpdf$weight[tmp])
    
    #give it to black multi super rich, equally
    tmp2 <- tmpdf$networth>=(rich_cutoff) & tmpdf$race==2 #black millionaires
    num_blackrich <- sum(tmpdf$weight[tmp2])
    tmpdf$networth[tmp2] <-  res_wealth/num_blackrich
    
    #check that networths are the same
    tmpdf[
      ,
      weighted.mean(networth,weight)
      ,
      by=c('race')
    ]
    
    
  } else if ( x=='status quo by patterned, status quo racial inequality') {
    
    #status quo, nothing chnages
    
  }
  
  tmpdf$scenario<-x
  tmpdf
  
}) %>% rbind.fill %>% data.table

########################################################
########################################################

#REDO Q'S
tmpfunction<-function(x,w) {
  tmpf<-ewcdf(x,w)
  100 * tmpf(x)
}
simsdf[
  ,
  networth_q := tmpfunction(networth,weight)
  ,
  by=c('scenario')
]
simsdf[
  ,
  networth_race_q := tmpfunction(networth,weight)
  ,
  by=c('scenario','race')
]


########################################################
########################################################

setwd(filesdir); dir()
write.csv(
  simsdf,
  'equalitydf.csv',
  row.names=F
)

########################################################
########################################################
