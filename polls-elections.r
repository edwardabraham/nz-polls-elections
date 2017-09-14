# Data from the `nzelect` package (https://github.com/ellisp/nzelect)
# install.packages('nzelect')

library(nzelect)
library(ggplot2)
library(data.table)
polls <- data.table(polls)

poll <- polls[Pollster != 'Election result',]
result <- polls[Pollster == 'Election result',]
party <- merge(poll, result[, .(Party, 
	ElectionYear, ElectionDate=StartDate, 
	Vote=VotingIntention)], 
	by=c('Party', 'ElectionYear'), all=F)

party[, DaysBefore := as.numeric(ElectionDate - EndDate, units='days')]
party[, VoteDiff := 100*( VotingIntention - Vote)]

partystats <- merge(party, party[DaysBefore < 20 & Vote > 0.05, 
	.(mean=mean(VoteDiff), lower=quantile(VoteDiff, 0.1), upper=quantile(VoteDiff, 0.9)), 
	by=Party])

ggplot(partystats[DaysBefore < 20 & Vote > 0.05], aes(x=DaysBefore, y=VoteDiff, colour=Party)) + 
	geom_point(size=3, alpha=0.5) + 
	facet_wrap(~Party) + 
	scale_colour_manual(values=parties_v) + 
	geom_hline(yintercept=0, colour='#AAAAAA') + 
	xlab('Poll period end, days before election') + 
	ylab('Poll (%) -  election result (%)') + 
	theme_bw() + 
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.spacing.x=unit(1,"lines")) +
	scale_x_reverse(breaks=seq(0, 20, by=2), limits=c(20, 0), expand=c(0, 0)) + 
	geom_ribbon(alpha=0.3, linetype=0, aes(x=ifelse(DaysBefore > 10, 20, 0), ymin=lower, ymax=upper, fill=Party)) +
	scale_fill_manual(values=parties_v) + 
	guides(colour=FALSE) + 
	guides(fill=FALSE) 

ggsave(width=6, height=4, file='polls.png', device='png')

