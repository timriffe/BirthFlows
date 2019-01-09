
# Author: tim
###############################################################################

distdiff <- function(D1, D2){
	D1 <- rescale.vector(D1)
	D2 <- rescale.vector(D2)
	
	sum(abs(D1 - D2))/2
}

# get all the distributions:
AP <- LexisUtils::PC2AP(PC,agemin=12,agemax=55,Lexis = 2)
p  <- 1736:2028
AP <- AP[,as.character(p)]

# distributions SD < 5
# distributions SD > 6.5

# low
matplot(12:55,AP[,as.character(Yrs[Per_SD < 5 & !is.na(Per_SD)])],type='l')
# high
matplot(12:55,AP[,as.character(Yrs[Per_SD > 6.6 & !is.na(Per_SD)])],type='l')
