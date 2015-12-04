###
### Data wrangling project

setwd("/Users/rameshcalamur/dev/Datascience/SlideRule/Module3")
## libraries to use
suppressMessages(library(dplyr))
library(reshape2)
library(ggplot2)
### Load the data sets - sub.txt is tab limited
subs <- read.delim("sub.txt")
subs2 <- subs %>% select(adsh,cik,name,sic,period)
### check how many submissions have SIC code 
subs2 %>% group_by(sic) %>% summarise(cnt = n()) %>% arrange(desc(cnt))
## identify companies with 'sic' == NA
companiesWithoutSic <- subs2 %>% filter(is.na(sic))  %>% select(cik) %>% unique

### submissions with a subset of columns
subs3 <- subs2 %>% select(adsh,cik,sic)

## Load the nums data set - contains the values
## num has data columns: 
##  1) adsh- EDGAR accession number 2) tag – tag used by the filer
##  3) version – if a standard tag, the taxonomy of origin, otherwise equal to adsh.
##  4) ddate - period end date 5) qtrs - duration in number of quarters
##  6) uom - unit of measure 7) coreg - coregistrant of the parent company registrant (if applicable)
## NOTE: tag is the variable for our data analysis and num.txt file has that as cell under column tag and column value per row (
nums <- read.delim("num.txt")

## Check which tags occur most in the data set
nums %>% group_by(tag) %>% summarise(cnt = n()) %>% arrange(desc(cnt)) %>% head(10)

nums %>% group_by(tag) %>% summarise(cnt = n()) %>%  arrange(desc(cnt)) %>% filter(cnt > 10000)
### numeric data and SIC codes only
numsNsic <- inner_join(nums,subs3)

## nums.txt has data in long format, need to convert to wide-format
## for testing, work with a subset, cit == 2178
numstst <- numsNsic %>% filter(cik == 2178)


numsx <- dcast(numstst,adsh + cik + sic ~ tag, value.var="value")

### filter a desired set of columns
tgtags <- c("Revenues","NetIncomeLoss", "Assets", "Liabilities")
numstst %>% filter(tag %in% tgtags)

### filter tags in the tgtags from the
numstst %>% filter(tag %in% tgtags) %>% dcast(adsh + cik + sic + version + ddate  ~ tag, fun.aggregate = sum, value.var="value")

tags2 <- c("AssetsCurrent", "LiabilitiesCurrent")
numstst %>% filter(tag %in% tags2) %>% dcast(adsh + cik + sic + version + ddate  ~ tag, fun.aggregate = sum, value.var="value") %>% mutate( curRatio = AssetsCurrent / LiabilitiesCurrent)

####
#### Data preparation for calculating current ratio
curtags2 <- c("AssetsCurrent", "LiabilitiesCurrent")
numCurRatio <- numsNsic %>% filter(tag %in% curtags2) %>% dcast(adsh + cik + sic + version + ddate  ~ tag, fun.aggregate = sum, value.var="value") %>% mutate( curRatio = AssetsCurrent / LiabilitiesCurrent)

### get the ddate  into period as a date
tags2 <- c("AssetsCurrent", "LiabilitiesCurrent")

numCurRatio <- numsNsic %>% filter(tag %in% tags2) %>% dcast(adsh + cik + sic + version + ddate  ~ tag, fun.aggregate = sum, value.var="value") %>% mutate( period = as.Date(as.character(ddate),"%Y%m%d"), curRatio = AssetsCurrent / LiabilitiesCurrent)

## find out how many NAs in current ratio
sum(is.na(numCurRatio$curRatio))

sum(is.na(numCurRatio$AssetsCurrent))

sum(is.na(numCurRatio$LiabilitiesCurrent))


###
### Current ratio grouped by SIC - statistics, filtering out Infinite values
numCurRatio %>% group_by(sic) %>% summarise( avgCR = mean(curRatio), maxCR = max(curRatio), minCR = min(curRatio), sdCR = sd(curRatio) ) %>% filter( !is.infinite(avgCR)) %>% arrange(desc(avgCR))

### plot the Standard deviation of Current ratio grouped by SIC
numCurRatio %>% group_by(sic) %>% summarise( avgCR = mean(curRatio), maxCR = max(curRatio), minCR = min(curRatio), sdCR = sd(curRatio) ) %>% filter( !is.infinite(avgCR)) %>% arrange(desc(avgCR)) %>% qplot(sic,sdCR,data = ., geom="point")

#### plot the current ratio for selected SIC
numCurRatio %>% filter(sic == 100)  %>% filter( !is.infinite(curRatio))%>%  qplot(cik,curRatio,data = ., geom="point")

#### Top 10 SICs with current ratio
top10sics <- numCurRatio %>% group_by(sic) %>% filter(!is.na(sic))  %>% summarize( cnt = n()) %>% arrange(desc(cnt)) %>% select(sic) %>% head(10)

### current ratios of CIKs for Top 10 SICs
numCurRatio %>% filter(sic %in% top10sics$sic)  %>% filter( !is.infinite(curRatio)) %>% head(10)

### SIC as factor for plotting SIC as lines
numCurRatio <- numCurRatio %>% mutate( fsic = as.factor(sic))

###plot the current ratio with different SIC as colors
numCurRatio %>% filter(sic %in% top10sics$sic) %>% filter(!is.infinite(curRatio))%>%  qplot(cik,curRatio,data = ., color = fsic, geom="point")
