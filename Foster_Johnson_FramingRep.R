## Replication script for:
## Foster and Johnson,
## "Rhetorical Framing in Inter-Governmental Negotiations"

rm(list=ls())

loadPkg=function(toLoad){
  for(lib in toLoad){
    if(! lib %in% installed.packages()[,1])
    {install.packages(lib,
                      repos='http://cran.rstudio.com/')}
    suppressMessages( library(lib, character.only=TRUE))}}


packs <- c('pdftools',
           "tidyr",
           "tidytext",
           "ggplot2", 
           "tidyverse", 
           "xtable",
           'Hmisc', 
           "ggridges")

loadPkg(packs)

#########################
## Declare Data Paths
#########################

dataPath <- "./data/"
imagePath <- "./images/"

#############################
## Load Processed Text Data
#############################

##
data <- readxl::read_excel(paste0(dataPath,
                                  "WTOSpeakerTurnsM1to113.xlsx"))

## Clean up naming styles:

colnames(data)[which(colnames(data)=="income_level_iso3c")] <- "income_level"
colnames(data)[which(colnames(data)=="meeting.end")] <- "meeting_end"

## Capture paragraph ordering
## Before filtering out the non-state delegations

colnames(data)[which(colnames(data)=="income_level_iso3c")] <- "income_level"
## AGG income into HIC
data[which(data$income_level=="AGG"), 
     "income_level"] <- "HIC"

## Consolidate "ADMN" (a few Chair and Committee) and "NOTST" (Most of them)
data[which(data$income_level=="ADMN"), 
     "income_level"] <- "NOTST"

table(data$income_level)

## Remove the one random row with NA in the info
data <- data %>%
  filter(!region=="NA") 

## Remove rows that occur after the "meeting end" flag (eg: appendices, reports, seminar notes)

## Tag everything after the end:
## Identify the paragraph number that the meeting ended on:
dat3 <- data[which(data$meeting_end > 0), c("meetingno", "paranum", "meeting_end")]

## use that paragraph number to create a cutpoint
## and fill anything after it, given the meeting, 
## with an excise tag:
data$toremove <- 0
for(r in 1:nrow(dat3)){
  m= as.numeric(dat3[r, "meetingno"]) ##meetingno
  e=as.numeric(dat3[r, "paranum"])  ##max paranum
  ##print(paste0(m," , ", e))
  data[which(data$meetingno== m &
               data$paranum> e), "toremove"] <-1
}

table(data$toremove) ## about 35 paragraphs after the flag

data <- data %>%
  filter(!toremove ==1)

## Modeling the sequences by income level:

## Create a lag feature, so we can quantify sequences
## (What income-level preceded each turn):

data <- data %>%
  arrange(meetingno) %>%
  group_by(meetingno) %>%
  arrange(paranum, .by_group = TRUE) %>%
  mutate(prev_speaker.inc= lag(income_level, order_by = meetingno)) %>%
  mutate(prev_speaker_ent= lag(firstent, order_by = meetingno))

## Now, see which blocks tend to follow others:
table(data[which(data$income_level=="HIC"), "prev_speaker.inc"])

## Framing/Response patterns:
h1 <- round(prop.table(table(data[which(data$income_level=="HIC"), "prev_speaker.inc"])), 3)
u1 <- round(prop.table(table(data[which(data$income_level=="UMC"), "prev_speaker.inc"])), 3)
l1 <- round(prop.table(table(data[which(data$income_level=="LMC"), "prev_speaker.inc"])), 3)
l2 <- round(prop.table(table(data[which(data$income_level=="LIC"), "prev_speaker.inc"])), 3)

n <- c("HIC", "UMC", "LMC", "LIC")

seqs <- as.data.frame(cbind(n, rbind(h1, u1, l1, l2)))
seqs$n <- as.factor(seqs$n)

## This isn't actually in the paper, but it's interesting!
print(xtable(seqs, caption = "Ordering of turns, by income level"),
      include.rownames=FALSE)

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Now just looking at Delegation content:
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ctm <- data%>% ## 5215 observations
  filter(!region %in% c("NOTST", "ADMN"))

dim(ctm) ##5115 in the STM processed data, 5215 in the fresh download?
## The differences are in cleaned up duplications; aggregations;
## and the ~35 annex speaker-turns that I cleaned up.


unique(ctm[which(ctm$region=="Aggregates"), "firstent"])
## "Aggregates" region income into "Europe & Central Asia" (B/c means EU)
ctm[which(ctm$region=="Aggregates"), 
    "region"] <- "Europe & Central Asia"

table(ctm$region)
table(ctm$income_level)


##%%%%%%%%%%%%%%%%%%%%%%
## Expectation: Coalitions by weaker states:
##%%%%%%%%%%%%%%%%%%%%%%%

## On behalf of:
## Looking only in the first 100 characters
## To avoid a speaker responding to a statement from a different speaker, on behalf of
## (Happen most with references to  statements by South Africa, On behalf of African Group)
ctm$behalf <- 0
ctm[which(grepl(pattern="on behalf of", 
                x=substr(ctm$paratext, 1, 100)) == TRUE), "behalf"] <-1 


table(ctm$behalf) ## 4945 no; 222 yes

## write this table to get an idea of 
## what the "on behalf of" looks like
ctm$behalf_of <- "None"
ctm[which(grepl(pattern="on behalf of the LDC*", ctm$paratext) == TRUE), "behalf_of"] <-"LDC Group"  
ctm[which(grepl(pattern="on behalf of * least-developed countries", ctm$paratext) == TRUE), "behalf_of"] <-"LDC Group" 
ctm[which(grepl(pattern="on behalf of the African Group*", ctm$paratext) == TRUE),
    "behalf_of"] <-"African Group"
ctm[which(grepl(pattern="on behalf of the Steering Committee*", ctm$paratext) == TRUE),
    "behalf_of"] <-"Steering Committee"
ctm[which(grepl(pattern="on behalf of the LAIA", ctm$paratext) == TRUE),
    "behalf_of"] <-"LAIA"
ctm[which(grepl(pattern="on behalf of the Latin American Integration Association",
                ctm$paratext) == TRUE), "behalf_of"] <-"LAIA"
ctm[which(grepl(pattern="on behalf of the LDC*", ctm$paratext) == TRUE), "behalf_of"] <-"LDC Group"
ctm[which(grepl(pattern="on behalf of the SVE", ctm$paratext) == TRUE), "behalf_of"] <-"SVE Group"
ctm[which(grepl(pattern="on behalf of the small and vulnerable economies ",
                ctm$paratext) == TRUE), "behalf_of"] <-"SVE Group"
ctm[which(grepl(pattern="on behalf of the six countries ",
                ctm$paratext) == TRUE), "behalf_of"] <-"OECS Countries"
ctm[which(grepl(pattern="on behalf of the LLDC Group*", ctm$paratext) == TRUE),
    "behalf_of"] <-"LLDC Group"
ctm[which(grepl(pattern="on behalf of ASEAN", ctm$paratext) == TRUE), "behalf_of"] <-"ASEAN"
ctm[which(grepl(pattern="on behalf of the Arab Group", ctm$paratext) == TRUE),
    "behalf_of"] <-"Arab Group"
ctm[which(grepl(pattern="on behalf of the Arab Members", ctm$paratext) == TRUE),
    "behalf_of"] <-"Arab WTO Members"
ctm[which(grepl(pattern="on behalf of the GCC", ctm$paratext) == TRUE), "behalf_of"] <-"GCC"
ctm[which(grepl(pattern="on behalf of the Participating States of APTA", ctm$paratext) == TRUE), 
    "behalf_of"] <-"APTA States"
ctm[which(grepl(pattern="on behalf of the member countries of the East African Community",
                ctm$paratext) == TRUE), "behalf_of"] <-"EAC Countries"
ctm[which(grepl(pattern="on behalf of the ACP Group", ctm$paratext) == TRUE), 
    "behalf_of"] <-"ACP Group"
ctm[which(grepl(pattern="on behalf of the GRULAC",
                ctm$paratext) == TRUE), "behalf_of"] <-"GRULAC"
ctm[which(grepl(pattern="on behalf of a core group of donors *", ctm$paratext) == TRUE),
    "behalf_of"] <-"Donors"
ctm[which(grepl(pattern="on behalf of the Non-Resident Members and Observers ",
                ctm$paratext) == TRUE), "behalf_of"] <-"Non-Residents"
ctm[which(grepl(pattern="on behalf of the MERCOSUR",
                ctm$paratext) == TRUE), "behalf_of"] <-"MERCOSUR Countries"

ctm[which(grepl(pattern="on behalf of the Informal Group of Developing Countries",
                ctm$paratext) == TRUE), "behalf_of"] <-"IGDC Countries"

## Other
ctm[which(ctm$behalf == 1 & ## 34
            ctm$behalf_of=="None"), "behalf_of"] <- "Other" 

## Substantive notes: 
## The casest that fall into "Other" are often coalitions of a few states
## Tonga "on behalf of on behalf of Fiji, Papua New Guinea and Solomon Islands" (M72)

## Sometimes contextual: 
## Brazil, on behalf of the proponents of (M108)
## Occasionally there are more than one, eg
## Egypt on behalf of the African and Arab Groups (M84)


table(ctm$behalf)
table(ctm$behalf_of) ##103 LDC,; 45 African Group; 23 ASEAN; etc...

## Expectation two

## Chi square test, null hypothesis that the distributions are 
## not distinguishable from a random distribution

inc.behalf <- table(ctm$behalf, ctm$income_level)

cs.test <- chisq.test(inc.behalf)
cs.test ## P < .001, so rejects 

## Make into one-hot encoding:
data.cor <- ctm[,c("behalf","income_level")]
colnames(data.cor) <- c("OnBehalf", "income_level")
data.cor <- mutate(data.cor, LIC = ifelse(income_level=="LIC",1,0))
data.cor <- mutate(data.cor, LMC = ifelse(income_level=="LMC",1,0))
data.cor <- mutate(data.cor, UMC = ifelse(income_level=="UMC",1,0))
data.cor <- mutate(data.cor, HIC = ifelse(income_level=="HIC",1,0))

c.df <- cor(data.cor[,c("OnBehalf", "LIC", "LMC", "UMC", "HIC")])

## Print the correlations:
capt = "Correlations between delegation income category and speaking for others"

print(xtable(c.df,
      caption=capt))

## Processing for Manuscript Figure 6

cf2 <- c.df[1,]

cf2 <- rbind(cf2, names(cf2))

cf2 <- as.data.frame(t(cf2))
colnames(cf2) <- c("corr", "inc.lev")

cf2 <- cf2 %>%
  filter(!inc.lev=="OnBehalf")

cf2$corr <- as.numeric(cf2$corr)
cf2$inc.lev <- factor(cf2$inc.lev,
                      levels=c("LIC", "LMC", "UMC", "HIC"))

cf2$inc.lev2 <- factor(cf2$inc.lev)

cf2$inc.lev2 <- recode(cf2$inc.lev2,
                       LIC = "Low Income", 
                       LMC = "Lower Middle Income",
                       UMC = "Upper Middle Income",
                       HIC = "High Income")

## Greyscale for print version:
p_gray <- ggplot(data=cf2, 
            aes(x=inc.lev2,
                y=corr,
                fill=inc.lev2)) +
  geom_bar(stat="identity")+
  scale_fill_grey() +
  theme_bw()+
  ylim(c(-.2, .2))+
  ylab("Correlation with 'On Behalf Of' Tag") +
  xlab("Delegation World Bank Income Classification") +
  theme(legend.position = "none") +
  geom_hline(yintercept=0, linetype="dashed", color = "black")

p_gray

ggsave(p_gray,
       width=8,
       height=6,
       file=paste0(imagePath,"correlationBar.pdf"))


## Color for repository:
p_color <- ggplot(data=cf2, 
                 aes(x=inc.lev2,
                     y=corr,
                     fill=inc.lev2)) +
  geom_bar(stat="identity")+
  theme_bw()+
  ylim(c(-.2, .2))+
  ylab("Correlation with 'On Behalf Of' Tag") +
  xlab("Delegation World Bank Income Classification") +
  theme(legend.position = "none") +
  geom_hline(yintercept=0, linetype="dashed", color = "black")

p_color

ggsave(p_color,
       width=8,
       height=6,
       file=paste0(imagePath,"correlationBar_color.png"))

##Working only in the subset of "on behalf of" references:
behalf.df <- ctm[which(ctm$behalf==1),]

## Correlation between using "on behalf of"
## and wealth:
table(behalf.df$income_level)
table(ctm$behalf) ## 0, 1
table(ctm$income_level) # HIC, LIC, LMC, UMC
aov1 = aov(ctm$behalf ~ ctm$income_level)
summary(aov1) ## Statistically significant at P < .001


##%%%%%%%%%%%%%%
## On Behalf of Visualizations:
##%%%%%%%%%%%%%%
behalf.sum <- behalf.df %>%
  group_by(year) %>%
  count(behalf_of)

## "On Behalf Of" LDCs:
## LDCs, SVEs

interest <- c("LDC Group")
ggplot(behalf.sum[which(behalf.sum$behalf_of %in% interest),],
       aes(x=year,
           y=n)) +
  geom_line() +
  theme_bw()+
  ylab("Frequency of Behalf LDC Group References") +
  xlab("Year")+
  theme(axis.text.x = element_text(angle = 45))+ 
  scale_x_continuous(breaks=seq(1995, 2020, by=1)) ## note refs start in 2002

## SVES:

hicsves <- c("Antigua and Barbuda",  "Barbados", "Panama",
             "Saint Kitts and Nevis", "Seychelles", 
             "Trinidad and Tobago")
LMCsvss <- c("Belize", " Bolivia", "Cabo Verde","El Salvador", 
             "Honduras", "Mauritania", "Mongolia", "Nicaragua", 
             "Papua New Guinea", "Samoa" , "Sri Lanka", "Vanuatu")
UMCsves <- c(" Cuba", "Dominica", "Dominican Republic",  "Ecuador", 
             " Fiji", " Grenada", "Guatemala", "Guyana", 
             "Jamaica", "Maldives", "Mauritius",
             "Saint Lucia", "Saint Vincent and the Grenadines",
             "Tonga")

table(ctm$behalf_of)

ctm[which(ctm$behalf_of=="SVE Group"), c("meetingno", "firstent", "paratext")]

ctm[which(ctm$behalf_of=="LDC Group"), c("meetingno", "firstent", "paratext")]

table(ctm[which(ctm$behalf_of=="LDC Group"),
          "income_level"])

table(ctm[which(ctm$behalf_of=="African Group"),
          "income_level"])

table(ctm[which(ctm$behalf_of=="LDC Group"),
          "firstent"])

###%%%%%%%%%%%%%%%%%%%
## Visualizations by-meeting turns
##%%%%%%%%%%%%%%%%%%%%

dim(ctm) ## 
colnames(ctm)

tokeep <- c("meetingno", "paranum", "income_level",
            "firstent")

## choose meeting to highlight:
set.seed(090422)
ceiling(runif(n=3, min=1, max=113)) ##28, ##74, ## 84

selectms <- c(28, 74, 84)

ctmselect <- ctm[,tokeep]%>%
  filter(meetingno %in% selectms)

## create a marker for end of meeting
## inc admin
summary(data[which(
  data$meetingno==28),]$paranum) ## Inc admin, ends on paraturn 96

summary(data[which(
  data$meetingno==74),]$paranum) ## inc admin, ends on paraturn 32

summary(data[which(
  data$meetingno==84),]$paranum) ## Inc admin, ends on paraturn 87

## Marker for end of meeting, including admin data:
ctmall.end <- data[data$meetingno %in% selectms,
                   c("meetingno", "paranum")] %>%
  group_by(meetingno) %>% slice(which.max(paranum)) ## paranums 96, 32, 87

ctmselect.end <- ctmselect %>%
  group_by(meetingno) %>% slice(which.max(paranum)) ## paranums 91, 31, 86

ctmselect.end

## "Weak" vs "strong" descriptors:

weak <- c("LIC", "LMC", "UMC")
ctmselect$strength <- NA
ctmselect[which(ctmselect$income_level%in% weak), "strength"] <- "Weak States"
ctmselect[which(ctmselect$income_level=="HIC"), "strength"] <- "Strong States"

ctmselect$strength <- factor(ctmselect$strength,
                             levels=c("Weak States", "Strong States"))

summary(ctmselect$strength)
summary(ctmselect$meetingno)

## Grayscale for manuscript
## Figure 3
gg2_gray <- ggplot(data=ctmselect,
              aes(x=paranum,
                  y=meetingno,
                  fill=strength))+
  geom_bar(position="fill",
           stat="identity") +
  scale_fill_grey(start = .33, end = .66)+
  theme_bw()+
  theme(legend.position="bottom") +
  theme(axis.text.y=element_blank(),
        axis.ticks.y = element_blank()) +
  scale_x_continuous(name="Speaker Turn",
                     limits=c(0, 85)) +
  ylab("Meeting") +
  xlab("Speaker Turn") +
  facet_wrap(~meetingno, 
             nrow=3, 
             ncol=1) +
  labs(fill='Speaker Strength') 
 
gg2_gray 

ggsave(gg2_gray,
       file=paste0(imagePath, "threeSequence.pdf"))

## Color for repository:
gg2_color <- ggplot(data=ctmselect,
                   aes(x=paranum,
                       y=meetingno,
                       fill=strength))+
  geom_bar(position="fill",
           stat="identity") +
  theme_bw()+
  theme(legend.position="bottom") +
  theme(axis.text.y=element_blank(),
        axis.ticks.y = element_blank()) +
  scale_x_continuous(name="Speaker Turn",
                     limits=c(0, 85)) +
  ylab("Meeting") +
  xlab("Speaker Turn") +
  facet_wrap(~meetingno, 
             nrow=3, 
             ncol=1) +
  labs(fill='Speaker Strength')+
  scale_fill_manual(values=c("#9ebcda", "#8856a7"))

## For repository
ggsave(gg2_color,
       file=paste0(imagePath, "threeSequenceColor.png"))


##%%%%%%%%%%%%%%%
## Variation in number of speakers by year:
##%%%%%%%%%%%%%%%

length(unique(ctm$firstent)) ##123
length(unique(ctm$iso3c)) ##123

## Are there differences by income:
## i.e.: heterogeneous effects
yearvariety2 <- ctm %>%
  group_by(year, income_level) %>%
  summarise(count_distinct = n_distinct(firstent))

yv2 <-  ggplot(yearvariety2,
       aes(x=year,
           y=count_distinct)) +
  geom_line() +
  theme_bw()+
  ylab("Number of Delegations Speaking") +
  xlab("Year")+
  theme(axis.text.x = element_text(angle = 90))+
  scale_x_continuous(breaks=seq(1995, 2020, by=1))+
  facet_wrap(~income_level)

## For repository
ggsave(yv2,
       file=paste0(imagePath, "yearvar2.png"))


## %%%%%%%%%%%%%%%%%%
##Visualizations:
## %%%%%%%%%%%%%%%%%%

## Prevalence in speaker-turns over time:
## "Time" here conceptualized as meeting number

## put AGG income into HIC:

ctm[which(ctm$income_level=="AGG"), "income_level"] <- "HIC"

timesum <- ctm %>%
  group_by(meetingno, income_level) %>%
  summarise(n=n()) %>%
  mutate(percentage = round( n / sum(n),2))

timesum$income_level <- factor(timesum$income_level,
                               levels=c("LIC", "LMC", "UMC", "HIC"))

ggplot(timesum,
       aes(x=meetingno, 
           y=percentage, 
           fill=income_level)) + 
  geom_area() +
  theme_bw()+
  ylab("Percentage of Speaker Turns")+
  scale_x_continuous("Meeting Number",
                     breaks =seq(from=1, 
                                 to=113, 
                                 by=1)) +
  labs(fill="Income Level") +
  scale_fill_grey() +
  theme(axis.text.x = element_text(angle = 45, 
                                   size=5))

## By year:
timesum.year <- ctm %>%
  group_by(year, income_level) %>%
  summarise(n=n()) %>%
  mutate(percentage = round( n / sum(n),2))

timesum.year$income_level <- factor(timesum.year$income_level,
                                    levels=c("LIC", "LMC", "UMC", "HIC"))
## All income levels:
gg.year <- ggplot(timesum.year,
                  aes(x=year, 
                      y=percentage, 
                      fill=income_level)) + 
  geom_area() +
  theme_bw()+
  scale_fill_manual(values=c('#a6cee3','#1f78b4','#b2df8a','#33a02c'))+
  ylab("Percentage of Speaker Turns")+
  scale_x_continuous(breaks =seq(from=1995,
                                 to=2020, 
                                 by=1)) +
  labs(fill="Income Level") +
  theme(axis.text.x = element_text(angle = 45, 
                                   size=5))+
  theme(legend.position="bottom")

gg.year
## Color for repository:
ggsave(gg.year,
       file=paste0(imagePath, "incomebyyear.png"))

## Manuscript Figure 3:
## By year, with just HIC vs rest:

not.hic <- c("LIC", "LMC", "UMC")
timesum.year$bimode <-"UNKN"
timesum.year[which(timesum.year$income_level=="HIC"), "bimode"] <-"Strong States"
timesum.year[which(timesum.year$income_level %in% not.hic), "bimode"] <-"Weak States"

timesum.year$bimode <- factor(timesum.year$bimode,
                              levels=c("Weak States", "Strong States"))



timesum.bimode <- timesum.year %>%
  group_by(year, bimode) %>%
  mutate(bipercent = sum(percentage))

timesum.bimode <- unique(timesum.bimode[,c("year", "bimode", "bipercent")])


gg.yearbimode <- ggplot(timesum.bimode,
                        aes(x=year, 
                            y=bipercent, 
                            fill=bimode)) + 
  geom_area() +
  theme_bw()+
  ylab("Percentage of Speaker Turns")+
  xlab("Year")+
  scale_x_continuous(breaks =seq(from=1995,
                                 to=2020, 
                                 by=1)) +
  labs(fill="Speaker Strength") +
  scale_fill_grey() +
  theme(axis.text.x = element_text(angle = 45, 
                                   size=5)) +
  theme(legend.position="bottom")

gg.yearbimode

ggsave(gg.yearbimode,
       file=paste0(imagePath,"incomebyyearbimode.pdf"))


## clean up a bit:  
rm(timesum)
rm(timesum.year)

## Manuscript Figure 7
## Ridge Plot for Distribution of Position:
## Within meeting sequence

colnames(ctm)

ctm$income_level <- factor(ctm$income_level,
                           levels=c("LIC", "LMC", "UMC", "HIC"))

## Paragraph location, scaled to meeting length:
meetingmax <- ctm %>%
  group_by(meetingno) %>%
  summarise(meetingmax = max(paranum))

summary(meetingmax$meetingmax) ## 5-306

meetingmax[which(meetingmax$meetingmax==5),] ## Meeting 3; qualitative confirmed that meeting 3 is very short!

tmp2 <- ctm %>%
  left_join(meetingmax, 
            by=c("meetingno"))

colnames(tmp2)

tmp2$posinmeeting <- round(tmp2$paranum/tmp2$meetingmax, 3)

summary(tmp2$posinmeeting)

## Figure 7
## Grayscale for manuscript:
posgg <- ggplot(tmp2,
                aes(x=posinmeeting,
                    y=income_level,
                    fill=income_level))+
  geom_density_ridges(alpha=.75)+
  scale_fill_grey()+
  ylab("Delegation Income Level")+
  xlab("Distribution of Position in Meeting Minutes")+
  labs(fill="Income Level") +
  theme_bw()+
  theme(legend.position="bottom")

posgg

ggsave(posgg,
       file= paste0(imagePath,"speakerTurnsPosition.pdf"))

## Color for repository:

posggc <- ggplot(tmp2,
                aes(x=posinmeeting,
                    y=income_level,
                    fill=income_level))+
  geom_density_ridges(alpha=.75)+
  scale_fill_manual(values=c('#a6cee3','#1f78b4','#b2df8a','#33a02c'))+
  ylab("Delegation Income Level")+
  xlab("Distribution of Position in Meeting Minutes")+
  labs(fill="Income Level") +
  theme_bw()+
  theme(legend.position="bottom")

posggc

ggsave(posggc,
       file= paste0(imagePath,
                    "speakerTurnsPosition.png"))

## Next: introduce early/middle/end cutpoints
## and look at the percentage of each part 
## that are dominated by the income levels


