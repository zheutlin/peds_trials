##################  Pediatric: Phase of Clinical Trials ###################

##########  LOAD PACKAGES  ########## 

library(tidyverse)
library(dplyr) 
library(ggplot2)  
library(kableExtra)
library(ggthemes)
library(viridis) 
library(lubridate)
library(RISmed)
library(ggsci)

##########  ACQUIRE DATASET  ########## 

### Defined Search Paramaters for General
srch_peds_RCT <- "((Randomized Controlled Trial[ptyp])) AND ((2006/10/21[PDat]:2017/11/13[PDat])) AND ((Child[Mesh] OR Infant[Mesh] OR Adolescent [Mesh]) NOT ((Adult[Mesh])"

srch_peds_p1 <- "((Clinical Trial, Phase I[ptyp])) AND ((2006/10/21[PDat]:2017/11/13[PDat])) AND ((Child[Mesh] OR Infant[Mesh] OR Adolescent[Mesh]) NOT ((Adult[Mesh])"

srch_peds_p2 <- "((Clinical Trial, Phase II[ptyp])) AND ((2006/10/21[PDat]:2017/11/13[PDat])) AND ((Child[Mesh] OR Infant[Mesh] OR Adolescent[Mesh]) NOT ((Adult[Mesh])"

srch_peds_p3 <- "((Clinical Trial, Phase III[ptyp])) AND ((2006/10/21[PDat]:2017/11/13[PDat])) AND ((Child[Mesh] OR Infant[Mesh] OR Adolescent[Mesh]) NOT ((Adult[Mesh])"

srch_peds_p4 <- "((Clinical Trial, Phase IV[ptyp])) AND ((2006/10/21[PDat]:2017/11/13[PDat])) AND ((Child[Mesh] OR Infant[Mesh] OR Adolescent[Mesh]) NOT ((Adult[Mesh])"

### Build function to scrape Pubmed into S4 
scrape_pubmed <- function (x) {
  x %>%
    EUtilsSummary(
      retmax=40000, 
      mindate= 1995,
      maxdate= 2017,
      datetype = "EPDT") %>% 
    EUtilsGet(type = "efetch", db = "pubmed")
}

## Build function to pull relevant data from S4 into list of lists 
make_list <- function (x) {
  list('PMID' = PMID(x),
       'Year' = YearPubmed(x), 
       'Month' = MonthPubmed(x),
       'Day' = DayPubmed(x))
}

### Build function to generate dataframe from lists 
extract_data <- function (x) {
  pmap_dfr(x, ~data.frame(
    PMID = .x, 
    year = .y, 
    month = ..3, 
    day = ..4, 
    stringsAsFactors = FALSE))
}

### Build function to create scrape, list, and dataframe process

gen_df <- function(x) {
  x %>% 
    scrape_pubmed() %>% 
    make_list() %>% 
    extract_data()
}


### Extract relevant dataframes 
df_rpeds1 <- gen_df(srch_peds_p1)
df_rpeds1$Field<-c("Phase 1")
df_rpeds2 <- gen_df(srch_peds_p2)
df_rpeds2$Field<-c("Phase 2")
df_rpeds3 <- gen_df(srch_peds_p3)
df_rpeds3$Field<-c("Phase 3")
df_rpeds4 <- gen_df(srch_peds_p4)
df_rpeds4$Field<-c("Phase 4")

##########  CLEAN DATASETS  ########## 
### Bind dataframes together 
#df <- rbind(df_rRCT)
df_peds<-rbind(df_rpeds1, df_rpeds2, df_rpeds3, df_rpeds4)


#write_csv(df, "RCT Peds - RCT Phase.csv")
write_csv(df_peds, "Pediatric RCT Project - Phase Extension.csv")

# Generate Date-based Time Variable

#df$date <- paste(df$year, df$month, df$day, sep="-")
#df$date <- ymd(df$date)

df_peds$date <- paste(df_peds$year, df_peds$month, df_peds$day, sep="-")
df_peds$date <- ymd(df_peds$date)

# Create intervals for time variable 

#df <- df %>% 
#  mutate(monthly = as.Date(cut(df$date, breaks = "month"))) 

df_peds <- df_peds %>% 
  mutate(monthly = as.Date(cut(df_peds$date, breaks = "month"))) 

##########  Overall Numbers  ########## 

#df %>%
#  group_by(kind) %>%
#  count() 

# Total RCTS ***

df_peds %>% 
  group_by(phase) %>%
  count()

#Fact Check 

class(df_peds$Field)

##########  Figure 1: RCTs By Clinical Phase Over Time ########## 
Figure1<- df_peds %>% 
  mutate(yearly = year(date)) %>%
  group_by(yearly, phase) %>%
  count() %>% 
  filter(yearly <= 2017)%>%
  ggplot(aes(
    x = yearly,
    y = n, 
    col = phase)) +
  geom_line() +
  theme_classic(
    base_size = 16) + theme(axis.text.x = element_text(angle=0, hjust=1))  +
  scale_color_jama() +
  labs(
    title = "\nFigure 1: Phase of Pediatric Clinical Trials in the Published Literatre, 2007-2017", 
    subset = "Stratified by Phase of Study, n = XXX\n",
    y = "\nTrials per Year\n", 
    x = "\nPublication Year\n",
    col = "Phase of Trial") + scale_x_discrete(limit = c(2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017))+ coord_cartesian(xlim = c(2007, 2017))

Figure1
