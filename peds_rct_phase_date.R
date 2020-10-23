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

### Defined Search Paramaters 
srch_peds_RCT <- "((Randomized Controlled Trial[ptyp])) AND ((2006/10/21[PDat]:2017/11/13[PDat])) AND ((Child[Mesh] OR Infant[Mesh] OR Adolescent [Mesh]) NOT ((Adult[Mesh])"

srch_peds_p1 <- "((Clinical Trial, Phase I[ptyp])) AND ((2006/10/21[PDat]:2017/11/13[PDat])) AND ((Child[Mesh] OR Infant[Mesh] OR Adolescent[Mesh]) NOT ((Adult[Mesh])"

srch_peds_p2 <- "((Clinical Trial, Phase II[ptyp])) AND ((2006/10/21[PDat]:2017/11/13[PDat])) AND ((Child[Mesh] OR Infant[Mesh] OR Adolescent[Mesh]) NOT ((Adult[Mesh])"

srch_peds_p3 <- "((Clinical Trial, Phase III[ptyp])) AND ((2006/10/21[PDat]:2017/11/13[PDat])) AND ((Child[Mesh] OR Infant[Mesh] OR Adolescent[Mesh]) NOT ((Adult[Mesh])"

srch_peds_p4 <- "((Clinical Trial, Phase IV[ptyp])) AND ((2006/10/21[PDat]:2017/11/13[PDat])) AND ((Child[Mesh] OR Infant[Mesh] OR Adolescent[Mesh]) NOT ((Adult[Mesh])"

### Build function to scrape Pubmed into S4 
scrape_pubmed <- function (x) {
  x %>%
    EUtilsSummary(
      retmax=10000, 
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
df_rRCT <- gen_df(srch_peds_RCT)

df_rp1 <- gen_df(srch_peds_p1)
df_rp2 <- gen_df(srch_peds_p2)
df_rp3 <- gen_df(srch_peds_p3)
df_rp4 <- gen_df(srch_peds_p4)

##########  CLEAN DATASETS  ########## 
### Bind dataframes together 

df_rRCT$kind <- c("RCT")
df_rp1$phase <- c("Phase 1")
df_rp2$phase <- c("Phase 2")
df_rp3$phase <- c("Phase 3")
df_rp4$phase <- c("Phase 4")

df <- rbind(df_rRCT)
df_p <-rbind(df_rp1, df_rp2, df_rp3, df_rp4)

write_csv(df, "RCT SRMA Project - RCT Phase.csv")
write_csv(df_p, "RCT Project - Phase Extension.csv")

# Generate Date-based Time Variable

df$date <- paste(df$year, df$month, df$day, sep="-")
df$date <- ymd(df$date)

df_p$date <- paste(df_p$year, df_p$month, df_p$day, sep="-")
df_p$date <- ymd(df_p$date)

# Create intervals for time variable 

df <- df %>% 
  mutate(monthly = as.Date(cut(df$date, breaks = "month"))) 

df_p <- df_p %>% 
  mutate(monthly = as.Date(cut(df_p$date, breaks = "month"))) 

##########  Overall Numbers  ########## 

df %>%
  group_by(kind) %>%
  count() 

# Total RCTS ***

df_p %>% 
  group_by(phase) %>%
  count()


##########  Figure 1: RCTs By Clinical Phase Over Time ########## 
df_p %>% 
  mutate(yearly = year(date)) %>%
  group_by(yearly, phase) %>%
  count() %>% 
  filter(yearly <= 2017)%>%
  ggplot(aes(
    x = yearly,
    y = n, 
    col = phase)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth() +
  theme_classic(
    base_size = 22) +
  scale_color_jama() +
  labs(
    title = "\nFigure 1: Phase of Pediatric Clinical Trials, 1995-2017", 
    subset = "Stratified by Phase of Study, n = 62\n",
    y = "\nTrials per Year\n", 
    x = "\nPublication Year\n",
    col = "Phase of Trial")



