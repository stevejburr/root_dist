# Get to get data from ESPN Cricinfo

library(tidyverse)
library(here)
library(RCurl)
library(XML)
library(RcppRoll)

# donwload match by match data for each player



get_rolling_averages <- function(id) {
  
  url <- paste0("https://stats.espncricinfo.com/ci/engine/player/",id,".html?class=1;template=results;type=batting;view=innings")
  
  data <- getURL(url)
  tables <-readHTMLTable(data, stringsAsFactors = F)
  
  table <- tables$`Innings by innings list`
  
  #some of the columns are blank, don't care about these so put in dummy values
  colnames(table)[10] <- "A"
  colnames(table)[14] <- "B"
  
  #remove DNB/absent etc from records
  table %>%
    mutate(
      Runs=str_replace(Runs,"\\*",""),
      Runs=as.numeric(Runs)) %>%
    filter(!is.na(Runs)) %>%
    mutate(out=if_else(Dismissal %in% c("retired notout","not out"),0,1)) -> table
  
  # check can match official average:  
  table %>%
    summarise(Runs=sum(Runs),
              outs=sum(out)) %>%
    mutate(Average=Runs/outs)
  
  # calculate cumulative averages up to this point in career
  # calculate rolling averages for latest 10, and 20 innings
  
  table %>%
    mutate(CumRuns=cumsum(Runs),
           CumOuts=cumsum(out),
           CumAverage=CumRuns/CumOuts,
           CumInnings=1:n(),
           CumRuns_10=roll_sum(Runs,10,align="right",fill=NA),
           CumOuts_10=roll_sum(out,10,align="right",fill=NA),
           CumAverage_10=CumRuns_10/CumOuts_10,
           CumRuns_20=roll_sum(Runs,20,align="right",fill=NA),
           CumOuts_20=roll_sum(out,20,align="right",fill=NA),
           CumAverage_20=CumRuns_20/CumOuts_20
           ) -> table
  
  table$player <- id
  
  return(table)
}
  

root <- get_rolling_averages(303669)

root %>%
  mutate(year=str_extract(`Start Date`,"[0-9]+$")) -> root
View(root)

root %>%
  ggplot() +
  #facet_wrap(.~ year) +
  geom_histogram(aes(x=Runs,
                     y=stat(density)*10),
                 binwidth = 10)

# bradman <- get_rolling_averages(4188)
# 
steve_smith <- get_rolling_averages(267192)

steve_smith%>%
  ggplot() +
  #facet_wrap(.~ year) +
  geom_histogram(aes(x=Runs,
                     y=stat(density)*10),
                 binwidth = 10)

kohli <- get_rolling_averages(253802)

williamson <- get_rolling_averages(277906)

labuschagne <- get_rolling_averages(787987)


bind_rows(steve_smith %>% mutate(player="SPD Smith"),
          williamson %>% mutate(player ="KS Williamson"),
          kohli %>% mutate(player="V Kohli"),
          root %>% mutate(player="JE Root"),
          labuschagne %>% mutate(player="M Labuschagne")) -> all_data


all_data %>%
  saveRDS("all_data.rds")

readRDS("all_data.rds") %>%
  filter(player!="M Labuschagne") -> all_data

all_data %>%
  group_by(player) %>%
  filter(n()==row_number()) %>%
  select(player,average=CumAverage) -> averages


all_data %>%
  mutate(mid_point=case_when(Runs<50 ~ 25,
                             Runs<100 ~ 75,
                             Runs<150 ~ 125,
                             Runs<200 ~ 175,
                             Runs<250 ~ 225,
                             TRUE ~ NA_real_)) %>%
  group_by(player, mid_point) %>%
  summarise(count=n()) %>%
  mutate(perc=scales::percent(count/sum(count), accuracy=1)) %>%
  filter(!is.na(mid_point))-> proportions
  
windowsFonts(Raleway="Raleway")

all_data %>%
  ggplot() +
  facet_grid(player ~ .) +
  geom_histogram(aes(x=Runs,
                     y=stat(density)*10),
                 binwidth = 10,
                 boundary=10,
                 fill="#9b0d0d") +
  geom_vline(data=averages,
             aes(xintercept=average),
             linetype=2,
             size=0.2,
             colour="grey50")+
  geom_text(data=proportions,
            aes(x=mid_point,
                y=0.4,
                label=perc),
            family="Raleway",
            colour="grey50",
            size=1.6)+
  scale_x_continuous(breaks=c(0,50,100,150,200,250)) +
  scale_y_continuous("% of innings played",limits=c(0,0.5),breaks=c(0,0.1,0.2,0.3),
                     labels=scales::percent)+
  theme_minimal(base_family = "Raleway",
                base_size=5) +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x = element_line(colour="black"),
        text=element_text(colour="grey50"),
        strip.text =element_text(colour="grey50")) +
  labs(title="Despite his recent good form, Joe Root falls between 50 and 100 more often\nthan the other best batsmen in the world.",
       subtitle="Distribution of innings by runs scored for Top 5* ranked test batsman as of 06/02/21\nTest averages shown as dotted lines.",
       caption="Date from ESPNcricinfo | Viz by @stevejburr\n*M Labuschagne excluded from comparison due to playing many fewer tests")

ggsave("output.png",
       width=1000/300,
       height=800/300)  


 
# look at top england players:


an_cook <- get_rolling_averages(11728)
ga_gooch <- get_rolling_averages(13399)
di_gower <- get_rolling_averages(13418)

kp_pietersen <- get_rolling_averages(19296)
g_boycott <- get_rolling_averages(9187)
ir_bell <- get_rolling_averages(9062)
aj_strauss <- get_rolling_averages(20387)
mp_vaughan <- get_rolling_averages(22182)
it_botham <- get_rolling_averages(9163)


bind_rows(an_cook %>% mutate(player="AN Cook"),
          ga_gooch %>% mutate(player ="GA Gooch"),
          di_gower %>% mutate(player="DI Gower"),
          root %>% mutate(player="JE Root"),
          kp_pietersen %>% mutate(player="KP Pietersen"),
          g_boycott %>% mutate(player="G Boycott"),
          ir_bell %>% mutate(player="IR Bell"),
          aj_strauss %>% mutate(player="AJ Strauss"),
          mp_vaughan %>% mutate(player="MP Vaughan"),
          it_botham %>% mutate(player="IT Botham")) -> eng_data



eng_data %>%
  group_by(player) %>%
  filter(n()==row_number()) %>%
  select(player,average=CumAverage) -> averages_eng


eng_data %>%
  mutate(mid_point=case_when(Runs<50 ~ 25,
                             Runs<100 ~ 75,
                             Runs<150 ~ 125,
                             Runs<200 ~ 175,
                             Runs<250 ~ 225,
                             TRUE ~ NA_real_)) %>%
  group_by(player, mid_point) %>%
  summarise(count=n()) %>%
  mutate(perc=scales::percent(count/sum(count), accuracy=1)) %>%
  filter(!is.na(mid_point))-> proportions_eng


eng_data %>%
  ggplot() +
  facet_wrap(player ~ .,ncol = 5) +
  geom_histogram(aes(x=Runs,
                     y=stat(density)*10),
                 binwidth = 10,
                 boundary=10,
                 fill="#9b0d0d") +
  geom_vline(data=averages_eng,
             aes(xintercept=average),
             linetype=2,
             size=0.2,
             colour="grey50")+
  geom_text(data=proportions_eng,
            aes(x=mid_point,
                y=0.4,
                label=perc),
            family="Raleway",
            colour="grey50",
            size=1.6)+
  scale_x_continuous(breaks=c(0,50,100,150,200,250)) +
  scale_y_continuous("% of innings played",limits=c(0,0.5),breaks=c(0,0.1,0.2,0.3),
                     labels=scales::percent)+
  theme_minimal(base_family = "Raleway",
                base_size=5) +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x = element_line(colour="black"),
        text=element_text(colour="grey50"),
        strip.text =element_text(colour="grey50")) +
  labs(title="Test batting score distributions for selected top English batsmen.",
       subtitle="Root's 100 rate is similat to Bell's, and worse than Vaughan despite a higher average.\nTest averages shown as dotted lines.",
       caption="Date from ESPNcricinfo as of 08/02/21| Viz by @stevejburr")

ggsave("output_eng.png",
       width=1600/300,
       height=1000/300)  


# top scoring batsmen:
sr_tendulkar <- get_rolling_averages(35320)
rt_ponting <- get_rolling_averages(7133)
jh_kallis <- get_rolling_averages(45789)
rs_dravid <- get_rolling_averages(28114)
an_cook <- get_rolling_averages(11728)
kc_sangakkara <- get_rolling_averages(50710)
bc_lara <- get_rolling_averages(52337)
s_chanderpaul <- get_rolling_averages(51469)
dpmd_jayawardene <- get_rolling_averages(49289)
ar_border <- get_rolling_averages(4174)


bind_rows(an_cook %>% mutate(player="AN Cook"),
          sr_tendulkar %>% mutate(player ="SR Tendulkar"),
          rt_ponting %>% mutate(player="RT Ponting"),
          jh_kallis %>% mutate(player="JH Kallis"),
          rs_dravid %>% mutate(player="RS Dravid"),
          kc_sangakkara %>% mutate(player="KC Sangakkara"),
          bc_lara %>% mutate(player="BC Lara"),
          s_chanderpaul %>% mutate(player="S Chanderpaul"),
          dpmd_jayawardene %>% mutate(player="DPMD Jayawardene"),
          ar_border %>% mutate(player="AR Border")) -> high_data



high_data %>%
  group_by(player) %>%
  filter(n()==row_number()) %>%
  select(player,average=CumAverage) -> averages_high


high_data %>%
  mutate(mid_point=case_when(Runs<50 ~ 25,
                             Runs<100 ~ 75,
                             Runs<150 ~ 125,
                             Runs<200 ~ 175,
                             Runs<250 ~ 225,
                             TRUE ~ NA_real_)) %>%
  group_by(player, mid_point) %>%
  summarise(count=n()) %>%
  mutate(perc=scales::percent(count/sum(count), accuracy=1)) %>%
  filter(!is.na(mid_point))-> proportions_high


high_data %>%
  ggplot() +
  facet_wrap(player ~ .,ncol = 5) +
  geom_histogram(aes(x=Runs,
                     y=stat(density)*10),
                 binwidth = 10,
                 boundary=10,
                 fill="#9b0d0d") +
  geom_vline(data=averages_high,
             aes(xintercept=average),
             linetype=2,
             size=0.2,
             colour="grey50")+
  geom_text(data=proportions_high,
            aes(x=mid_point,
                y=0.4,
                label=perc),
            family="Raleway",
            colour="grey50",
            size=1.4)+
  scale_x_continuous(breaks=c(0,50,100,150,200,250)) +
  scale_y_continuous("% of innings played",limits=c(0,0.5),breaks=c(0,0.1,0.2,0.3),
                     labels=scales::percent)+
  theme_minimal(base_family = "Raleway",
                base_size=4.5) +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x = element_line(colour="black"),
        text=element_text(colour="grey50"),
        strip.text =element_text(colour="grey50")) +
  labs(title="Test batting score distributions for top 10 test run scorers",
       subtitle="Test averages shown as dotted lines.",
       caption="Date from ESPNcricinfo as of 08/02/21| Viz by @stevejburr")

ggsave("output_high.png",
       width=1600/300,
       height=1000/300)  



# bradman vs smith:
dg_bradman <- get_rolling_averages(4188)
ac_voges <- get_rolling_averages(8119)

bind_rows(steve_smith %>% mutate(player="SPD Smith"),
          dg_bradman %>% mutate(player ="DG Bradman"),
          ac_voges %>% mutate(player="AC Voges"),
          labuschagne %>% mutate(player="M Labuschagne")) -> aus_data


aus_data %>%
  group_by(player) %>%
  filter(n()==row_number()) %>%
  select(player,average=CumAverage) -> averages_aus


aus_data %>%
  mutate(mid_point=case_when(Runs<50 ~ 25,
                             Runs<100 ~ 75,
                             Runs<150 ~ 125,
                             Runs<200 ~ 175,
                             Runs<250 ~ 225,
                             TRUE ~ NA_real_)) %>%
  group_by(player, mid_point) %>%
  summarise(count=n()) %>%
  mutate(perc=scales::percent(count/sum(count), accuracy=1)) %>%
  filter(!is.na(mid_point))-> proportions_aus



aus_data %>%
  ggplot() +
  facet_grid(player ~ .) +
  geom_histogram(aes(x=Runs,
                     y=stat(density)*10),
                 binwidth = 10,
                 boundary=10,
                 fill="#9b0d0d") +
  geom_vline(data=averages_aus,
             aes(xintercept=average),
             linetype=2,
             size=0.2,
             colour="grey50")+
  geom_text(data=proportions_aus,
            aes(x=mid_point,
                y=0.4,
                label=perc),
            family="Raleway",
            colour="grey50",
            size=1.6)+
  scale_x_continuous(breaks=c(0,50,100,150,200,250)) +
  scale_y_continuous("% of innings played",limits=c(0,0.5),breaks=c(0,0.1,0.2,0.3),
                     labels=scales::percent)+
  theme_minimal(base_family = "Raleway",
                base_size=5) +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x = element_line(colour="black"),
        text=element_text(colour="grey50"),
        strip.text =element_text(colour="grey50")) +
  labs(title="Comparison of Australians with high test averages",
       subtitle="Distribution of innings by runs scored.\nTest averages shown as dotted lines.",
       caption="Date from ESPNcricinfo | Viz by @stevejburr")

ggsave("output_aus.png",
       width=1000/300,
       height=800/300)  


# current "good" batsmen -

ca_pujara <- get_rolling_averages(32540)
b_azam <- get_rolling_averages(348144)
am_rahane <- get_rolling_averages(277916)
hm_nicholls <- get_rolling_averages(539511)
ba_stokes <- get_rolling_averages(311158)



bind_rows(ca_pujara %>% mutate(player="CA Pujara"),
          b_azam %>% mutate(player ="B Azam"),
          am_rahane %>% mutate(player="AM Rahane"),
          root %>% mutate(player="JE Root"),
          hm_nicholls %>% mutate(player="HM Nicholls"),
          ba_stokes %>% mutate(player="BA Stokes")) -> good_data


good_data %>%
  group_by(player) %>%
  filter(n()==row_number()) %>%
  select(player,average=CumAverage) -> averages_good


good_data %>%
  mutate(mid_point=case_when(Runs<50 ~ 25,
                             Runs<100 ~ 75,
                             Runs<150 ~ 125,
                             Runs<200 ~ 175,
                             Runs<250 ~ 225,
                             TRUE ~ NA_real_)) %>%
  group_by(player, mid_point) %>%
  summarise(count=n()) %>%
  mutate(perc=scales::percent(count/sum(count), accuracy=1)) %>%
  filter(!is.na(mid_point))-> proportions_good



good_data %>%
  ggplot() +
  facet_grid(player ~ .) +
  geom_histogram(aes(x=Runs,
                     y=stat(density)*10),
                 binwidth = 10,
                 boundary=10,
                 fill="#9b0d0d") +
  geom_vline(data=averages_good,
             aes(xintercept=average),
             linetype=2,
             size=0.2,
             colour="grey50")+
  geom_text(data=proportions_good,
            aes(x=mid_point,
                y=0.4,
                label=perc),
            family="Raleway",
            colour="grey50",
            size=1.6)+
  scale_x_continuous(breaks=c(0,50,100,150,200,250)) +
  scale_y_continuous("% of innings played",limits=c(0,0.5),breaks=c(0,0.1,0.2,0.3),
                     labels=scales::percent)+
  theme_minimal(base_family = "Raleway",
                base_size=4.5) +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x = element_line(colour="black"),
        text=element_text(colour="grey50"),
        strip.text =element_text(colour="grey50")) +
  labs(title="Root looks a cut above the tier below.",
       subtitle="Distribution of innings by runs scored for Top 5-10* ranked test batsman as of 08/02/21\nTest averages shown as dotted lines.",
       caption="Date from ESPNcricinfo | Viz by @stevejburr")

ggsave("output_good.png",
       width=1000/300,
       height=800/300)  
