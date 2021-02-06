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


 


