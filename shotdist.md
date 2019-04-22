---
title: "Shot distance, xG efficiency and player style"
author: "@wiscostretford"
date: "2019-04-22"
output: 
  html_document:
    keep_md: yes
---



One of the most productive data points in popular modern football analytics is x,y coordinates of game events, especially shots. The widespread availability of x,y game data is a crucial enabler of xG models (using shot positions to estimate scoring probability), beautiful shotplots, player heatmaps and so forth.

But another thing that x,y coordinates allow is to track *distances*. In this post, I'll have a look at *shot* distances in particular. As we will see below, shot distances can tell us quite a lot about team and player style and efficiency.

**Data**

Like in previous posts, I will use data from understat here, which provides extensive info for all big league games, including x,y coordinates of shot events. Here I will take the example of 2018/19 Premier League games up to an including gameweek 34.

The data set, which I have manipulated slightly for the purposes at hand, looks like so:




```r
head(usdata)
```

```
##   X.1     id minute      result    X    Y         xG         player h_a
## 1   1 232811      1 BlockedShot 86.3 71.1 0.03996211 Alexis Sánchez   h
## 2   2 232812      2        Goal 88.5 50.0 0.76116884     Paul Pogba   h
## 3   3 232818     39   SavedShot 72.4 65.5 0.01839577     Paul Pogba   h
## 4   4 232819     40   SavedShot 88.0 65.3 0.08121494      Luke Shaw   h
## 5   5 232821     55   SavedShot 78.1 33.0 0.02830883 Matteo Darmian   h
## 6   6 232822     64 MissedShots 81.3 47.6 0.07612572      Juan Mata   h
##   player_id situation season  shotType match_id            h_team
## 1       498  OpenPlay   2018 RightFoot     9197 Manchester United
## 2      1740   Penalty   2018 RightFoot     9197 Manchester United
## 3      1740  OpenPlay   2018 RightFoot     9197 Manchester United
## 4      1006  OpenPlay   2018 RightFoot     9197 Manchester United
## 5       557  OpenPlay   2018 RightFoot     9197 Manchester United
## 6       554  OpenPlay   2018  LeftFoot     9197 Manchester United
##      a_team h_goals a_goals                date player_assisted lastAction
## 1 Leicester       2       1 2018-08-10 22:00:00       Luke Shaw       Pass
## 2 Leicester       2       1 2018-08-10 22:00:00            <NA>   Standard
## 3 Leicester       2       1 2018-08-10 22:00:00  Alexis Sánchez       Pass
## 4 Leicester       2       1 2018-08-10 22:00:00       Juan Mata    Chipped
## 5 Leicester       2       1 2018-08-10 22:00:00  Alexis Sánchez       Pass
## 6 Leicester       2       1 2018-08-10 22:00:00  Alexis Sánchez       Pass
##   id.1 isResult            datetime h.id           h.title h.short_title
## 1 9197     TRUE 2018-08-10 22:00:00   89 Manchester United           MUN
## 2 9197     TRUE 2018-08-10 22:00:00   89 Manchester United           MUN
## 3 9197     TRUE 2018-08-10 22:00:00   89 Manchester United           MUN
## 4 9197     TRUE 2018-08-10 22:00:00   89 Manchester United           MUN
## 5 9197     TRUE 2018-08-10 22:00:00   89 Manchester United           MUN
## 6 9197     TRUE 2018-08-10 22:00:00   89 Manchester United           MUN
##   a.id   a.title a.short_title goals.h goals.a   xG.h    xG.a forecast.w
## 1   75 Leicester           LEI       2       1 1.5137 1.73813     0.2812
## 2   75 Leicester           LEI       2       1 1.5137 1.73813     0.2812
## 3   75 Leicester           LEI       2       1 1.5137 1.73813     0.2812
## 4   75 Leicester           LEI       2       1 1.5137 1.73813     0.2812
## 5   75 Leicester           LEI       2       1 1.5137 1.73813     0.2812
## 6   75 Leicester           LEI       2       1 1.5137 1.73813     0.2812
##   forecast.d forecast.l              team   Y2 result2 goals
## 1     0.3275     0.3913 Manchester United 28.9  nogoal     0
## 2     0.3275     0.3913 Manchester United 50.0    goal     1
## 3     0.3275     0.3913 Manchester United 34.5  nogoal     0
## 4     0.3275     0.3913 Manchester United 34.7  nogoal     0
## 5     0.3275     0.3913 Manchester United 67.0  nogoal     0
## 6     0.3275     0.3913 Manchester United 52.4  nogoal     0
```

&nbsp;

**Calculating shot distance**

From the x,y coordinates, we can use basic Pythagorean math (remember that from school?) to calculate the distance-to-goal for any given shot in the league. However, there are a few steps on the way. 

First, we have to make an assumption about *where* the goal, in fact, is. For simplicity I will assume that the goal position - the position from which to calculate the shot distance - is in the middle of the goal. Because understat uses a standard 100x100 pitch, that means the middle of one the goal is at (x,y) = (100,50) - the other is at (0,50). Since understat's x,y coordinates are standardised for the former end of the pitch, we will use the (100,50) point, and flip the y coordinates to match (using 'Y2').

Second, we have to adjust for pitch size. That's in part because understat assumes a 100x100 pitch, whereas no actual pitch is that size. And in part it's because pitch sizes vary across the Premier League. So we have to adjust shot distance based on the actual size of the pitch on which the game was played.

I start out by grabbing pitch sizes from [Football-Stadiums.co.uk](https://www.football-stadiums.co.uk/articles/are-all-football-pitches-the-same-size/) with 'rvest' and adjust the variables to include in our understat data:


```r
library(dplyr)
library(rvest)
```

```
## Loading required package: xml2
```


```r
pitchs <- read_html("https://www.football-stadiums.co.uk/articles/are-all-football-pitches-the-same-size/",trim=T)

pitchtable <- pitchs %>% html_nodes(xpath="//*[@id='content']/div/div/div[3]/div/table") %>% html_table(fill=T) %>% data.frame()

pitchtable$Stadium <- c("Brighton","Huddersfield","Tottenham","Burnley","Watford","Newcastle United","Arsenal","West Ham","Manchester United","Manchester City","Bournemouth","Southampton","Chelsea","Liverpool","Leicester","Everton","Cardiff","Crystal Palace","Fulham","Wolverhampton Wanderers")

colnames(pitchtable) <- c("team","length","width","area")

pitchtable <- pitchtable %>% mutate(length=length/100) %>% mutate(width=width/100)
```

Then we merge that data in based on the home team ('h_team') and produce a new variable 'dist' that measures the distance-to-goal for any given shot.


```r
usdata2 <- usdata

usdata2$Xadj <- pitchtable$length[match(usdata2$h_team,pitchtable$team)]
usdata2$Yadj <- pitchtable$width[match(usdata2$h_team,pitchtable$team)]

usdata2 <- usdata2 %>% 
 mutate(dist=sqrt(((X-100) * Xadj)^2 + ((Y2-50) * Yadj)^2))
```

**Shot distance and xG**

To illustrate how shot distance underlies xG models, we can plot the two variables against each other. Generally speaking, the further away from goal the shot is, the less likely the shooter is to score:


```r
library(ggplot2)

usdata2 %>% 
  filter(result!="OwnGoal") %>% # removing own goals since they are essentially the 'reverse' shot distance, skewing our data
  ggplot(aes(x=dist,y=xG)) +
  geom_point(colour="black",size=2,alpha=0.5) +
  theme_bw() + 
  geom_smooth(method="loess") +
  labs(title="Shot distance underlies xG models",x="Shot distance-to-goal",y="shot xG") 
```

![](https://i.imgur.com/na4zFbN.png)<!-- -->

**Shot distance and xG efficiency**

Importantly, and interestingly, however, shot distance-to-goal is by no means the only variable in scoring probability (xG) or scoring efficiency (xG efficiency). 

We know that some players are more efficiency compared to their expected goals than others. Think Eden Hazard (very efficient) vs. Christian Benteke (very inefficient).

These are things we can look at more closely with shot coordinates and distance measures. Below, I illustrate the relationship (or lack thereof) between average shot location and distance and xG efficiency. For simplicity, xG efficiency as I will use it here refers simply to actual goals scored over expected goals scored (G/xG), which provides a basic measure of player scoring skill (and/or luck).

Let us start by looking at all players in the 2018/19 Premier League with more than four goals scored:


```r
library(ggrepel)
```

```
## Warning: package 'ggrepel' was built under R version 3.4.3
```

```r
# defining a nice team colour palette

pPalette <- c("Arsenal"="#EF0107",Bournemouth="#000000","Brighton"="#0057B8","Burnley"="#6C1D45","Cardiff"="#0070B5","Chelsea"="#034694","Crystal Palace"="#1B458F","Everton"="#003399","Fulham"="#000000","Huddersfield"="#0E63AD","Leicester"="#003090","Liverpool"="#C8102E","Manchester City"="#6CABDD","Manchester United"="#DA291C","Newcastle United"="#241F20","Southampton"="#D71920","Tottenham"="#132257","Watford"="#FBEE23","West Ham"="#7A263A","Wolverhampton Wanderers"="#FDB913","Draw"="#b1b2b5")

# plotting

usdata2 %>% 
  filter(result!="OwnGoal") %>% 
  group_by(player,team) %>% 
  summarize(meanx=mean(X),meany=mean(Y2),goals=sum(goals),xG=sum(xG)) %>%
  filter(goals > 4) %>% 
  mutate(xgeff = goals / xG) %>% 
  mutate(dist=sqrt((meanx-100)^2 + (meany-50)^2)) %>% 
  ggplot(aes(x=dist,y=xgeff,colour=team)) +
    geom_point(size=3,alpha=0.8) +
    scale_colour_manual(values=pPalette,guide=F) +
    geom_text_repel(size=3,aes(label=player)) +
    theme_bw() +
    labs(x="Average shot distance to goal",y="xG efficiency (goals/xG)", title="Average shot distance to goal and xG efficiency",subtitle="Premier League 2018/19 (min. 4 goals)",caption="Data: understat | @wiscostretford") +
    theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5),plot.caption=element_text(colour="grey",hjust=1,vjust=2))
```

![](https://i.imgur.com/kzLN4W4.png)<!-- -->

Here, we can see some distinct categories of players' shooting styles:

* Towards the right-hand side, you have long-range strikers, often set piece takers such as Christian Eriksen, Victor Camarasa, and James Ward-Prowse.

* Towards the left-hand side, you have box strikers like Glenn Murray, Callum Wilson, and the occasional defender like Shane Duffy.

* At the top, you have ultra-efficient (in terms of xG) scorers like Ruben Loftus-Cheek and Son Heung-Min.

* At the bottom, you have xG-inefficient players, who are involved in a lot of big chances but score relatively little. These are players like Gabriel Jesus, Aleksandar Mitrovic, and Nathan Redmond.

**Shot distance and player style**

We can also look at this at the micro-level, studying the average shot location and distance for individual players, compared to their peers.

Let's start with the example of Manchester United's prime goalscorers - Marcus Rashford, Romelu Lukaku and Paul Pogba. Using the ggsoccer package, we can plot a basic average shotmap and adjust the points by xG efficiency:


```r
library(ggsoccer)

usdata2 %>% 
  group_by(player,team) %>% 
  summarize(meanx=mean(X),meany=mean(Y2),xgeff=sum(goals)/sum(xG)) %>% 
  filter(player %in% c("Marcus Rashford","Romelu Lukaku","Paul Pogba")) %>% 
  ggplot(aes(x=meanx,y=meany,colour=player,size=xgeff)) +
  annotate_pitch() +
  theme_pitch() +
  geom_point() +
  coord_flip(xlim=c(70,101),ylim=c(-1,101)) +
  scale_size(range=c(4,8)) +
  labs(title="Player Average Shot Position - Premier League 2018/19",subtitle="Bubble size by xG efficiency") +
  theme(plot.subtitle = element_text(hjust = 0.5), plot.title = element_text(hjust = 0.5),plot.caption=element_text(colour="grey",hjust=1,vjust=2))
```

![](https://i.imgur.com/HVngbUn.png)<!-- -->

What we see here is the superior xG efficiency of Rashford and Lukaku (box strikers) relative to Paul Pogba (long-ranger). Which is pretty much as you might expect. We can also see the variations in average shot locations. Pogba and Rashford (right-footers) like to shoot from left-of-centre, whereas Lukaku (left-footer) shoots more from the centre.

Let's compare and contrast another set of prime goalscorers - Arsenal's strike duo, Lacazette and Aubameyang:


```r
usdata2 %>% 
  group_by(player,team) %>% 
  summarize(meanx=mean(X),meany=mean(Y2),xgeff=sum(goals)/sum(xG)) %>% 
  filter(player %in% c("Alexandre Lacazette","Pierre-Emerick Aubameyang")) %>% 
  ggplot(aes(x=meanx,y=meany,colour=player,size=xgeff)) +
  annotate_pitch() +
  theme_pitch() +
  geom_point() +
  coord_flip(xlim=c(70,101),ylim=c(-1,101)) +
  scale_size(range=c(4,8)) +
  labs(title="Player Average Shot Position - Premier League 2018/19",subtitle="Bubble size by xG efficiency") +
  theme(plot.subtitle = element_text(hjust = 0.5), plot.title = element_text(hjust = 0.5),plot.caption=element_text(colour="grey",hjust=1,vjust=2))
```

![](https://i.imgur.com/ML0tBj0.png)<!-- -->

Although the average shot position (and distance-to-goal) are extremely similar for the two players, their xG efficiency is quite different, with Lacazette clearly the more clinical striker given his opportunities.

Let's take a final comparison, looking at three of the best players in the league this season - Eden Hazard, Raheem Sterling and Mohamed Salah:


```r
usdata2 %>% 
  group_by(player,team) %>% 
  summarize(meanx=mean(X),meany=mean(Y2),xgeff=sum(goals)/sum(xG)) %>% 
  filter(player %in% c("Eden Hazard","Raheem Sterling","Mohamed Salah")) %>% 
  ggplot(aes(x=meanx,y=meany,colour=player,size=xgeff)) +
  annotate_pitch() +
  theme_pitch() +
  geom_point() +
  coord_flip(xlim=c(70,101),ylim=c(-1,101)) +
  scale_size(range=c(4,8)) +
  labs(title="Player Average Shot Position - Premier League 2018/19",subtitle="Bubble size by xG efficiency") +
  theme(plot.subtitle = element_text(hjust = 0.5), plot.title = element_text(hjust = 0.5),plot.caption=element_text(colour="grey",hjust=1,vjust=2))
```

![](https://i.imgur.com/FZOZxUq.png)<!-- -->

All players are quite xG efficient (>1.0), but Eden is the stand-out in that regard, followed by Sterling and then Salah. The average shot distance is relatively similar, although Sterling generally pounces from locations slightly closer to goal than the other two. Have you seen that Sané-to-Sterling slip across the six-yard box? Yup, that's a big part of this.

Moreover, you can see the y-axis variations, with Hazard coming in from the left, Salah coming in from the right, and Sterling shooting from the middle.

**Long-range goals**

A final bit of interest is to look at long-range strikes in the Premier League this season. As we know, it's hard to score from long distances. But not impossible. And a few have been scored. Let's take the five longest-range goals in the Premier League this season:


```r
usdata2 %>% 
  filter(result!="OwnGoal") %>% 
  filter(result2=="goal") %>% 
  arrange(-dist) %>% 
  select(date,player,h.title,a.title,xG,dist) %>%  
  slice(1:5)
```

```
##                  date          player         h.title         a.title
## 1 2018-12-22 15:00:00 Kevin De Bruyne Manchester City  Crystal Palace
## 2 2019-03-10 12:00:00 Ashley Westwood       Liverpool         Burnley
## 3 2019-01-20 13:30:00          Danilo    Huddersfield Manchester City
## 4 2018-12-01 15:00:00  James McArthur  Crystal Palace         Burnley
## 5 2018-12-22 15:00:00  Philip Billing    Huddersfield     Southampton
##            xG     dist
## 1 0.006977557 35.49073
## 2 0.010851506 33.66379
## 3 0.013048996 33.24000
## 4 0.010331935 32.83810
## 5 0.009138890 32.59503
```

What this tells us is that the longest-range goals in the Premier League this season have been scored by Kevin De Bruyne vs. Crystal Palace (35.5m), Ashley Westwood vs. Liverpool (33.7m), Danilo vs. Huddersfield (33.2m), James McArthur vs. Burnley (32.8m), and Philip Billing (32.6m). (Note these are the longest-range strikes by distance-to-goal, not distance travelled by the ball or the like.)

Of note, it's hard to argue the first four goals were quite fortunate: De Bruyne's was a cross [in-swinger](https://www.youtube.com/watch?v=qObaZLt3MQI&t=73s), Westwood's a [direct corner](https://www.youtube.com/watch?v=1juT8Q4_h9g), Danilo's strike was [massively deflected](https://www.youtube.com/watch?v=YfJDU-NUlCE), McArthur's was [another cross](https://www.nbcsports.com/video/crystal-palaces-james-mcarthur-scores-cross-v-burnley-fc). Billing's goal was a [true long-range strike](https://www.youtube.com/watch?v=oLqffujhwwM), albeit with some questionable goalkeeping.

**Conclusion**

That's it for now. What I (hope to) have shown is the fruitful possibilities for football analytics of some of the less-prevalent uses of (x,y) coordinate data. Here, I have used it to look at shot distance correlation with xG, compare shot distance to xG efficiency, assess and compare player shot styles, and estimate the longest-range goals in the Premier League this season.

Comments and feedback are welcome. Any questions, go find me on Twitter [wiscostretford](http://www.twitter.com/wiscostretford)
