#Load packages
#データロガーの読み込みと作図
#CTRL + D 1行削除
#CTRL + ALT + ↑ or ↓　カーソル複製　
library(tidyverse)
library(lubridate)
library(gnnlab)
library(magick)
library(showtext)
library(readxl)
library(patchwork)

#plots
color=viridis::viridis(6)
font_add_google("Noto Sans JP","notosans-jp")
font_add_google("Noto Sans","notosans")
showtext_auto()

#read data
#データのフォルダー
folder="~/Lab_Data/kawatea/"
dir(folder)

#ファイル名とパス
mnames=str_glue("{folder}Microstation") |> dir(full=T)
onames=str_glue("{folder}Oxygen") |> dir(full=T)

#2021/04のデータの存在を確認する
str_subset(mnames,"_2104")
str_subset(onames,"_2104")


#Microstatation　2021　april を読み込む
micro=str_subset(mnames,"_2104") |> read_onset()

#oxygenを読み込む
oxygen=tibble(onames) |> 
  filter(str_detect(onames,"2104")) |> 
  mutate(data=map(onames,read_onset))

oxygen=oxygen |> 
  mutate(onames=basename(onames)) |> 
  separate(onames,
           c("logger",
             "ID",
             "location",
             "position",
             "survey",
             "extension")) |> 
  select(-logger,-survey,-extension)

oxygen=oxygen |> 
  mutate(location=ifelse(str_detect(location,"amamo"),
                         "Zostera","Sargassum"))
oxygen=oxygen |> unnest(data)

oxygen |> slice(1:3)
micro |> slice(1:3)

micro=micro |> 
  mutate(datetime=floor_date(datetime,"min"))


dall=full_join(oxygen,
          micro,
          by="datetime")

s0=as_date("2021/04/24")
s1=as_date("2021/05/18")

interval(s0,s1)
dall=dall |> filter(datetime %within% interval(s0,s1))

dall |> 
  ggplot()+
  geom_line(aes(x=datetime,
                y=temperature,
                color=position))+
  facet_grid(rows = vars(location))
  
dall |> 
  ggplot()+
  geom_line(aes(x=datetime,
                y=mgl,
                color=position))+
  facet_grid(rows = vars(location))

dall |> 
  ggplot()+
  geom_line(aes(x=,
                y=ppfd,
                color=position))+
  facet_grid(rows = vars(location))

dsum=dall |> 
  mutate(date=as_date(datetime),
         .before="location") |> 
  group_by(location,position,date) |> 
  summarise(across(c(mgl,temperature,
                     ppfd,wind),
                   list(m=mean,
                        sd=sd,
                        median=median)))
ggplot(dsum)+
  geom_point(aes(x=ppfd_m,
                 y=mgl_m))+
  facet_grid(rows = vars(location),
             cols = vars(position))

dall |> 
  mutate(date=as_date(datetime),
         .before="location") |> 
  group_by(location,position,date) |> 
  mutate(rate=mgl-lag(mgl),.before="mgl") |> 
  select(location,position,date,
         rate,ppfd) |> drop_na() |> 
  summarise(across(c(rate,ppfd),
                   list(mean=mean,sd=sd))) |> 
  ggplot()+
  geom_point(aes(x=ppfd_mean,y=rate_mean))+
  facet_grid(rows = vars(location),
             cols = vars(position))
  
  
  
  
  