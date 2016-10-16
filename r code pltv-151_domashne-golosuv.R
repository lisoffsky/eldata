library(dplyr)
library(psych)
library(tidyr)
library(leaflet)

poltava %>% 
  mutate(PEC = `X..ВД`,
       voters = `X3..К.сть.виборців..внесених.до.списку`,
       `Явка` = round((`X7..Сумарна.к.сть.виборців..які.отримали.бюлетені` / `X3..К.сть.виборців..внесених.до.списку`) * 100,0),
       invalid = round((`X13..К.сть.бюлетенів..визнаних.недійсними` / `X12..К.сть.виборців..які.голосували.на.виборчій.дільниці`)*100,0),
       turnout_point = round((`X7..Сумарна.к.сть.виборців..які.отримали.бюлетені` / `X3..К.сть.виборців..внесених.до.списку`) * 100,0),
       home_vote = round((`X6..К.сть.виборців..які.отримали.за.місцем.перебування` / `X7..Сумарна.к.сть.виборців..які.отримали.бюлетені`) * 100, 0)) %>% 
  select(PEC, voters, turnout_point, invalid, home_vote) %>% 
  filter(voters > 500, home_vote >= 10) %>% 
  arrange(desc(home_vote)) ->pltv
write.csv(pltv, "pltv_151_home-vote.csv")

cor(pltv)

cord_151_okrug %>% 
  separate(Кординати, c('long', 'lat'), sep=",") %>% 
  mutate(PEC = Номер.дільниці) %>% 
  select(PEC, long, lat, Опис.меж.дільниці, Адреса.приміщення.для.голосування..місцезнаходження....Адреса.дільничної.виборчої.комісії) ->coord_pltv
write.csv(coord_pltv, "coordinates_151_poltavska,csv")  

mapfile <- merge(coord_pltv, pltv, by=c("PEC"))
mapfile$hv_l = paste(mapfile$PEC, ". ", mapfile$voters, " виборців. Голосували на дому - ", mapfile$home_vote,
                     "%. ", mapfile$Опис.меж.дільниці, sep = "")

black = colorNumeric("#90a092", domain = NULL)

leaflet (mapfile) %>% 
  setView(lat = 50.27, lng = 33.72, zoom = 8) %>% 
  addTiles (attribution = "Дані: <a href=http://cvk.gov.ua>ЦВК</a> - Обробка: lisoffsky")%>% 
  addCircleMarkers(~long, ~lat, radius = ~home_vote, popup = ~as.character(hv_l), color = ~black(home_vote))