library(ggmap);library(maps);
library(mapdata)

us_states = map_data("state") %>% tbl_df()


Northeast = c('connecticut', 'maine', 'massachusetts',
              'new hampshire', 'rhode island','vermont','new jersey',
              'new York','pennsylvania')

Midwest = c('Illinois', 'Indiana','Michigan', 'Ohio',
            'Wisconsin','Iowa', 'Kansas', 'Minnesota', 
            'Missouri', 'Nebraska', 'North Dakota', 'South Dakota')

South = c('Delaware', 'Florida', 'Georgia', 'Maryland',
          'North Carolina', 'South Carolina', 'Virginia', 
          'District of Columbia', 'West Virginia','Alabama',
          'Kentucky', 'Mississippi','Tennessee','Arkansas',
          'Louisiana', 'Oklahoma', 'Texas')

West = c('Arizona', 'Colorado', 'Idaho', 'Montana', 'Nevada',
         'New Mexico', 'Utah', 'Wyoming','California',
         'Hawaii', 'Oregon', 'Washington')

northeast = tolower(Northeast)

midwest = tolower(Midwest)

south = tolower(South)

west = tolower(West)


us_states2 = us_states %>% select(-subregion)

us_states2$reg[us_states$region %in% northeast] = "NorthEast"
us_states2$reg[us_states$region %in% west] = "West"
us_states2$reg[us_states$region %in% south] = "South"
us_states2$reg[us_states$region %in% midwest] = "MidWest"


included = c('arkansas','alaska',' arizon',
'california', "colorado", "connecticut",
"district of columbia", "delaware", "florida",
"georgia", "hawaii","iowa", "illinois", "indiana",
"kansas", "kentucky", "lousiana", "maryland", "maine",
"massachucets", "minnesota", "missouri", "montana",
"michigan", "montana", "north carolina", "north dakota",
"nebraska", "new jersey", "new mexico", "nevada", "new york",
"ohio", "oklahoma", "pennsylvania", "rhode island",
"south carolina", "south dakota", "tennesee", "texas",
"utah", "virginia", "vermont", "washington", "wisconsin",
"west virginia", "wyoming")

library(ggthemr)

ggthemr::ggthemr(palette = "dust")

# create simple map
a = ggplot(data= us_states2, 
  aes(x=long, y=lat, group=group, fill = reg))

a2 = a + geom_polygon(color = "black", size = 0.1) +
  theme(legend.position = "bottom") + 
  labs(fill = "Region") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  xlab("") + ylab("")




# + coord_map(projection = "albers",
#   lat0 = 39, lat1 = 45)




