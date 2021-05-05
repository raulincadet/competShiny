library(tidyverse)
covid<-read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")
cases<-covid%>%
  group_by(location)%>%
  summarise(cases=tail(total_cases,1),deaths=tail(total_deaths,1),tests=tail(total_tests,1))

#################################################
###########    Importing Data       #############
#################################################
covid_who<-read_csv("covid_WHO.csv")# read_csv("https://covid19.who.int/WHO-COVID-19-global-table-data.csv") # Source: World Health Organization - https://covid19.who.int/table
pib_capita<-read_csv("pib_percapita.csv") # Source: World Development Indicators - https://data.worldbank.org/indicator/NY.GDP.PCAP.CD
population<-read_csv("population.csv")    # Source: WDI - https://data.worldbank.org/indicator/SP.POP.TOTL
tourists<-read_csv("tourismarrival.csv") # Tourists arrival - Source: WDI - https://data.worldbank.org/indicator/ST.INT.ARVL
country_groups<-read_csv("countriesGroups.csv")
################################################
###########    Cleanning Data       #############
#################################################
covid<-covid_who%>% # selecting needed variables and changing their names
  select(Name,`Cases - cumulative total`, `Deaths - cumulative total`)%>%
  transmute(`Country Name`=Name,Cases=`Cases - cumulative total`, Deaths=`Deaths - cumulative total`)

pib_capita19<-pib_capita%>%  # PIB per capita in 2019
  mutate(PIBcapita=`2019`)%>%
  select(`Country Name`,`Country Code`,PIBcapita)

population19<-population%>%  # Population in 2019
  mutate(Population=`2019`)%>%
  select(`Country Name`,`Country Code`,Population)

tourists19<-tourists%>%  # Tourists arrival in 2019
  mutate(Tourists=`2019`)%>%
  select(`Country Name`,`Country Code`,Tourists)

############## Merging tibbles #####################
df<-inner_join(inner_join(inner_join(inner_join(covid,pib_capita19,by="Country Name"),
                      population19,by="Country Name"),
           tourists19,by="Country Name"),
           country_groups,by="Country Code")

######### Eliminating duplicated columns of country codes ##########
df<-df%>%
  select(`Country Code`,`Country Name`,Region,IncomeGroup,Cases,Deaths,Population,PIBcapita,Tourists)


########################################################
######## Creating new variables #######################
df<-df%>%
  mutate(Cases_rate=Cases/Population,Tourists_rate=Tourists/Population)


################################################################
##################### DATA ANALYSIS ###########################
###############################################################

ggplot(df, aes(x = Cases_rate, y = PIBcapita, color = Region))+
 geom_point(size=3)+
  geom_smooth(method='loess')+
  theme_classic()


ggplot(df, aes(x = Cases_rate, y = PIBcapita, color = IncomeGroup))+
  geom_point(size=3)+
  #geom_smooth(method='loess')+
  theme_classic()
