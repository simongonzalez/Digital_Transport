library(shiny)
library(bs4Dash)
library(tidyverse)
library(data.table)
library(geojsonio)
library(rmapshaper)
library(jsonlite)
library(rjson)
library(sf)
library(broom)
library(viridis)
library(leaflet)
library(SpatialEpi)
library(colourvalues)
library(geofilter)
library(leaflet.minicharts)
library(leaflet.extras)
library(DT)
library(shinybusy)
library(echarts4r)
library(geosphere)
library(highcharter)
library(rAmCharts4)
library(shinyWidgets)
library(lubridate)
library(TSstudio)
library("xts")
library(forecast)
library(plotly)
library(timetk)
library(tidyquant)
library(shinyBS)

shinyApp(
  ui = dashboardPage(
    header = dashboardHeader(
      title = dashboardBrand(
        title = "Digital Transport",
        color = "primary"
      )
    ),
    sidebar = dashboardSidebar(
      sidebarMenu(
        menuItem(
          text = "Past, Present and Future",
          tabName = "context_item"
        ),
        menuItem(
          text = "Transport across Time",
          tabName = "ts_item"
        ),
        menuItem(
          text = "Transport and the Community",
          tabName = "path_item"
        ),
        menuItem(
          text = "Transport and Wildlife",
          tabName = "wl_item"
        )
      )
    ),
    body = dashboardBody(
      # lapply(getAdminLTEColors(), function(color) {
      #   box(status = color)
      # }),
      tabItems(
        tabItem(
          tabName = "context_item",
          
          tabsetPanel(
            id = "tabset1",
            selected = "Tab 2",
            tabPanel(
              title = "ACT Transport in context", 
              bs4Callout(
                title = "",
                elevation = 4,
                width = 12,
                status = "warning",
                p("The growth of a city is a balance of cause and effect of all the complex social, economic and environmental infrastructure. Since they cohabit in a fragile balance, the change in one affects all the other. In the middle of it all, transportation, both public and private, seem to play a vital role in a changing society, especially in the Australian Capital territory."),
                p('In this tool, we have developed a solution that tries to give answer to four fundamental aspects in our community, especially in post pandemic world that has affected the mental health of the community and the environment. We also hope that this helps not only for the short term, but also the future and legacy in our city:'),
                p(strong("How we can protect our people")),
                p(strong("How we can protect our open areas")),
                p(strong("How we can protect our wildlife")),
                p(strong("How we can protect our environment"))
              )
            ),
            tabPanel(
              title = "The challenge", 
              bs4Callout(
                title = "",
                elevation = 4,
                width = 12,
                status = "warning",
                p("In this section, we talk about the challenge that is in front of us. We have analysed and observed data from different sources and different aspects. For a more detailed assessment of the data, we have added and ASSESSMENT button next to the visualisations in the Transport across time tab."),
                
                p(strong('Mental Health')),
                highchartOutput('dfwellbeing_ui'),
                p(em("Source: "), a("All Wellbeing Measures", href="https://www.data.act.gov.au/dataset/All-Wellbeing-Measures/b5kg-deh8", target="_blank")),
                p("Data from the mental health shows that Canberrans highly value the ACT is a good place to live (86%) and that local areas have a high level if liveability (88%). This shows that if there are more ways to make public spaces more accessible, this will help Canberrans to improve their chances of experiencing more the city and its surroundings."),
                p("On the other hand, the data shows that living in post pandemic world has affected our mental health. It shows that 21% are experiencing loneliness and 11% struggling with social connections. These statistics show that it is necessary to create more opportunities of connection and social interaction. For this reason, one efficient way is to make public spaces, especially in nature, like playgrounds, nature parks, lakes, etc, more accessible. In this way, it helps people connecting but also allows people be more scattered and not overcrowding smaller spaces."),
                p(strong('Vehicles and Zero Emissions')),
                p('The data explored in the Transport across time shows that there is a small, yet relevant trend, of people slowly trying to go into more environmentally friendly options. Another assessment of the data, shows that people rely heavily on public transportation to go to places. This suggests that people can use more public transportation if this allows a more efficient way of taking care of pour environment.'),
                
              )
            ),
            tabPanel(
              title = "A solution", 
              bs4Callout(
                title = "",
                elevation = 4,
                width = 12,
                status = "warning",
                p("Since Public Transport plays such a important role in our community, we propose that bus stops do not function just as transitions points from places, but also places from where passengers can have more information accessible to open and public places nearby."),
                p("Public transport routes go across all the veins and arteries of our city. We can then maximize this by connecting people to places they might not even know exist. This requires, not a change on the physical infrastructure, but rather a digital infrastructure. For this, we can also maximise the fact that Canberrans have easy access to networks in which tools like the ones here presented can be made accessible."),
                p("This is why we have designed and put together this tool. This is a functional prototype of the power and reach that public transformation can have in a post pandemic world, keeping up with the new technological advances available at our fingertips.")
              )
            )
          )
          
        ),
        
        tabItem(
          tabName = "ts_item",
          
          tabsetPanel(
            id = "tabset1",
            selected = "Tab 2",
            tabPanel(
              title = "Public Transport Timeseries",
              bs4Callout(
                title = "",
                elevation = 4,
                width = 12,
                status = "warning",
                fluidRow(
                  p(strong("All number of trips across time")),
                  popover(
                    actionButton("ts_pop_content", "Assessment", status = 'success'),
                    title = "What the data shows",
                    placement = "right",
                    content = "Public transportation shows steady seasonal patterns and consistent across the years. The troughs observe mainly correlate with school holidays, and the peaks are close to the start of the school terms. This suggests that much of the patterns of public transportation are driven by the rhythm of school terms.\nAnother important pattern is that May 2020 saw an uncharacteristic low peak, which can be related to the impact that COVID19 had on public transportation."
                  )),
                highchartOutput('act_ts_all'),
                p(em("Source: "), a("Canberra Metro Light Rail Transit Feed - Trip Updates (Historical Archive)", href="https://www.data.act.gov.au/Transport/Canberra-Metro-Light-Rail-Transit-Feed-Trip-Update/jxpp-4iiz", target="_blank")),
                p(strong("All trips by time of the day")),
                highchartOutput('act_ts_pod'),
                p(strong("All trips by day of the week")),
                highchartOutput('act_ts_day')
              )
            ),
            tabPanel(
              title = "Public Transport Journeys",
              bs4Callout(
                title = "",
                elevation = 4,
                width = 12,
                status = "warning",
                
                fluidRow(
                  p(strong("Public Transport Passanger Journeys")),
                  popover(
                    actionButton("ts_pop_content_journeys", "Assessment", status = 'success'),
                    title = "What the data shows",
                    placement = "right",
                    content = "The impact of the COVID19 in 2020 is unmistakable observed in the data. As expected, the number of passengers drop during January (related to the longest school holiday periods in the ACT). But it is during April 2020 where the biggest drop of passenger journeys in the ACT. There is a gradual increase from this time up until May-June 2021, and the numbers drop again for August. This last drop, again, shows how COVID19 affects the movement of passengers."
                  )),
                
                
                highchartOutput('journey_ts_all'),
                p(em("Source: "), a("Daily Public Transport Passenger Journeys by Service Type", href="https://www.data.act.gov.au/Transport/Daily-Public-Transport-Passenger-Journeys-by-Servi/nkxy-abdj", target="_blank")),
                p(strong("Journeys by Service")),
                echarts4rOutput('journey_type_ts2_all')
              )
            ),
            tabPanel(
              title = "Private Vehicules and Motive Power",
              bs4Callout(
                title = "",
                elevation = 4,
                width = 12,
                status = "warning",
                
                
                fluidRow(
                  p(strong("Total Number of Registrations across Time")),
                  popover(
                    actionButton("ts_pop_content_private", "Assessment", status = 'success'),
                    title = "What the data shows",
                    placement = "right",
                    content = "The data shows that the number of vehicle registrations vary depending on the year and there does not seem to be a rigid seasonal pattern. However, January seems to be a month where more registrations start to increase. This may be related to people starting to work and then going to school in February.\nAlso, there is a gradual downward major trend in the timeframe observed, which can be related to people using more public transportation, especially during 2020. That year shows the lowest number of registrations (November 2020) and also a more stabilisation of the downward pattern."
                  )),
                
                
                highchartOutput('personal_ts_all'),
                p(em("Source: "), a("Vehicle registrations by motive power", href="https://www.data.act.gov.au/Transport/Vehicle-registrations-by-motive-power/x4hp-vihn", target="_blank")),
                
                
                fluidRow(
                  p(strong("Breakdown by Motive Power")),
                  popover(
                    actionButton("ts_pop_content_motivepower", "Assessment", status = 'success'),
                    title = "What the data shows",
                    placement = "right",
                    content = "Diesel and Petrol vehicles are by far the most numerous in the data, with Diesel at the top and Petrol behind. The other vehicule types show a more steady pattern across time, with Hybrid cars shows a very gradual increase.\nBut a good sign is that Petrol and Diesel vehicles start showing a gradual downward trend from July 2019, with petrol vehicles starting earlier than Diesel vehicles."
                  )),
                
                
                highchartOutput('personal_ts_power'),
                
                
                fluidRow(
                  p(strong("Breakdown by MAI Insurance class")),
                  popover(
                    actionButton("ts_pop_content_insurance", "Assessment", status = 'success'),
                    title = "What the data shows",
                    placement = "right",
                    content = "The type on insurance shows two diverging patterns. From July 2015 till January 2018, Organisations shows a gradual lowering of the number of registrations, then it peaks up again until July 2021.\nOn the other hand, the Person insurance shows a sharp increase from July 2015 until January 2017. Then it gradually starts falling up until April 2021."
                  )),
                
                highchartOutput('personal_ts_insurance')
              )
            )
          )
        ),
        tabItem(
          tabName = "wl_item",
          tabsetPanel(
            id = "tabset_wl",
            #selected = "Tab 2",
            tabPanel(
              title = "Lower Wild life Accidents",
              
              bs4Callout(
                title = "Wildlife Spots",
                elevation = 4,
                width = 12,
                status = "warning",
                p("Each point represents a wildlife accident recorded."),
                
                leafletOutput("map"),
                fluidRow(
                  column(3,
                         uiOutput('wl_bus_ui')
                  ),
                  column(3,
                         uiOutput('wl_species_stats_ui')
                  )
                )
              ),
              fluidRow(
                column(9, offset = 4,
                       bs4Callout(
                         title = "",
                         elevation = 4,
                         width = 4,
                         status = "warning",
                         source(file.path("ui", "imagesurls.R"),  local = TRUE)$value
                       )
                )
              ),
              bs4Callout(
                title = "",
                elevation = 4,
                width = 12,
                status = "warning",
                highchartOutput('wl_species_ts'),
                highchartOutput('wl_species'),
                highchartOutput('wl_dn'),
                fluidRow(
                  column(4,
                         highchartOutput('wl_species_top')
                  ),
                  column(4,
                         highchartOutput('wl_speciesgroup')
                  ),
                  column(4,
                         highchartOutput('wl_dn_top')
                  )
                ),
                fluidRow(
                  column(6,
                         highchartOutput('wl_day_word')
                  ),
                  column(6,
                         highchartOutput('wl_pod')
                  )
                ),
                fluidRow(
                  column(6,
                         highchartOutput('wl_month_word')
                  ),
                  column(6,
                         highchartOutput('wl_year')
                  )
                )
              )
              
              
            ),
            tabPanel(
              title = "Call",
              bs4Callout(
                title = "In case of a collision, please call any of the following numbers:",
                elevation = 4,
                width = 12,
                status = "warning",
                tags$ul(
                  tags$li(strong('WIRES'), '1300 094 737'),
                  tags$li(strong('ACT Snake Removals'), '0450 210 090'),
                  tags$li(strong('ACT Wildlife'), '0432 300 033'),
                  tags$li(strong('Canberra Snake Rescue & Relocation'), '0405 405 304'),
                  tags$li(strong('Native Animal Rescue Group'), '1300 094 737'),
                  tags$li(strong('Wildcare Queanbeyan'), '(02) 6299 1966')
                )
              )
            ),
            tabPanel(
              title = "Importance of ACT Wildlife",
              bs4Callout(
                title = "",
                elevation = 4,
                width = 12,
                status = "warning",
                p(strong("Protecting ACT Motorists and Wildlife")),
                p("The motivation of this tool is to help decrease the number of collisions between motorists and animals in ACT roads. The gravity of this issue has been presented by ", strong('Canberra Weekly'), " on ", em("August 17, 2020"), " and you can read the post by following this ",
                  a("link", href="https://canberraweekly.com.au/canberra-roads-worst-hotspot-for-animal-collisions/", target="_blank"), '.'),
                p('The following information, as well as the content on ', strong('What to do?'),
                  ', is extracted from this post.'),
                p(tags$ul('"', em("An analysis of more than 21 thousand AAMI animal collision claims between 1 February 2019 and 31 January 2020 revealed Canberra as the country’s most dangerous hotspot. This refers to postcodes 2601 and 2600 which encompass areas including the "), strong('City, Acton, Barton, Harman and Yarralumla'), em(".
                 
                 Locally, the top five animal collision hotspots in the ACT are listed as "), strong('Canberra, Kambah, Belconnen, Hume and Symonston'), em(".
                 
                 AAMI’s data found motorists are most likely to experience a major collision with a "), strong('kangaroo (84%), wallaby (5%), wombat (2%), deer (2%) or bird (1%)'), em(". The worst day of the week for animal crashes is "), strong('Friday, followed by the weekend.'), em('"')))
              )
            ),
            tabPanel(
              title = "What to do?",
              bs4Callout(
                title = "What to do in case of a collision:",
                elevation = 4,
                width = 12,
                status = "warning",
                p(a("The ACT Environment, Planning and Sustainable Development Directorate", href="https://www.environment.act.gov.au/parks-conservation/plants-and-animals/urban-wildlife/kangaroos/kangaroos_and_vehicles", target="_blank"), " website has some helpful information for motorists about kangaroos and vehicles, including a list of hot spots for collisions, and what to do if you have been in an accident involving wildlife."),
                p(strong('These are some tips from AAMI:')),
                tags$ul(
                  tags$li(em("If you notice roadkill, "), strong('slow down and pay extra attention'), em(". It’s an indicator of wildlife in the area. If you spot a kangaroo crossing the road, it’s a sign that more kangaroos will be following as they move in groups.")),
                  tags$li(em("If you see an animal on the road, "), strong('slow down and brake'), em(", but avoid swerving so as not to endanger yourself and other drivers on the road. It’s far less dangerous to keep driving and damage your car than swerve to avoid the animal and collide with another vehicle or tree.")),
                  tags$li(em("If you’re involved in an animal collision, "), strong('stop to check its welfare'), em(", but only if it is safe to do so. If the animal is alive and injured, "), strong('call WIRES or your local wildlife rescue service.')),
                  tags$li(em("If it’s a dead kangaroo, "), strong('check if it is a female and if there’s a joey(s) in her pouch or around her'), em(". Pouches/flaps of wombats and echidnas should also be checked as well as the surrounding area, as young echidnas are often dislodged during a vehicle collision.")),
                  tags$li(strong('Drive slowly and be extra vigilant'), em(" when driving at dawn or dusk, as this is when animals are most active.")),
                  tags$li(strong('Use your peripheral vision and be aware of your surroundings'), em(", especially when travelling through forest or grassland areas where animals are not clearly visible."))
                )
              )
            )
            
          )
        ),
        tabItem(
          tabName = "path_item",
          tabsetPanel(
            id = "path_item_1",
            #selected = "Tab 2",
            tabPanel(
              title = "Things to do near Bus Stop",
              
              box(status = 'warning',
                  title = 'Find closest places close to current Bus Stop',
                  elevation = 4, width = 12,
                  leafletOutput("map_path"),
                  uiOutput('path_bus_ui')
              ),
              uiOutput('path_tl_ui')
            ),
            tabPanel(title = 'Know ACT Playgrounds',
                     
                     box(elevation = 4,width = 12,
                         
                         actionButton("update", "Filter Data"),
                         sidebar = boxSidebar(
                           id = "mycardsidebar",
                           uiOutput('pg_type_ui'),
                           uiOutput('pg_suburb_ui'),
                           uiOutput('pg_year_ui'),
                           uiOutput('pg_parking_ui'),
                           uiOutput('pg_shade_ui'),
                           uiOutput('pg_ind_ui')
                         ),
                         fluidRow(
                           column(4,
                                  bs4Callout(
                                    title = "Map Location",
                                    elevation = 4,
                                    width = 12,
                                    status = "warning",
                                    leafletOutput("map_pg")
                                  )
                           ),
                           column(8,
                                  bs4Callout(
                                    title = "Shade Level",
                                    elevation = 4,
                                    width = 12,
                                    status = "warning",
                                    amChart4Output("shadelevel", height = "400px")
                                  )
                           )
                           
                         )
                     ),
                     fluidRow(
                       valueBoxOutput('tree_shade_ui', width = 4),
                       valueBoxOutput('sail_shade_ui', width = 4),
                       valueBoxOutput('shelter_shade_ui', width = 4)
                     ),
                     fluidRow(
                       infoBoxOutput('pg_ui_location'),
                       infoBoxOutput('pg_ui_parking'),
                       infoBoxOutput('pg_ui_trees')
                     ),
                     fluidRow(
                       infoBoxOutput('pg_ui_year'),
                       infoBoxOutput('pg_ui_area'),
                       infoBoxOutput('pg_ui_perimeter')
                     )
                     
            ),
            
            tabPanel(title = 'ACT Ammenities',
                     p('Each point/heatmap represents a place/object of interest.'),
                     p('Click on Circle Markers to expand information on items.'),
                     fluidRow(
                       uiOutput('select_all_ui'),
                       uiOutput('select_all_cat_ui')
                     ),
                     box(elevation = 4, width = 12,
                         leafletOutput("map_all")
                     ),
                     fluidRow(
                       infoBoxOutput('info_ui_object'),
                       infoBoxOutput('info_ui_type'),
                       infoBoxOutput('info_ui_dv'),
                       infoBoxOutput('info_ui_location')
                     ),
                     box(title = 'By Suburb',
                         status = 'danger',
                         width = 12,
                         echarts4rOutput("bysuburb")
                     ),
                     box(title = 'By Type',
                         status = 'danger',
                         width = 12,
                         echarts4rOutput("bytype")
                     )     
            )
            
            
          )
        )
      )
      
      
    ),
    controlbar = dashboardControlbar(),
    title = "DashboardPage"
  ),
  server = function(input, output, session) { 
    
    show_modal_spinner(text = 'Please wait...')
    
    `%nin%` <- Negate(`%in%`)
    
    specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))
    
    dist_funct <- function(long1, lat1, long2, lat2, metric='ms'){
      distout <- as.numeric(specify_decimal(as.numeric(distm(c(long1, lat1), 
                                                             c(long2, lat2), 
                                                             fun = distHaversine)), 1))
      
      if(metric == 'kms'){
        distout <- distout / 1000
      }
      
      return(distout)
    }
    
    pal <- colorNumeric("viridis", NULL)
    
    #path files.............................................................
    #import csv files
    df_dns <- as.data.frame(fread('./infiles/df_dns.csv'))
    df_dns_allcounts <- as.data.frame(fread('./infiles/df_dns_allcounts.csv'))
    bbqs_n <- as.data.frame(fread('./infiles/bbqs_n.csv'))
    bbs_n <- as.data.frame(fread('./infiles/bbs_n.csv'))
    tables_n <- as.data.frame(fread('./infiles/tables_n.csv'))
    seats_n <- as.data.frame(fread('./infiles/seats_n.csv'))
    pedestrians_n <- as.data.frame(fread('./infiles/pedestrians_n.csv'))
    dogs_n <- as.data.frame(fread('./infiles/dogs_n.csv'))
    skates_n <- as.data.frame(fread('./infiles/skates_n.csv'))
    toilets_complex_all <- as.data.frame(fread('./infiles/toilets_complex_all.csv'))
    playgrounds_complex_df <- as.data.frame(fread('./infiles/playgrounds_complex_df.csv'))
    
    df_wellbeing <- as.data.frame(fread('./infiles/df_wellbeing.csv'))
    
    playgrounds_complex <- as.data.frame(fread('./infiles/playgrounds_complex.csv'))
    
    in_geo <- rgdal::readOGR("./infiles/ACT Division Boundaries.geojson")
    
    in_geo@data <- in_geo@data %>% 
      left_join(df_dns_allcounts, by = 'DIVISION_NAME') %>%
      left_join(bbqs_n, by = 'DIVISION_NAME') %>% 
      left_join(bbs_n, by = 'DIVISION_NAME') %>% 
      left_join(tables_n, by = 'DIVISION_NAME') %>% 
      left_join(seats_n, by = 'DIVISION_NAME') %>% 
      left_join(pedestrians_n, by = 'DIVISION_NAME') %>% 
      left_join(dogs_n, by = 'DIVISION_NAME') %>% 
      left_join(skates_n, by = 'DIVISION_NAME') %>% 
      left_join(toilets_complex_all, by = 'DIVISION_NAME') %>% 
      left_join(playgrounds_complex_df, by = 'DIVISION_NAME') %>%
      replace(is.na(.), 0)
    
    #ACT Division Boundaries..................................................
    in_csv <- as.data.frame(fread("./infiles/wildlife_accidents.csv"))
    in_csv <- in_csv %>% filter(dist < 100) %>% filter(SPECIES %nin% c('OTHER')) %>%
      filter(SPECIES != 'EASTERN GREY KANGAROO') %>%
      mutate(nn = 1)
    
    in_csv$date <- gsub(' .*', '', in_csv$CREATED_DATE)
    in_csv$asdate <- as.Date(in_csv$date, format = '%m/%d/%Y')
    in_csv$day <- day(in_csv$asdate)
    in_csv$month <- month(in_csv$asdate)
    in_csv$year <- year(in_csv$asdate)
    in_csv$monthyear <- format(in_csv$asdate, "%Y-%m")
    in_csv$dayname <- weekdays(in_csv$asdate)
    in_csv$monthname <- month(in_csv$asdate, label = TRUE)
    in_csv$timeall <- sub('.+? ', '', in_csv$CREATED_DATE)
    in_csv$time <- sub(' .*', '', in_csv$timeall )
    
    points <- eventReactive(input$recalc, {
      cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
    }, ignoreNULL = FALSE)
    
    load("./infiles/df_act_hist.RData")
    load("./infiles/df_journey_long.RData")
    load("./infiles/df_journey.RData")
    load("./infiles/df_personal.RData")
    load("./infiles/tmplist_busids.RData")
    
    dfstops <- fread('./infiles/dat_stops.csv')
    
    df_hist <- df_act_hist
    
    dfc <- df_hist %>% group_by(asdate) %>% count()
    
    #dfc <- df %>% group_by(asdate) %>% count() #%>%
    #mutate(date = as.POSIXct(asdate, format="%Y-%m-%d"))
    
    dfc_group_pod <- df_hist %>% group_by(asdate, pod) %>% count() %>% 
      replace(is.na(.), 0) #%>%
    #mutate(date = as.POSIXct(asdate, format="%Y-%m-%d"))
    
    dfc_group_day <- df_hist %>% group_by(asdate, dayname) %>% count() %>% 
      replace(is.na(.), 0) #%>%
    #mutate(date = as.POSIXct(asdate, format="%Y-%m-%d"))
    #
    
    #wrangle journey data
    df_journey_group <- df_journey_long %>% group_by(monthyear, type) %>% 
      summarise(n = sum(count, na.rm = TRUE)) %>%
      rename(`Date` = monthyear, `Number of passangers` = n)
    
    df_journey_tsprep <- df_journey %>% select(asdate, `Local Route`:Other) 
    names(df_journey_tsprep) <- gsub(' ','',names(df_journey_tsprep))
    df_journey_group_ts <- ts(data = df_journey_tsprep[-1],
                              start = c(2019, 7),
                              end = c(2020, 8),
                              frequency = 12)
    
    #plot values
    df_journey_group_nots <- df_journey_long %>% group_by(type) %>% 
      summarise(n = sum(count, na.rm = TRUE)) %>%
      arrange(n)
    
    #df personal data ....................................................................................
    df_personal_trend <- df_personal %>% group_by(asdate) %>% 
      count() %>%
      rename(Date = asdate, Total = n)
    
    df_personal_power <- df_personal %>% group_by(asdate, Motivepower) %>% 
      count() %>%
      rename(Date = asdate, Total = n)
    
    df_personal_mai <- df_personal %>% group_by(asdate, MAIInsuranceclass) %>% 
      count() %>%
      rename(Date = asdate, Total = n)
    
    
    remove_modal_spinner()
    
    output$act_ts_all <- renderHighchart({
      hchart(dfc, "spline", hcaes(x = asdate, y = n))
    })
    
    output$act_ts_pod <- renderHighchart({
      hchart(dfc_group_pod, "spline", hcaes(x = asdate, y = n, group = pod))
    })
    
    output$act_ts_day <- renderHighchart({
      hchart(dfc_group_day, "spline", hcaes(x = asdate, y = n, group = dayname))
    })
    
    output$journey_ts_all <- renderHighchart({
      hchart(df_journey_group, "spline", hcaes(x = `Date`, y = `Number of passangers`, group = type))
    })
    
    output$journey_type_ts2_all <- renderEcharts4r({
      df_journey_group_nots %>%
        e_charts(type) %>% 
        e_pie(n, roseType = "radius")
    })
    
    
    #add Personal..................
    output$personal_ts_all <- renderHighchart({
      hchart(df_personal_trend, "spline", hcaes(x = Date, y = Total))
    })
    output$personal_ts_power <- renderHighchart({
      hchart(df_personal_power, "spline", hcaes(x = Date, y = Total, group =  Motivepower))
    })
    output$personal_ts_insurance <- renderHighchart({
      hchart(df_personal_mai, "spline", hcaes(x = Date, y = Total, group =  MAIInsuranceclass))
    })
    
    
    
    
    #maps..............................
    
    output$wl_bus_ui <- renderUI({
      #tmpchoices <- sort(unique(dfstops$stop_name))
      
      #create listing
      tmplist <- tmplist_busids
      
      # for(i in LETTERS){
      #   for(j in tmpchoices){
      #     if(length(grep(paste0('^', i), tmpchoices)) != 0){
      #       if(length(grep(paste0('^', i), tmpchoices)) == 1){
      #         tmplist[[tmpchoices[grep(paste0('^', i), tmpchoices)]]] <- 
      #           tmpchoices[grep(paste0('^', i), tmpchoices)]
      #       }else(
      #         tmplist[[i]] <- tmpchoices[grep(paste0('^', i), tmpchoices)] 
      #       )
      #     }
      #   }
      # }
      # 
      # tmplist_busids <- tmplist
      # save(tmplist_busids, file = "./infiles/tmplist_busids.RData")
      
      selectInput('wl_bus', label = 'Choose Reference Bus Stop', 
                  choices = tmplist, 
                  multiple = FALSE, selected = tmplist[[1]][1])
    })
    
    output$wl_species_stats_ui <- renderUI({
      tmpchoices <- c('All', sort(unique(in_csv$SPECIES)))
      
      #create listing
      tmplist <- list()
      
      for(i in LETTERS){
        for(j in tmpchoices){
          if(length(grep(paste0('^', i), tmpchoices)) != 0){
            if(length(grep(paste0('^', i), tmpchoices)) == 1){
              tmplist[[tmpchoices[grep(paste0('^', i), tmpchoices)]]] <- 
                tmpchoices[grep(paste0('^', i), tmpchoices)]
            }else(
              tmplist[[i]] <- tmpchoices[grep(paste0('^', i), tmpchoices)] 
            )
          }
        }
      }
      
      selectInput('wl_species_stats', label = 'Choose Species to see stats', 
                  choices = tmplist, 
                  multiple = TRUE, selected = tmplist[[1]][1])
    })
    
    
    output$map <- renderLeaflet({
      
      if(is.null(input$wl_bus))
        return()
      
      if(is.null(input$wl_species_stats))
        return()
      
      dfstops_in <- dfstops %>% filter(stop_name == input$wl_bus)
      
      if(is.null(input$wl_bus)){
        busidref_lat <- as.numeric(dfstops$stop_lat[1])
        busidref_long <- as.numeric(dfstops$stop_lon[1])
        busidref_pop <- dfstops$stop_name[1]
      }else{
        busidref_lat <- as.numeric(dfstops_in$stop_lat)
        busidref_long <- as.numeric(dfstops_in$stop_lon)
        busidref_pop <- dfstops_in$stop_name
      }
      
      if(input$wl_species_stats == 'All'){
        in_csv_in <- in_csv
      }else{
        in_csv_in <- in_csv %>% filter(SPECIES %in% input$wl_species_stats)
      }
      
      colourcol <- 'SPECIES'
      factpal <- colorFactor(viridis::plasma(n = length(unique(in_csv_in[[colourcol]]))), 
                             in_csv_in[[colourcol]], )
      
      incolornamessmall <- c("red", "orange", "beige", "green", "blue", "purple", "pink", "cadetblue", "white", "lightred", "lightgreen", "lightblue", "lightgray", "darkred", "darkgreen", "darkblue", "darkpurple", "gray", "black")
      
      colorsn <- colors()[2:(length(unique(unique(in_csv_in[[colourcol]]))) + 1)]
      
      mapColour <- setNames(colorsn,
                            unique(in_csv_in[[colourcol]]))
      
      # add icon label column
      df_withIcon <- in_csv_in %>%
        mutate(icon = case_when(
          speciesGroup == "Bird" ~ "paw",
          speciesGroup == "Fish" ~ "paw",
          speciesGroup == "Frog" ~ "paw",
          speciesGroup == "Lizard" ~ "paw",
          speciesGroup == "Mammal" ~ "paw",
          speciesGroup == "Other" ~ "paw",
          speciesGroup == "Snake" ~ "paw")) %>%
        mutate(color = case_when(
          speciesGroup == "Bird" ~ "red",
          speciesGroup == "Fish" ~ "orange",
          speciesGroup == "Frog" ~ "green",
          speciesGroup == "Lizard" ~ "blue",
          speciesGroup == "Mammal" ~ "purple",
          speciesGroup == "Other" ~ "beige",
          speciesGroup == "Snake" ~ "cadetblue"))
      
      IconSet <- awesomeIconList(
        "Bird"   = makeAwesomeIcon(icon= 'paw', markerColor = 'red', iconColor = 'white', library = "fa"),
        "Fish" = makeAwesomeIcon(icon= 'paw', markerColor = 'orange', iconColor = 'white', library = "fa"),
        "Frog" = makeAwesomeIcon(icon= 'paw', markerColor = 'green', iconColor = 'white', library = "fa"),
        "Lizard" = makeAwesomeIcon(icon= 'paw', markerColor = 'blue', iconColor = 'white', library = "fa"),
        "Mammal" = makeAwesomeIcon(icon= 'paw', markerColor = 'purple', iconColor = 'white', library = "fa"),
        "Other" = makeAwesomeIcon(icon= 'paw', markerColor = 'beige', iconColor = 'white', library = "fa"),
        "Snake" = makeAwesomeIcon(icon= 'paw', markerColor = 'cadetblue', iconColor = 'white', library = "fa")
      )
      
      #With heatmap.................................................
      
      leaflet() %>%
        setView(lng=busidref_long, 
                lat=busidref_lat, zoom = 15) %>%
        addTiles() %>%
        addMarkers(lng = busidref_long,
                   lat = busidref_lat, popup = busidref_pop, 
                   label = busidref_pop) %>%
        addHeatmap(data = df_withIcon,
                   lng = ~LONGITUDE, lat = ~LATITUDE,
                   intensity = ~nn,
                   blur = 20, max = 0.05, radius = 15,
                   group = 'Heatmap'
        )  %>% 
        addCircleMarkers(data = df_withIcon, 
                         lng = ~LONGITUDE, lat = ~LATITUDE, 
                         radius = 8,
                         color = 'blue',
                         stroke = FALSE, fillOpacity = 0.7,
                         group = 'Circles') %>%
        addAwesomeMarkers(data = df_withIcon, 
                          lng = ~LONGITUDE, lat = ~LATITUDE, 
                          label = ~SPECIES,
                          clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F),
                          group = 'Clusters', 
                          #icon = my_icons,
                          icon = ~IconSet[speciesGroup]) %>%
        addPopups(data = df_withIcon, 
                  lng = ~LONGITUDE, lat = ~LATITUDE,
                  popup = ~SPECIES,
                  options = popupOptions(closeButton = FALSE),
                  group = 'Labels'
        ) %>%
        addFullscreenControl() %>%
        addResetMapButton() %>%
        addLayersControl(
          overlayGroups = c("Heatmap", "Circles", 'Clusters', "Labels"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>% 
        hideGroup("Circles") %>%
        hideGroup("Clusters") %>%
        hideGroup("Labels") %>%
        addProviderTiles(providers$CartoDB.Positron)
    })
    
    #Build animal statistics
    observe({
      #build timeline
      ## All animal statistics
      
      if(is.null(input$wl_species_stats))
        return()
      
      if(input$wl_species_stats == 'All'){
        in_csv_in <- in_csv
      }else{
        in_csv_in <- in_csv %>% filter(SPECIES %in% input$wl_species_stats)
      }
      
      output$wl_species_ts <- renderHighchart({
        as.data.frame(in_csv_in %>% group_by(monthyear) %>% count() %>%
                        rename(Date=monthyear, Total = n)) %>% 
          hchart("line", hcaes(x = Date, y = Total)) %>%
          hc_add_theme(hc_theme_darkunica())
      })
      
      output$wl_species <- renderHighchart({
        as.data.frame(in_csv_in %>% filter(SPECIES != 'EASTERN GREY KANGAROO') %>%
                        group_by(SPECIES) %>% count() %>%
                        rename(Total = n) %>% 
                        ungroup() %>%
                        arrange(desc(Total))) %>% 
          hchart("column", hcaes(x = SPECIES, y = Total, color = SPECIES))%>%
          hc_add_theme(hc_theme_darkunica())
      })
      
      output$wl_speciesgroup <- renderHighchart({
        as.data.frame(in_csv_in %>% filter(SPECIES != 'EASTERN GREY KANGAROO') %>%
                        group_by(speciesGroup) %>% count() %>%
                        rename(Total = n) %>% 
                        ungroup() %>%
                        arrange(desc(Total))) %>% 
          hchart("lollipop", hcaes(x = speciesGroup, y = Total, color = speciesGroup))%>%
          hc_add_theme(hc_theme_darkunica())
      })
      
      output$wl_species_top <- renderHighchart({
        as.data.frame(in_csv_in %>% filter(SPECIES != 'EASTERN GREY KANGAROO') %>%
                        group_by(SPECIES) %>% count() %>%
                        rename(Total = n) %>% 
                        ungroup() %>%
                        top_n(10) %>%
                        arrange(desc(Total))) %>% 
          hchart("bubble", hcaes(x = SPECIES, y = Total, size = Total, 
                                 color = SPECIES))%>%
          hc_add_theme(hc_theme_darkunica())
      })
      
      output$wl_dn <- renderHighchart({
        as.data.frame(in_csv_in %>% group_by(DIVISION_NAME) %>% count() %>%
                        rename(Total = n) %>% 
                        ungroup() %>%
                        arrange(desc(Total))) %>% 
          hchart("column", hcaes(x = DIVISION_NAME, y = Total, color = DIVISION_NAME))%>%
          hc_add_theme(hc_theme_darkunica())
      })
      
      output$wl_dn_top <- renderHighchart({
        as.data.frame(in_csv_in %>% group_by(DIVISION_NAME) %>% count() %>%
                        rename(Total = n) %>% 
                        ungroup() %>%
                        top_n(10) %>%
                        arrange(desc(Total))) %>% 
          hchart("item", hcaes(x = DIVISION_NAME, y = Total, 
                               value = Total, color = DIVISION_NAME))%>%
          hc_add_theme(hc_theme_darkunica())
      })
      
      output$wl_month_word <- renderHighchart({
        as.data.frame(in_csv_in %>% group_by(month_word) %>% count() %>%
                        rename(Total = n) %>% 
                        ungroup() %>%
                        top_n(10) %>%
                        arrange(desc(Total))) %>% 
          hchart("wordcloud", hcaes(x = month_word, y = Total, 
                                    weight = Total, color = month_word))%>%
          hc_add_theme(hc_theme_darkunica())
      })
      
      output$wl_day_word <- renderHighchart({
        as.data.frame(in_csv_in %>% group_by(day_word) %>% count() %>%
                        rename(Total = n) %>% 
                        ungroup() %>%
                        top_n(10) %>%
                        arrange(desc(Total))) %>% 
          hchart("column", hcaes(x = day_word, y = Total, 
                                 color = day_word)) %>%
          hc_chart(polar = TRUE)%>%
          hc_add_theme(hc_theme_darkunica())
      })
      
      output$wl_pod <- renderHighchart({
        as.data.frame(in_csv_in %>% group_by(pod) %>% count() %>%
                        rename(Total = n) %>% 
                        ungroup() %>%
                        top_n(10) %>%
                        arrange(desc(Total))) %>% 
          hchart("column", hcaes(x = pod, y = Total, 
                                 color = pod))%>%
          hc_add_theme(hc_theme_darkunica())
      })
      
      output$wl_year <- renderHighchart({
        as.data.frame(in_csv_in %>% 
                        mutate(year = factor(year)) %>%
                        group_by(year) %>% count() %>%
                        rename(Total = n) %>% 
                        ungroup() %>%
                        top_n(10) %>%
                        arrange(desc(Total))) %>% 
          hchart("waterfall", hcaes(x = year, y = Total, 
                                    weight = Total, color = year))%>%
          hc_add_theme(hc_theme_darkunica())
      })
      
    })
    output$info_ui_dv <- renderInfoBox({
      
      if(is.null(input$map_all_marker_click)){
        return()
      }else{
        
        p <- input$map_all_marker_click
        pclick_lat <- input$map_all_marker_click$lat
        pclick_lng <- input$map_all_marker_click$lng
        
        df_dns_click <- df_dns %>% filter(lat == pclick_lat & long == pclick_lng)
        df_dns_click <- df_dns_click[1,]
        
        infoBox(
          title = "SUBURB",
          color = "info",
          value = as.character(df_dns_click$DIVISION_NAME),
          icon = icon("building")
        )
      }
      
    })
    
    #My path===============================================
    output$path_bus_ui <- renderUI({
      #create listing
      tmplist <- tmplist_busids
      
      selectInput('path_bus', label = 'Choose Reference Bus Stop', 
                  choices = tmplist, 
                  multiple = FALSE, selected = tmplist[[1]][1])
    })
    
    output$map_path <- renderLeaflet({
      
      if(is.null(input$path_bus))
        return()
      
      dfstops_in <- dfstops %>% filter(stop_name == input$path_bus)
      
      if(is.null(input$path_bus)){
        busidref_lat <- as.numeric(dfstops$stop_lat[1])
        busidref_long <- as.numeric(dfstops$stop_lon[1])
        busidref_pop <- dfstops$stop_name[1]
      }else{
        busidref_lat <- as.numeric(dfstops_in$stop_lat)
        busidref_long <- as.numeric(dfstops_in$stop_lon)
        busidref_pop <- dfstops_in$stop_name
      }
      
      show_modal_spinner(text = 'Please wait...') 
      
      #get closest fields
      tmplocexample <- c(lng=busidref_long, lat=busidref_lat, poplabel = busidref_pop)
      
      df_dns_custom <- df_dns
      df_dns_custom$long_ref = as.numeric(tmplocexample[1])
      df_dns_custom$lat_ref = as.numeric(tmplocexample[2])
      
      df_dns_custom$dist <- apply(df_dns_custom[,c('long_ref','lat_ref', 'long', 'lat')], 1, 
                                  function(y) dist_funct(y['long_ref'],y['lat_ref'],
                                                         y['long'],y['lat'], 'kms'))
      
      df_dns_custom_unique <- df_dns_custom %>%
        group_by(OBJECT) %>%
        filter(dist == min(dist)) %>%
        arrange(dist)
      
      IconSet <- awesomeIconList(
        "Barbeque"   = makeAwesomeIcon(icon= 'fire', markerColor = 'lightred', iconColor = 'white', library = "fa"),
        "Basketball Court" = makeAwesomeIcon(icon= 'glyphicon glyphicon-flag', markerColor = 'orange', iconColor = 'white', library = "glyphicon"),
        "Cyclist" = makeAwesomeIcon(icon= 'glyphicon glyphicon-screenshot', markerColor = 'green', iconColor = 'white', library = "glyphicon"),
        "Dog park" = makeAwesomeIcon(icon= 'glyphicon glyphicon-tree-deciduous', markerColor = 'blue', iconColor = 'white', library = "glyphicon"),
        "Pedestrian" = makeAwesomeIcon(icon= 'glyphicon glyphicon-user', markerColor = 'green', iconColor = 'white', library = "glyphicon"),
        "Playground" = makeAwesomeIcon(icon= 'map-pin', markerColor = 'pink', iconColor = 'white', library = "fa"),
        "Seat" = makeAwesomeIcon(icon= 'glyphicon glyphicon-bed', markerColor = 'cadetblue', iconColor = 'white', library = "glyphicon"),
        "Skate park" = makeAwesomeIcon(icon= 'glyphicon glyphicon-road', markerColor = 'orange', iconColor = 'white', library = "glyphicon"),
        "Table" = makeAwesomeIcon(icon= 'glyphicon glyphicon-bookmark', markerColor = 'cadetblue', iconColor = 'white', library = "glyphicon"),
        "Toilet" = makeAwesomeIcon(icon= 'glyphicon glyphicon-home', markerColor = 'cadetblue', iconColor = 'white', library = "glyphicon")
      )
      
      remove_modal_spinner() 
      
      #plot on map
      leaflet() %>%
        setView(lng=as.numeric(tmplocexample[1]), 
                lat=as.numeric(tmplocexample[2]), zoom = 17) %>%
        addTiles() %>%
        addMarkers(lng = as.numeric(tmplocexample[1]),
                   lat = as.numeric(tmplocexample[2]), popup = tmplocexample[3], 
                   label = tmplocexample[3]) %>%
        addAwesomeMarkers(data = df_dns_custom_unique, 
                          lng = ~long, lat = ~lat, 
                          label = ~OBJECT,
                          #clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F),
                          group = 'Clusters', 
                          #icon = my_icons,
                          icon = ~IconSet[OBJECT]) %>%
        addProviderTiles(providers$CartoDB.Positron)
    })
    
    #Create time line
    output$path_tl_ui <- renderUI({
      if(is.null(input$path_bus))
        return()
      
      dfstops_in <- dfstops %>% filter(stop_name == input$path_bus)
      
      if(is.null(input$path_bus)){
        busidref_lat <- as.numeric(dfstops$stop_lat[1])
        busidref_long <- as.numeric(dfstops$stop_lon[1])
        busidref_pop <- dfstops$stop_name[1]
      }else{
        busidref_lat <- as.numeric(dfstops_in$stop_lat)
        busidref_long <- as.numeric(dfstops_in$stop_lon)
        busidref_pop <- dfstops_in$stop_name
      }
      
      #get closest fields
      tmplocexample <- c(lng=busidref_long, lat=busidref_lat, poplabel = busidref_pop)
      
      df_dns_custom <- df_dns
      df_dns_custom$long_ref = as.numeric(tmplocexample[1])
      df_dns_custom$lat_ref = as.numeric(tmplocexample[2])
      
      df_dns_custom$dist <- apply(df_dns_custom[,c('long_ref','lat_ref', 'long', 'lat')], 1, 
                                  function(y) dist_funct(y['long_ref'],y['lat_ref'],
                                                         y['long'],y['lat'], 'kms'))
      
      df_dns_custom_unique <- df_dns_custom %>%
        group_by(OBJECT) %>%
        filter(dist == min(dist)) %>%
        arrange(dist) %>%
        mutate(lbl = ifelse(dist >= 1, paste0(round(dist), ' kilometers'), 
                            paste0(round((dist* 1000)), ' meters')),
               object_label = case_when(
                 OBJECT == 'Table' ~ 'Closest Table',
                 OBJECT == 'Seat' ~ 'Closest Seat',
                 OBJECT == 'Toilet' ~ 'Need a toilet?',
                 OBJECT == 'Pedestrian' ~ 'Warning: Closest pedestrian incident',
                 OBJECT == 'Cyclist' ~ 'Warning: Closest cyclist incident',
                 OBJECT == 'Skate park' ~ 'Feel like skating?',
                 OBJECT == 'Basketball Court' ~ 'Shooting some hoops?',
                 OBJECT == 'Playground' ~ 'Closest Playground',
                 OBJECT == 'Barbeque' ~ 'Cooking some barbie?',
                 OBJECT == 'Dog park' ~ 'Closest Dog park'
               ),
               TYPE = gsub('_', ' ', TYPE))
      
      IconSet <- data.frame(OBJECT = c("Barbeque", "Basketball Court", "Cyclist", "Dog park", "Pedestrian", "Playground", "Seat", "Skate park", "Table", "Toilet"), markerColor = c('danger', 'orange', 'success', 'primary', 'lime', 'pink', 'warning', 'indigo', 'lightblue', 'teal'), icon = c("fire", "map-pin", "exclamation", "map", "exclamation", "tree", "thumbtack", "map-pin", "thumbtack", "thumbtack"), library = c("fa", "fa", "fa", "fa", "fa", "fa", "fa", "fa", "fa", "fa"))
      
      df_dns_custom_unique <- df_dns_custom_unique %>%
        left_join(IconSet, by = 'OBJECT')
      
      box(
        title = "Closest points",
        timelineBlock(
          width = 12,
          elevation = 4,
          reversed = TRUE,
          timelineLabel(df_dns_custom_unique$DIVISION_NAME[1], color = "navy"),
          timelineItem(
            elevation = 4, 
            title = df_dns_custom_unique$object_label[1],
            icon = icon(df_dns_custom_unique$icon[1], lib = df_dns_custom_unique$library[1]),
            color = df_dns_custom_unique$markerColor[1],
            time = df_dns_custom_unique$lbl[1],
            footer = df_dns_custom_unique$TYPE[1],
            df_dns_custom_unique$LOCATION_DESCRIPTION[1]
          )
        ),
        timelineBlock(
          width = 12,
          reversed = TRUE,
          timelineLabel(df_dns_custom_unique$DIVISION_NAME[2], color = "navy"),
          timelineItem(
            elevation = 4, 
            title = df_dns_custom_unique$object_label[2],
            icon = icon(df_dns_custom_unique$icon[2], lib = df_dns_custom_unique$library[2]),
            color = df_dns_custom_unique$markerColor[2],
            time = df_dns_custom_unique$lbl[2],
            footer = df_dns_custom_unique$TYPE[2],
            df_dns_custom_unique$LOCATION_DESCRIPTION[2]
          )
        ),
        timelineBlock(
          width = 12,
          reversed = TRUE,
          timelineLabel(df_dns_custom_unique$DIVISION_NAME[3], color = "navy"),
          timelineItem(
            elevation = 4, 
            title = df_dns_custom_unique$object_label[3],
            icon = icon(df_dns_custom_unique$icon[3], lib = df_dns_custom_unique$library[3]),
            color = df_dns_custom_unique$markerColor[3],
            time = df_dns_custom_unique$lbl[3],
            footer = df_dns_custom_unique$TYPE[3],
            df_dns_custom_unique$LOCATION_DESCRIPTION[3]
          )
        ),
        timelineBlock(
          width = 12,
          reversed = TRUE,
          timelineLabel(df_dns_custom_unique$DIVISION_NAME[4], color = "navy"),
          timelineItem(
            elevation = 4, 
            title = df_dns_custom_unique$object_label[4],
            icon = icon(df_dns_custom_unique$icon[4], lib = df_dns_custom_unique$library[4]),
            color = df_dns_custom_unique$markerColor[4],
            time = df_dns_custom_unique$lbl[4],
            footer = df_dns_custom_unique$TYPE[4],
            df_dns_custom_unique$LOCATION_DESCRIPTION[4]
          )
        ),
        timelineBlock(
          width = 12,
          reversed = TRUE,
          timelineLabel(df_dns_custom_unique$DIVISION_NAME[5], color = "navy"),
          timelineItem(
            elevation = 4, 
            title = df_dns_custom_unique$object_label[5],
            icon = icon(df_dns_custom_unique$icon[5], lib = df_dns_custom_unique$library[5]),
            color = df_dns_custom_unique$markerColor[5],
            time = df_dns_custom_unique$lbl[5],
            footer = df_dns_custom_unique$TYPE[5],
            df_dns_custom_unique$LOCATION_DESCRIPTION[5]
          )
        ),
        timelineBlock(
          width = 12,
          reversed = TRUE,
          timelineLabel(df_dns_custom_unique$DIVISION_NAME[6], color = "navy"),
          timelineItem(
            elevation = 4, 
            title = df_dns_custom_unique$object_label[6],
            icon = icon(df_dns_custom_unique$icon[6], lib = df_dns_custom_unique$library[6]),
            color = df_dns_custom_unique$markerColor[6],
            time = df_dns_custom_unique$lbl[6],
            footer = df_dns_custom_unique$TYPE[6],
            df_dns_custom_unique$LOCATION_DESCRIPTION[6]
          )
        ),
        timelineBlock(
          width = 12,
          reversed = TRUE,
          timelineLabel(df_dns_custom_unique$DIVISION_NAME[7], color = "navy"),
          timelineItem(
            elevation = 4, 
            title = df_dns_custom_unique$object_label[7],
            icon = icon(df_dns_custom_unique$icon[7], lib = df_dns_custom_unique$library[7]),
            color = df_dns_custom_unique$markerColor[7],
            time = df_dns_custom_unique$lbl[7],
            footer = df_dns_custom_unique$TYPE[7],
            df_dns_custom_unique$LOCATION_DESCRIPTION[7]
          )
        ),
        timelineBlock(
          width = 12,
          reversed = TRUE,
          timelineLabel(df_dns_custom_unique$DIVISION_NAME[8], color = "navy"),
          timelineItem(
            elevation = 4, 
            title = df_dns_custom_unique$object_label[8],
            icon = icon(df_dns_custom_unique$icon[8], lib = df_dns_custom_unique$library[8]),
            color = df_dns_custom_unique$markerColor[8],
            time = df_dns_custom_unique$lbl[8],
            footer = df_dns_custom_unique$TYPE[8],
            df_dns_custom_unique$LOCATION_DESCRIPTION[8]
          )
        ),
        timelineBlock(
          width = 12,
          reversed = TRUE,
          timelineLabel(df_dns_custom_unique$DIVISION_NAME[9], color = "navy"),
          timelineItem(
            elevation = 4, 
            title = df_dns_custom_unique$object_label[9],
            icon = icon(df_dns_custom_unique$icon[9], lib = df_dns_custom_unique$library[9]),
            color = df_dns_custom_unique$markerColor[9],
            time = df_dns_custom_unique$lbl[9],
            footer = df_dns_custom_unique$TYPE[9],
            df_dns_custom_unique$LOCATION_DESCRIPTION[9]
          )
        ),
        timelineBlock(
          width = 12,
          reversed = TRUE,
          timelineLabel(df_dns_custom_unique$DIVISION_NAME[10], color = "navy"),
          timelineItem(
            elevation = 4, 
            title = df_dns_custom_unique$object_label[10],
            icon = icon(df_dns_custom_unique$icon[10], lib = df_dns_custom_unique$library[10]),
            color = df_dns_custom_unique$markerColor[10],
            time = df_dns_custom_unique$lbl[10],
            footer = df_dns_custom_unique$TYPE[10],
            df_dns_custom_unique$LOCATION_DESCRIPTION[10]
          )
        )
      )
    })
    
    
    
    #visualise individual playgrounds============================================
    #visualise individual ones
    output$pg_type_ui <- renderUI({
      tmpchoices <- c('All', sort(unique(playgrounds_complex$PLAYGROUND_TYPE)))
      
      #create listing
      tmplist <- list()
      
      for(i in LETTERS){
        for(j in tmpchoices){
          if(length(grep(paste0('^', i), tmpchoices)) != 0){
            if(length(grep(paste0('^', i), tmpchoices)) == 1){
              tmplist[[tmpchoices[grep(paste0('^', i), tmpchoices)]]] <- 
                tmpchoices[grep(paste0('^', i), tmpchoices)]
            }else(
              tmplist[[i]] <- tmpchoices[grep(paste0('^', i), tmpchoices)] 
            )
          }
        }
      }
      
      selectInput('pg_type', label = 'Type', choices = tmplist, 
                  multiple = TRUE, selected = tmplist[[1]][1])
    })
    
    
    output$pg_suburb_ui <- renderUI({
      if(is.null(input$pg_type))
        return()
      
      if(length(input$pg_type) == 1 & input$pg_type == 'All'){
        df <- playgrounds_complex
      }else{
        df <- playgrounds_complex %>%
          filter(PLAYGROUND_TYPE %in% input$pg_type)
      }
      
      tmpchoices <- c('All', sort(unique(df$DIVISION_NAME)))
      
      #create listing
      tmplist <- list()
      
      for(i in LETTERS){
        for(j in tmpchoices){
          if(length(grep(paste0('^', i), tmpchoices)) != 0){
            if(length(grep(paste0('^', i), tmpchoices)) == 1){
              tmplist[[tmpchoices[grep(paste0('^', i), tmpchoices)]]] <- 
                tmpchoices[grep(paste0('^', i), tmpchoices)]
            }else(
              tmplist[[i]] <- tmpchoices[grep(paste0('^', i), tmpchoices)] 
            )
          }
        }
      }
      
      selectInput('pg_suburb', label = 'Suburb', choices = tmplist, multiple = TRUE, 
                  selected = tmplist[[1]][1])
    })
    
    output$pg_year_ui <- renderUI({
      if(is.null(input$pg_type))
        return()
      
      if(is.null(input$pg_suburb))
        return()
      
      if(length(input$pg_type) == 1 & input$pg_type == 'All'){
        df <- playgrounds_complex
      }else{
        df <- playgrounds_complex %>%
          filter(PLAYGROUND_TYPE %in% input$pg_type)
      }
      
      if(length(input$pg_suburb) == 1 & input$pg_suburb == 'All'){
        df <- df
      }else{
        df <- df %>%
          filter(DIVISION_NAME %in% input$pg_suburb)
      }
      
      tmpchoices <- c('All', sort(unique(df$UPGRADE_YEAR)))
      
      selectInput('pg_year', label = 'Upgrade Year', choices = tmpchoices, multiple = TRUE, 
                  selected = tmpchoices[1])
    })
    
    
    output$pg_parking_ui <- renderUI({
      if(is.null(input$pg_type))
        return()
      
      if(is.null(input$pg_suburb))
        return()
      
      if(is.null(input$pg_year))
        return()
      
      if(length(input$pg_type) == 1 & input$pg_type == 'All'){
        df <- playgrounds_complex
      }else{
        df <- playgrounds_complex %>%
          filter(PLAYGROUND_TYPE %in% input$pg_type)
      }
      
      if(length(input$pg_suburb) == 1 & input$pg_suburb == 'All'){
        df <- df
      }else{
        df <- df %>%
          filter(DIVISION_NAME %in% input$pg_suburb)
      }
      
      if(length(input$pg_year) == 1 & input$pg_year == 'All'){
        df <- df
      }else{
        df <- df %>%
          filter(UPGRADE_YEAR %in% input$pg_year)
      }
      
      tmpchoices <- c('All', sort(unique(df$PARKING)))
      
      #create listing
      tmplist <- list()
      
      for(i in LETTERS){
        for(j in tmpchoices){
          if(length(grep(paste0('^', i), tmpchoices)) != 0){
            if(length(grep(paste0('^', i), tmpchoices)) == 1){
              tmplist[[tmpchoices[grep(paste0('^', i), tmpchoices)]]] <- 
                tmpchoices[grep(paste0('^', i), tmpchoices)]
            }else(
              tmplist[[i]] <- tmpchoices[grep(paste0('^', i), tmpchoices)] 
            )
          }
        }
      }
      
      selectInput('pg_parking', label = 'Parking Type', choices = tmplist, multiple = TRUE, 
                  selected = tmplist[[1]][1])
    })
    
    output$pg_shade_ui <- renderUI({
      if(is.null(input$pg_type))
        return()
      
      if(is.null(input$pg_suburb))
        return()
      
      if(is.null(input$pg_year))
        return()
      
      if(is.null(input$pg_parking))
        return()
      
      if(length(input$pg_type) == 1 & input$pg_type == 'All'){
        df <- playgrounds_complex
      }else{
        df <- playgrounds_complex %>%
          filter(PLAYGROUND_TYPE %in% input$pg_type)
      }
      
      if(length(input$pg_suburb) == 1 & input$pg_suburb == 'All'){
        df <- df
      }else{
        df <- df %>%
          filter(DIVISION_NAME %in% input$pg_suburb)
      }
      
      if(length(input$pg_year) == 1 & input$pg_year == 'All'){
        df <- df
      }else{
        df <- df %>%
          filter(UPGRADE_YEAR %in% input$pg_year)
      }
      
      if(length(input$pg_parking) == 1 & input$pg_parking == 'All'){
        df <- df
      }else{
        df <- df %>%
          filter(PARKING %in% input$pg_parking)
      }
      
      tmpchoices <- c('All', sort(unique(df$SHADE_LEVEL)))
      
      #create listing
      tmplist <- list()
      
      for(i in LETTERS){
        for(j in tmpchoices){
          if(length(grep(paste0('^', i), tmpchoices)) != 0){
            if(length(grep(paste0('^', i), tmpchoices)) == 1){
              tmplist[[tmpchoices[grep(paste0('^', i), tmpchoices)]]] <- 
                tmpchoices[grep(paste0('^', i), tmpchoices)]
            }else(
              tmplist[[i]] <- tmpchoices[grep(paste0('^', i), tmpchoices)] 
            )
          }
        }
      }
      
      selectInput('pg_shade', label = 'Shade level', choices = tmplist, multiple = TRUE, 
                  selected = tmplist[[1]][1])
    })
    
    output$pg_ind_ui <- renderUI({
      if(is.null(input$pg_type))
        return()
      
      if(is.null(input$pg_suburb))
        return()
      
      if(is.null(input$pg_year))
        return()
      
      if(is.null(input$pg_parking))
        return()
      
      if(is.null(input$pg_shade))
        return()
      
      if(length(input$pg_type) == 1 & input$pg_type == 'All'){
        df <- playgrounds_complex
      }else{
        df <- playgrounds_complex %>%
          filter(PLAYGROUND_TYPE %in% input$pg_type)
      }
      
      if(length(input$pg_suburb) == 1 & input$pg_suburb == 'All'){
        df <- df
      }else{
        df <- df %>%
          filter(DIVISION_NAME %in% input$pg_suburb)
      }
      
      if(length(input$pg_year) == 1 & input$pg_year == 'All'){
        df <- df
      }else{
        df <- df %>%
          filter(UPGRADE_YEAR %in% input$pg_year)
      }
      
      if(length(input$pg_parking) == 1 & input$pg_parking == 'All'){
        df <- df
      }else{
        df <- df %>%
          filter(PARKING %in% input$pg_parking)
      }
      
      if(length(input$pg_shade) == 1 & input$pg_shade == 'All'){
        df <- df
      }else{
        df <- df %>%
          filter(SHADE_LEVEL %in% input$pg_shade)
      }
      
      tmpchoices <- sort(unique(df$NAME))
      
      #create listing
      tmplist <- list()
      
      for(i in LETTERS){
        for(j in tmpchoices){
          if(length(grep(paste0('^', i), tmpchoices)) != 0){
            if(length(grep(paste0('^', i), tmpchoices)) == 1){
              tmplist[[tmpchoices[grep(paste0('^', i), tmpchoices)]]] <- 
                tmpchoices[grep(paste0('^', i), tmpchoices)]
            }else(
              tmplist[[i]] <- tmpchoices[grep(paste0('^', i), tmpchoices)] 
            )
          }
        }
      }
      
      selectInput('pg_ind', label = 'Play Ground', choices = tmplist, multiple = FALSE, 
                  selected = tmplist[[1]][1])
    })
    
    observeEvent(input$update, {
      updateBoxSidebar("mycardsidebar")
    })
    
    observe(print(input$mycardsidebar))
    
    observe({
      
      
      if(is.null(input$pg_type))
        return()
      
      if(is.null(input$pg_suburb))
        return()
      
      if(is.null(input$pg_year))
        return()
      
      if(is.null(input$pg_parking))
        return()
      
      if(is.null(input$pg_shade))
        return()
      
      if(is.null(input$pg_ind))
        return()
      
      if(length(input$pg_type) == 1 & input$pg_type == 'All'){
        df <- playgrounds_complex
      }else{
        df <- playgrounds_complex %>%
          filter(PLAYGROUND_TYPE %in% input$pg_type)
      }
      
      if(length(input$pg_suburb) == 1 & input$pg_suburb == 'All'){
        df <- df
      }else{
        df <- df %>%
          filter(DIVISION_NAME %in% input$pg_suburb)
      }
      
      if(length(input$pg_year) == 1 & input$pg_year == 'All'){
        df <- df
      }else{
        df <- df %>%
          filter(UPGRADE_YEAR %in% input$pg_year)
      }
      
      if(length(input$pg_parking) == 1 & input$pg_parking == 'All'){
        df <- df
      }else{
        df <- df %>%
          filter(PARKING %in% input$pg_parking)
      }
      
      if(length(input$pg_shade) == 1 & input$pg_shade == 'All'){
        df <- df
      }else{
        df <- df %>%
          filter(SHADE_LEVEL %in% input$pg_shade)
      }
      
      if(length(input$pg_shade) == 1 & input$pg_shade == 'All'){
        df <- df
      }else{
        df <- df %>%
          filter(SHADE_LEVEL %in% input$pg_shade)
      }
      
      df <- df %>%
        filter(NAME == input$pg_ind)
      
      shademap <- setNames(c("?", "None", "Poor", "Adjacent", "Adjacent",  "Fair", "Fair", "Good"), c("UNIDENTIFIED", "NONE", "POOR", "SHADE STRUCTURE ADJACENT", "SURROUNDING TREES",  "FAIR", "MODERATE", "GOOD"))
      
      df$SHADE_LEVEL <- shademap[as.character(df$SHADE_LEVEL )]
      
      df <- df %>%
        mutate(score = case_when(
          SHADE_LEVEL == "?" ~ 0+((16.66667-0)/2),
          SHADE_LEVEL == "None" ~ 16.66667+((33.33333-16.66667)/2),
          SHADE_LEVEL == "Poor" ~ 33.33333+((50.00000-33.33333)/2),
          SHADE_LEVEL == "Adjacent" ~ 50.00000+((66.66667-50.00000)/2),
          SHADE_LEVEL == "Fair" ~ 66.66667+((83.33333-66.66667)/2),
          SHADE_LEVEL == "Good" ~ 83.33333+((100-83.33333)/2),
        ))
      
      gradingDataIn <- data.frame(
        label = c('?', 'None', 'Poor', 'Adjacent', 'Fair', 'Good'),
        color = c('black', 'white', 'orange', 'red', 'blue', 'green'),
        lowScore = c(0, 16.66667, 33.33333, 50.00000, 66.66667, 83.33333),
        highScore = c(16.66667, 33.33333, 50.00000, 66.66667, 83.33333, 100)
      )
      
      output[["shadelevel"]] <- renderAmChart4({
        
        amGaugeChart(
          chartTitle = input$pg_ind,
          caption = 'Shade level',
          theme = 'dark',
          score = df$score, minScore = 0, 
          maxScore = 100, gradingData = gradingDataIn
        )
      })
      
      output$tree_shade_ui <- renderValueBox({
        valueBox(
          value = df$tree_shade,
          subtitle = "Tree Shade",
          color = "primary",
          icon = icon("tree")
        )
      })
      
      output$sail_shade_ui <- renderValueBox({
        valueBox(
          value = df$sail_shade,
          subtitle = "Sail Shade",
          color = "lightblue",
          icon = icon("bezier-curve")
        )
      })
      
      output$shelter_shade_ui <- renderValueBox({
        valueBox(
          value = df$shelter_shade,
          subtitle = "Shelter Shade",
          color = "teal",
          icon = icon("home")
        )
      })
      
      #info boxes
      output$pg_ui_location <- renderInfoBox({ 
        infoBox(
          title = "Location Description",
          value = df$LOCATION_DESCRIPTION,
          icon = icon("compass"),
          color = "danger",
          fill = FALSE,
        )
      })
      
      output$pg_ui_parking <- renderInfoBox({ 
        infoBox(
          title = "Type of Parking",
          value = df$PARKING,
          icon = icon("parking"),
          color = "orange",
          fill = FALSE,
        )
      })
      output$pg_ui_trees <- renderInfoBox({ 
        infoBox(
          title = "Trees Description",
          value = df$TREES,
          icon = icon("tree"),
          color = "warning",
          fill = FALSE,
        )
      })
      
      #FILL
      output$pg_ui_year <- renderInfoBox({ 
        infoBox(
          title = "Upgrade Year",
          value = df$UPGRADE_YEAR,
          icon = icon("calendar"),
          color = "olive",
          fill = TRUE,
        )
      })
      
      output$pg_ui_area <- renderInfoBox({ 
        infoBox(
          title = "Playground Area",
          value = df$area,
          icon = icon("chart-area"),
          color = "success",
          fill = TRUE,
        )
      })
      
      output$pg_ui_perimeter <- renderInfoBox({ 
        infoBox(
          title = "Playground Perimeter",
          value = round(df$PERIMETER),
          icon = icon("ruler-vertical"),
          color = "lime",
          fill = TRUE,
        )
      })
      
      #add PG map
      output$map_pg <- renderLeaflet({
        
        leaflet() %>%
          setView(lng=as.numeric(df$long), 
                  lat=as.numeric(df$lat), zoom = 15) %>%
          addTiles() %>%
          addControlGPS(options = gpsOptions(position = "topleft", activate = TRUE,
                                             autoCenter = TRUE, maxZoom = 60,
                                             setView = TRUE)) %>%
          addCircleMarkers(data = df, 
                           lng = ~long, lat = ~lat, 
                           radius = 8,
                           #color = ~factpal(colourcol),
                           stroke = FALSE, fillOpacity = 0.5,
                           popup = ~OBJECT,
                           group = 'Circles') %>%
          addLayersControl(
            overlayGroups = c('Polygons', "Circles"),
            options = layersControlOptions(collapsed = FALSE)
          ) %>%
          addProviderTiles(providers$CartoDB.DarkMatter)
      })
      
      
    })
    
    
    
    
    
    
    #all places description===================================
    output$select_all_ui <- renderUI({
      
      tmpchoices <- c('All', sort(unique(df_dns$OBJECT)))
      
      #create listing
      tmplist <- list()
      
      for(i in LETTERS){
        for(j in tmpchoices){
          if(length(grep(paste0('^', i), tmpchoices)) != 0){
            if(length(grep(paste0('^', i), tmpchoices)) == 1){
              tmplist[[tmpchoices[grep(paste0('^', i), tmpchoices)]]] <- 
                tmpchoices[grep(paste0('^', i), tmpchoices)]
            }else(
              tmplist[[i]] <- tmpchoices[grep(paste0('^', i), tmpchoices)] 
            )
          }
        }
      }
      
      pickerInput(
        inputId = 'select_all', 
        label = 'Select Categories', 
        choices = tmplist, 
        options = list(
          `actions-box` = TRUE, 
          size = 10
        ), 
        multiple = TRUE,
        selected = tmplist[[1]][1]
      )
      
      # selectInput('select_all', 'Select Categories', choices = tmplist,
      #             multiple = TRUE, selected = tmplist[[1]][1])
    })
    
    output$map_all <- renderLeaflet({
      show_modal_spinner(text = 'Please wait...') 
      if(is.null(input$select_all))
        return()
      
      tmpoptions <- input$select_all
      
      if(tmpoptions %in% c('', ' ')){
        tmpoptions <- 'All'
      }
      
      if(length(tmpoptions) != 1){
        tmpoptions <- tmpoptions[tmpoptions != 'All']
      }
      
      #setup data
      if(tmpoptions == 'All'){
        df_dns_sample <- df_dns
      }else{
        df_dns_sample <- df_dns %>%
          filter(OBJECT %in% tmpoptions)
      }
      
      tmplocexample <- c(lng=149.1300, lat=-35.2809)
      
      if(length(tmpoptions) == 1 & tmpoptions != 'All' ){
        df_dns_sample$colourcol <- df_dns_sample$TYPE
      }else{
        df_dns_sample$colourcol <-df_dns_sample$OBJECT
      }
      
      colourcol <- 'colourcol'
      
      factpal <- colorFactor(viridis::plasma(n = length(unique( df_dns_sample[[colourcol]]))), 
                             df_dns_sample[[colourcol]], )
      
      remove_modal_spinner()
      
      leaflet() %>%
        setView(lng=as.numeric(tmplocexample[1]), 
                lat=as.numeric(tmplocexample[2]), zoom = 17) %>%
        addTiles() %>%
        addControlGPS(options = gpsOptions(position = "topleft", activate = TRUE,
                                           autoCenter = TRUE, maxZoom = 60,
                                           setView = TRUE)) %>%
        addPolygons(data = in_geo,
                    fillColor = ~pal(fields_n),
                    label = ~paste0(DIVISION_NAME, ": ", fields_n),
                    stroke = T, weight = 0.5, opacity = 1,
                    group = 'Polygons') %>%
        addHeatmap(data = df_dns_sample,
                   lng = ~long, lat = ~lat, 
                   intensity = ~nn,
                   blur = 20, max = 0.05, radius = 15,
                   group = 'Heatmap'
        ) %>%
        addCircleMarkers(data = df_dns_sample, 
                         lng = ~long, lat = ~lat, 
                         radius = 8,
                         color = ~factpal(colourcol),
                         stroke = FALSE, fillOpacity = 0.5,
                         popup = ~OBJECT,
                         group = 'Circles') %>%
        addLayersControl(
          #baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
          overlayGroups = c('Polygons', 'Heatmap', "Circles"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>% 
        hideGroup("Circles") %>%
        addProviderTiles(providers$CartoDB.Positron)
    })
    
    output$info_ui_dv <- renderInfoBox({
      
      if(is.null(input$map_all_marker_click)){
        infoBox(
          title = "SUBURB",
          color = "info",
          value = "Click on marker",
          icon = icon("building")
        )
      }else{
        
        p <- input$map_all_marker_click
        pclick_lat <- input$map_all_marker_click$lat
        pclick_lng <- input$map_all_marker_click$lng
        
        df_dns_click <- df_dns %>% filter(lat == pclick_lat & long == pclick_lng)
        df_dns_click <- df_dns_click[1,]
        
        infoBox(
          title = "SUBURB",
          color = "info",
          value = as.character(df_dns_click$DIVISION_NAME),
          icon = icon("building")
        )
      }
      
    })
    
    output$info_ui_location <- renderInfoBox({
      
      if(is.null(input$map_all_marker_click)){
        infoBox(
          title = "LOCATION",
          color = "success",
          value = "Click on marker",
          icon = icon("street-view")
        )
      }else{
        
        p <- input$map_all_marker_click
        pclick_lat <- input$map_all_marker_click$lat
        pclick_lng <- input$map_all_marker_click$lng
        
        df_dns_click <- df_dns %>% filter(lat == pclick_lat & long == pclick_lng)
        df_dns_click <- df_dns_click[1,]
        
        infoBox(
          title = "LOCATION",
          color = "success",
          value = as.character(df_dns_click$LOCATION_DESCRIPTION),
          icon = icon("street-view")
        )
      }
      
    })
    
    output$info_ui_type <- renderInfoBox({
      
      if(is.null(input$map_all_marker_click)){
        infoBox(
          title = "DESCRIPTION",
          color = "warning",
          value = "Click on marker",
          icon = icon("bookmark")
        )
      }else{
        
        p <- input$map_all_marker_click
        pclick_lat <- input$map_all_marker_click$lat
        pclick_lng <- input$map_all_marker_click$lng
        
        df_dns_click <- df_dns %>% filter(lat == pclick_lat & long == pclick_lng)
        df_dns_click <- df_dns_click[1,]
        
        df_dns_click$TYPE <- gsub('_', ' ', df_dns_click$TYPE)
        
        infoBox(
          title = "DESCRIPTION",
          color = "warning",
          value = as.character(df_dns_click$TYPE),
          icon = icon("bookmark")
        )
      }
      
    })
    
    output$info_ui_object <-  bs4Dash::renderInfoBox({
      
      if(is.null(input$map_all_marker_click)){
        infoBox(
          title = "OBJECT",
          color = "danger",
          value = 'Click on marker',
          icon = icon("location-arrow")
        )
      }else{
        p <- input$map_all_marker_click
        pclick_lat <- input$map_all_marker_click$lat
        pclick_lng <- input$map_all_marker_click$lng
        
        df_dns_click <- df_dns %>% filter(lat == pclick_lat & long == pclick_lng)
        df_dns_click <- df_dns_click[1,]
        
        if(df_dns_click$OBJECT %in% c('Pedestrian', 'Cyclist')){
          df_dns_click$OBJECT <- paste0(df_dns_click$OBJECT, ' Accident')
        }
        
        infoBox(
          title = "OBJECT",
          color = "danger",
          value = as.character(df_dns_click$OBJECT),
          icon = icon("location-arrow")
        )
      }
      
      
      
      
    })
    
    #radius of overall maps
    
    output$bysuburb <- renderEcharts4r({
      
      if(is.null(input$select_all))
        return()
      
      tmpoptions <- input$select_all
      
      if(length(tmpoptions) != 1){
        tmpoptions <- tmpoptions[tmpoptions != 'All']
      }
      
      #setup data
      if(tmpoptions == 'All'){
        df_dns_sample <- df_dns
      }else{
        df_dns_sample <- df_dns %>%
          filter(OBJECT %in% tmpoptions)
      }
      
      as.data.frame(df_dns_sample %>%
                      group_by(DIVISION_NAME) %>%
                      count()) %>%
        top_n(10) %>%
        arrange(n) %>%
        e_charts(DIVISION_NAME) %>% 
        e_pie(n, roseType = "radius")
      
    })
    
    
    output$bytype <- renderEcharts4r({
      
      if(is.null(input$select_all))
        return()
      
      tmpoptions <- input$select_all
      
      if(length(tmpoptions) != 1){
        tmpoptions <- tmpoptions[tmpoptions != 'All']
      }
      
      #setup data
      if(tmpoptions == 'All'){
        df_dns_sample <- df_dns
      }else{
        df_dns_sample <- df_dns %>%
          filter(OBJECT %in% tmpoptions)
      }
      
      if(length(tmpoptions) == 1 & tmpoptions != 'All' ){
        as.data.frame(df_dns_sample %>%
                        group_by(TYPE) %>%
                        count()) %>%
          top_n(10) %>%
          arrange(n) %>%
          e_charts(TYPE) %>% 
          e_pie(n, roseType = "radius")
      }else{
        as.data.frame(df_dns_sample %>%
                        group_by(OBJECT) %>%
                        count()) %>%
          top_n(10) %>%
          arrange(n) %>%
          e_charts(OBJECT) %>% 
          e_pie(n, roseType = "radius")
      }
      
    })

    output$dfwellbeing_ui <- renderHighchart({
      as.data.frame(df_wellbeing) %>% 
        hchart("column", hcaes(x = Measure, y = Value, 
                               color = Measure))%>%
        hc_add_theme(hc_theme_darkunica())
    })
  }
)
