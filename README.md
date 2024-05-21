## Introduction

With the UEFA European football championship (EURO 2024) standing just
around the corner from June 14th to July 14th in Germany, many avid
football fans are curious who is going to take the title this year. The
European football championship rotates bi-annually with the FIFA World
Cup. While the regular season for club football is for approximately 10
months of the year, both international tournaments are a phenomenon on
their own, eliciting extra special emotions of pride in football fans.
The most appealing factor about the international tournaments, opposed
to club football, is that citizenship is assigned by birth. Although
there are many exceptions to this rule these days, generally, an
international team cannot buy players as in club football. This means
that a coach has to work with what they got. Additionally, many of these
players are opponents for most of their season, when suddenly the cards
are radically shuffled for about two months including preparation. These
factors make predicting a winner in these international tournaments
extra difficult. A team can have 11 world-class players, most playing
for teams that are part of the UEFA Champions League (European club
tournament for the best teams from each league), yet still get knocked
out in the group phase of the World Cup or EURO. The prime example is
the 2014 world champion, Germany, in the 2018 and 2022 world cups. While
on paper Germany should have reached the quarterfinal, at minimum, they
embarassingly did not reach the knock-out phase in neither tournament.
Therefore, predicting a winner for the EURO 2024 is additionally
difficult, but also adds an additional challenge at the same time. The
current project aims to utilize historical team-level, as well as
player-level data to train a tree-based regression model, to
subsequently predict the wins of each participating team, and simulate
the EURO 2024 tournament. The code below shows, annotated, the data
acquisition, preparation, modeling, and simulation. As a comparative
baseline model, the same simulation was conducted with the FIFA rank as
dependent variable at the very bottom.

## Data Acquisition

The data for this project was acquired using an API (see here:
<https://www.api-football.com>). Because there is a pull-limit for this
API, the code shown below for the actual scraping process is
non-functional, as the authorization key is removed. Instead, the
scraped data is imported from GitHub in form of a csv file.
Additionally, two manual data frames are imported: leagues and nations.
These include a list of leagues to be scraped from the API, such as
recent EUROs, world cups, nation leagues, and others. The nations data
frame lists the participating teams, as well as the groups and the ID on
the API. Lastly, all packages required for this project are loaded at
the very top.

    #Load packages
    library(httr)
    library(jsonlite)
    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    library(tidyr)
    library(caret)

    ## Loading required package: ggplot2

    ## Loading required package: lattice

    ## 
    ## Attaching package: 'caret'

    ## The following object is masked from 'package:httr':
    ## 
    ##     progress

    library(randomForest)

    ## randomForest 4.7-1.1

    ## Type rfNews() to see new features/changes/bug fixes.

    ## 
    ## Attaching package: 'randomForest'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     margin

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

    library(randomForestExplainer)

    ## Registered S3 method overwritten by 'GGally':
    ##   method from   
    ##   +.gg   ggplot2

    library(Metrics)

    ## 
    ## Attaching package: 'Metrics'

    ## The following objects are masked from 'package:caret':
    ## 
    ##     precision, recall

    library(gt)
    library(gtExtras)
    library(lubridate)

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

    library(shapviz)
    library(kernelshap)
    #Import nations and leagues information for API scrape
    leagues = read.csv("https://raw.githubusercontent.com/lucasweyrich958/EURO2024_Simulation/main/Leagues.csv")
    nations = read.csv("https://raw.githubusercontent.com/lucasweyrich958/EURO2024_Simulation/main/Nations.csv")
    head(leagues)

    ##              League ID Season
    ## 1         World Cup  1   2010
    ## 2         World Cup  1   2014
    ## 3         World Cup  1   2018
    ## 4         World Cup  1   2022
    ## 5 Euro Championship  4   2008
    ## 6 Euro Championship  4   2012

    head(nations)

    ##        Nation Group   ID FIFA_Rank
    ## 1     Germany     A   25        16
    ## 2    Scotland     A 1108        39
    ## 3     Hungary     A  769        26
    ## 4 Switzerland     A   15        19
    ## 5       Spain     B    9         8
    ## 6     Croatia     B    3        10

The chunk below shows the code and functions utilized to scrape the API.
Again, the code scrape code is non-functional due to the cost associated
with the API.

    scrape_data = function(league_id, season, team_id) {
      url = "https://api-football-v1.p.rapidapi.com/v3/teams/statistics" #Identify API URL

      queryString = list(
        league = league_id,
        season = season,
        team = team_id) #Create a quuery string to be used in for loop

      response = VERB("GET", url, query = queryString, add_headers('X-RapidAPI-Key' = 'xxx',
                                                                    'X-RapidAPI-Host' = 'api-football-v1.p.rapidapi.com'),
                       content_type("application/octet-stream")) #Request API pull

      output = fromJSON(rawToChar(response$content))
      output = output$response #Save out response
      data = NULL #Reset data frame

      if (!is.null(output$team) && !is.null(output$form) && !is.null(output$fixtures) &&
          !is.null(output$goals$`for`$total) && !is.null(output$goals$against$total)) {
        data = as.data.frame(output$team)
        data = cbind(data, as.data.frame(output$form))
        data = cbind(data, as.data.frame(output$league$season))
        data = cbind(data, as.data.frame(output$fixtures))
        data = cbind(data, as.data.frame(output$goals$`for`$total))
        data = cbind(data, as.data.frame(output$goals$against$total))} #Take out all the necessary data of the nested lists
      return(data)}

    team_data_scrape = data.frame()

    #For-loop to scrape all teams and all leagues and seasons from the dataframe, and save it into a new dataframe
    for (i in 1:nrow(leagues)) {
      league_id = leagues$ID[i]
      season = leagues$Season[i]

      for (j in 1:nrow(nations)) {
        team_id = nations$ID[j]
        team_data = scrape_data(league_id, season, team_id)
        all_data = rbind(all_data, team_data)}}

    team_data_scrape = read.csv("https://raw.githubusercontent.com/lucasweyrich958/EURO2024_Simulation/main/Team_scrape.csv") 
    head(team_data_scrape)

    ##   X   id        name                                                logo
    ## 1 1   25     Germany   https://media.api-sports.io/football/teams/25.png
    ## 2 2   15 Switzerland   https://media.api-sports.io/football/teams/15.png
    ## 3 3    9       Spain    https://media.api-sports.io/football/teams/9.png
    ## 4 4  768       Italy  https://media.api-sports.io/football/teams/768.png
    ## 5 5 1091    Slovenia https://media.api-sports.io/football/teams/1091.png
    ## 6 6   21     Denmark   https://media.api-sports.io/football/teams/21.png
    ##   output.form output.league.season played.home played.away played.total
    ## 1     WLWWWLW                 2010           4           3            7
    ## 2         WLD                 2010           1           2            3
    ## 3     LWWWWWW                 2010           3           4            7
    ## 4         DDL                 2010           2           1            3
    ## 5         WDL                 2010           2           1            3
    ## 6         LWL                 2010           1           2            3
    ##   wins.home wins.away wins.total draws.home draws.away draws.total loses.home
    ## 1         2         3          5          0          0           0          2
    ## 2         0         1          1          1          0           1          0
    ## 3         2         4          6          0          0           0          1
    ## 4         0         0          0          2          0           2          0
    ## 5         0         1          1          1          0           1          1
    ## 6         0         1          1          0          0           0          1
    ##   loses.away loses.total home away total home.1 away.1 total.1
    ## 1          0           2    8    8    16      3      2       5
    ## 2          1           1    0    1     1      0      1       1
    ## 3          0           1    3    5     8      1      1       2
    ## 4          1           1    2    2     4      2      3       5
    ## 5          0           1    2    1     3      3      0       3
    ## 6          1           2    1    2     3      3      3       6

The function above takes in the variables league\_id, season, and
team\_id, and subsequently requests to the API the team data from that
specific tournament in that season. The function then selects the
relevant columns from the response, such as wins, losses, goals scored,
goals conceded, etc. The for-loop loops though the leagues and nations
data frames to request the API for every team and every tournament and
season. It is important to note that not every team will receive a
response, because some teams may not have qualified for a specific
tournament in some year. This data frame was then used to retrieve the
roster for the player IDs for the respective team, season, and league
from the API. Once the player IDs were retrieved, another function
retrieved the player-level statistics for the whole season (e.g., club
included). Like above, the authorization key is masked.

    scrape_player_ids = function(league_id, season) {
        url <- "https://api-football-v1.p.rapidapi.com/v3/players?league=" # Identify API URL
        
        league = league_id
        season = season
        page = 1
        
        all_data = data.frame()
        repeat {
          query_string = paste(url, league, '&season=', season, '&page=', page, sep = '')
          
          response = VERB("GET", query_string, add_headers('X-RapidAPI-Key' = 'xxxx',
                                                            'X-RapidAPI-Host' = 'api-football-v1.p.rapidapi.com'),
                           content_type("application/octet-stream")) # Request API pull
          Sys.sleep(3)
          output = fromJSON(rawToChar(response$content))
          total_pages = output$paging$total
          current_page = output$paging$current
          output = output$response # Save response
          output = unnest(output, cols = statistics)
          
          
          data = as.data.frame(output$player$id)
          data = cbind(data, as.data.frame(output$team$id))
          data = cbind(data, as.data.frame(output$league$season))
          all_data = rbind(all_data, data)
          if (current_page >= total_pages) {
            break # Exit loop if current page is greater than or equal to total pages
          }
          
          page = page + 1 # Increment page number for next iteration
        }
        
        return(all_data)
      }

    player_id_data = data.frame()

    for (i in 1:nrow(leagues)) {
      league_id = leagues$ID[i]
      season = leagues$Season[i]
      team_data = scrape_player_ids(league_id, season)
      player_id_data = rbind(player_id_data, team_data)} #Pull players for each tournament and season necessary

    colnames(player_id_data) = c('Player_ID', 'Team_ID', 'Season')
    team_ids = as.data.frame(nations$ID)

    player_ids_EUR = merge(x = player_id_data, y = nations, by.x = 'Team_ID', by.y = 'ID') #Join player ID data with nations ID, to only retain relevant player IDs

    all_data = data.frame()

    scrape_player_stats = function(player_id, season) {
        url = "https://api-football-v1.p.rapidapi.com/v3/players?id=" # Identify API URL
        
        player = player_id
        season = season

        query_string = paste(url, player, '&season=', season, sep = '')
        
        response = VERB("GET", query_string, add_headers('X-RapidAPI-Key' = 'xxxx',
                                                          'X-RapidAPI-Host' = 'api-football-v1.p.rapidapi.com'),
                         content_type("application/octet-stream"))

        output = fromJSON(rawToChar(response$content))
        output = output$response #Save out response
        output = unnest(output, cols = statistics) #Unnest the statistics lists
        data = NULL #Reset data frame
        
        data = output %>%
          summarize(id = first(output$player$id),
                    name = first(output$player$name),
                    birthdate = first(output$player$birth$date),
                    height = first(output$player$height),
                    weight = first(output$player$weight),
                    season = first(output$league$season),
                    rating = mean(as.numeric(output$games$rating), na.rm = T),
                    minutes = sum(output$games$minutes, na.rm = T),
                    total_shots = sum(output$shots$total, na.rm = T),
                    target_shots = sum(output$shots$on, na.rm = T),
                    player_goals = sum(output$goals$total, na.rm = T),
                    total_passes = sum(output$passes$total, na.rm = T),
                    key_passes = sum(output$passes$key, na.rm = T),
                    accuracy_passes = sum(output$passes$accuracy, na.rm = T),
                    tackles = sum(output$tackles$total, na.rm = T),
                    total_duels = sum(output$duels$total, na.rm = T),
                    won_duels = sum(output$duels$won, na.rm = T),
                    total_dribbles = sum(output$dribbles$attempts, na.rm = T),
                    won_dribbles = sum(output$dribbles$success, na.rm = T),
                    fouls_drawn = sum(output$fouls$drawn, na.rm = T),
                    fouls_comitted = sum(output$fouls$committed, na.rm = T),
                    yellow = sum(output$cards$yellow, na.rm = T),
                    red = sum(output$cards$red, na.rm = T),
                    yellowred = sum(output$cards$yellowred, na.rm = T))
        all_data <<- rbind(all_data, data)
        
        return(all_data)}


    for (i in 1:nrow(player_ids_EUR)) {
      player_id = player_ids_EUR$Player_ID[i]
      season = player_ids_EUR$Season[i]
      player_data = scrape_player_stats(player_id, season)}

    player_data_scrape = read.csv('https://raw.githubusercontent.com/lucasweyrich958/EURO2024_Simulation/main/player_stats_data.csv')
    player_id_scrape = read.csv('https://raw.githubusercontent.com/lucasweyrich958/EURO2024_Simulation/main/player_id_data.csv')
    head(player_id_scrape)

    ##   X Player_ID Team_ID Season
    ## 1 1     27736    4672   2010
    ## 2 2     35806      16   2010
    ## 3 3    100768    1504   2010
    ## 4 4    104275    1561   2010
    ## 5 5    104297    1561   2010
    ## 6 6    104375    1561   2010

    head(player_data_scrape)

    ##   X     id                              name  birthdate height weight season
    ## 1 1  27736     Ricardo Gabriel Canales Lanza 1982-05-30 181 cm  78 kg   2010
    ## 2 2  35806 Francisco Javier Rodríguez Pinedo 1981-10-20 191 cm  80 kg   2010
    ## 3 3 100768                   Dominic Adiyiah 1989-11-29 172 cm  70 kg   2010
    ## 4 4 104275                      Nam-Chol Pak 1988-10-03 183 cm  78 kg   2010
    ## 5 5 104297                         Jun-Il Ri 1987-08-24 178 cm  66 kg   2010
    ## 6 6 104375                      Chol-Hyok An 1987-06-27 178 cm  72 kg   2010
    ##   rating minutes total_shots target_shots player_goals total_passes key_passes
    ## 1     NA     312           0            0            0            0          0
    ## 2     NA    3136           0            0            2            0          0
    ## 3     NA    1079           0            0            2            0          0
    ## 4     NA     472           0            0            0            0          0
    ## 5     NA    2483           0            0            0            0          0
    ## 6     NA     504           0            0            1            0          0
    ##   accuracy_passes tackles total_duels won_duels total_dribbles won_dribbles
    ## 1               0       0           0         0              0            0
    ## 2               0       0           0         0              0            0
    ## 3               0       0           0         0              0            0
    ## 4               0       0           0         0              0            0
    ## 5               0       0           0         0              0            0
    ## 6               0       0           0         0              0            0
    ##   fouls_drawn fouls_comitted yellow red yellowred
    ## 1           0              0      0   0         0
    ## 2           0              0      9   1         1
    ## 3           0              0      3   0         0
    ## 4           0              0      0   0         0
    ## 5           0              0      0   0         0
    ## 6           0              0      2   0         0

The code above utilizes the same logic as the scrape function for the
team-level data. It uses the team IDs to scrape the roster, then
utilizes that roster data to scrape the appropriate player-level data.
Looking at the first rows of the player data, these are much more
nuanced data-points, that will suite for evaluating a team’s performance
based on the player’s performance throughout that whole season, not just
for the specific national team games.

## Data Preparation

In the following code chunk, the data is prepared and cleaned. The
player-level data is aggregated and then joined with the team-level data
in order to build a data frame that includes each team for each season.

    player_id_scrape = player_id_scrape %>%
      distinct(Player_ID, Season, .keep_all = T)

    #Join player data with the player IDs to later group by team IDs
    player_data_team = merge(player_data_scrape, 
                             player_id_scrape, by.x  = c('id', 'season'), by.y = c('Player_ID', 'Season'))
    player_data_team = subset(player_data_team, select = -c(X.x, X.y)) #Remove unnecessary columns
    player_data_team$season = as.Date.character(player_data_team$season, format = '%Y') 
    player_data_team$birthdate = as.Date.character(player_data_team$birthdate, format = '%Y') #Convert columns to date

    #Group by team ID and calculate age in years by subtracting the season year from the birthday, dividing by 52.25 weeks.
    player_data_team_agg = player_data_team %>%
      group_by(Team_ID) %>%
      mutate(age = as.numeric(difftime(season, birthdate, unit="weeks"))/52.25)  

    player_data_team_agg$season = substr(player_data_team_agg$season, 1, 4) #Remove dd and mm from season columns that snuck in there
    #Engineer some additional features, such as duel efficiency as a ration between won duels and total duels
    player_data_team_agg = player_data_team_agg %>%
      mutate(height = as.numeric(gsub("[^0-9.]", "", height))) %>%
      mutate(weight = as.numeric(gsub("[^0-9.]", "", weight))) %>%
      mutate(duel_efficiency = as.numeric(won_duels) / as.numeric(total_duels) * 100) %>%
      mutate(dribble_efficicency = as.numeric(won_dribbles) / as.numeric(total_dribbles) * 100)

    #Aggregate data frame by team ID and season
    player_data_team_agg = player_data_team_agg %>%
      group_by(Team_ID, season) %>%
      summarise(as.numeric(mean(height, na.rm = T)),
                as.numeric(mean(weight, na.rm = T)),
                as.numeric(mean(rating, na.rm = T)),
                as.numeric(mean(minutes, na.rm = T)),
                as.numeric(mean(total_shots, na.rm = T)),
                as.numeric(mean(player_goals, na.rm = T)),
                as.numeric(mean(total_passes, na.rm = T)),
                as.numeric(mean(key_passes, na.rm = T)),
                as.numeric(mean(accuracy_passes, na.rm = T)),
                as.numeric(mean(tackles, na.rm = T)),
                as.numeric(mean(dribble_efficicency, na.rm = T)),
                as.numeric(mean(duel_efficiency, na.rm = T)),
                as.numeric(mean(fouls_drawn, na.rm = T)),
                as.numeric(mean(fouls_comitted, na.rm = T)),
                as.numeric(mean(yellow, na.rm = T)),
                as.numeric(mean(red, na.rm = T)),
                as.numeric(mean(yellowred, na.rm = T)),
                as.numeric(mean(age, na.rm = T)))

    ## `summarise()` has grouped output by 'Team_ID'. You can override using the
    ## `.groups` argument.

    #Change column names
    colnames(player_data_team_agg) = c('Team_ID', 'Season', 'Height', 'Weight', 'Rating', 'Minutes_Played',
                                       'Total_Shots', 'Player_Goals', 'Total_Passes', 'Key_Passes', 'Accuracy_Passes',
                                       'Tackles', 'Dribble_Efficiency', 'Duel_Efficiency', 'Fouls_Drawn', 'Fouls_Comitted', 'Yellows',
                                       'Reds', 'Yellowreds', 'Age')

    #Create hitsory data frame by joining player-level and team-level data
    hist = merge(team_data_scrape, nations, by.x = "name", by.y = "Nation", all = TRUE) #Merge Nation df with Scrape df
    hist = subset(hist, select = -c(X,played.home, played.total, wins.home, 
                                    wins.away, played.away, logo, output.form, ID, draws.total, 
                                    loses.total, home, away, home.1, away.1)) #Remove unnecessary
    colnames(hist) = c("Nation",'ID','Season', "Wins_Total", "Draws_Home", "Draws_Away", "Loses_Home",
                           "Loses_Away", "Goals_Scored", "Goals_Conceded", 
                           "Group", "FIFA_Rank") #Change column names
    hist = merge(hist, player_data_team_agg, by.x = c('ID', 'Season'), by.y = c('Team_ID', 'Season'))

Above, the history dataframe is created, which is the final dataframe to
be included into the machine learning model. It includes team-level and
aggregated player-level data per team, season, and tournament. The
machine learning model utilized in this project is a random forest
regression model, due to its efficiency and transparency. The target
variable to predict is total wins. The data was split into a 80%
training set and 20% test set. The main evaluation metric is the mean
absolute error as well as R2. The target variable is to predict total
wins for each season and tournament by the random forest.

    model_df = subset(hist, select = -c(Group, ID, Group, Nation, FIFA_Rank, Season)) #Create model df by subtracting unneeded columns
    model_df[is.na(model_df)] = 0 #Replace NaNs with 0, because in this case it is bad if a team has NaN
    set.seed(2024) #Seed for reproducibility

    train_ind = createDataPartition(model_df$Wins_Total, p = 0.8, list = FALSE) #Create an index for split
    train = model_df[train_ind, ] #Create training set by applying row index
    test = model_df[-train_ind, ] #Create test set by subtracting row index

    model = randomForest(Wins_Total ~ ., 
                          data = train, ntree = 180, mtry = 20) #Fit a random forest model with 150 trees

    print(model)

    ## 
    ## Call:
    ##  randomForest(formula = Wins_Total ~ ., data = train, ntree = 180,      mtry = 20) 
    ##                Type of random forest: regression
    ##                      Number of trees: 180
    ## No. of variables tried at each split: 20
    ## 
    ##           Mean of squared residuals: 1.102707
    ##                     % Var explained: 83.05

    plot(model) #Plot model performance

![](EURO-2024-Simulation_files/figure-markdown_strict/unnamed-chunk-7-1.png)

    predictions = predict(model, newdata = test) #Run predictions

    mean_actual = mean(test$Wins_Total) 
    total_variance = sum((test$Wins_Total - mean_actual)^2)
    residual_variance = sum((test$Wins_Total - predictions)^2)
    r_squared = 1 - (residual_variance / total_variance)
    summary(test$Wins_Total) #Show summary of total wins of test set

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   0.000   1.000   2.000   2.763   4.000  10.000

    mae = mae(test$Wins_Total, predictions) #Show MAE
    mse = mse(test$Wins_Total, predictions) #Show MSE
    r_squared #Calculate and show R2

    ## [1] 0.8335978

    print(paste('MAE = ', mae))

    ## [1] "MAE =  0.757875"

    print(paste('MSE = ', mse))

    ## [1] "MSE =  0.93702628600823"

    print(paste('R2 = ', r_squared))

    ## [1] "R2 =  0.833597818184393"

    model_test <- data.frame(Actual = test$Wins_Total, Predicted = predictions)
    ggplot(model_test, aes(x = Actual, y = Predicted)) +
      geom_point(color = "#505D68") +
      geom_smooth(method = "lm", se = FALSE, color = "#4793AF") +
      labs(x = "Actual Wins", y = "Predicted Wins",
           title = "Actual vs Predicted Wins") +
      theme_minimal() + #Plot predictions vs actual
      geom_text(x = 1.3, y = 9.5, label = "MAE = 0.758")

    ## `geom_smooth()` using formula = 'y ~ x'

![](EURO-2024-Simulation_files/figure-markdown_strict/unnamed-chunk-7-2.png)

After a short trial-and-error hyperparameter tuning, the random forest
was set to 180 trees. The final mean absolute error for the test set was
0.758, which is a good error given a range of 0 to 10 wins across team,
season and tournament. Additionally, the R2 of 83% points to good
predictive power of the random forest model. The scatterplot shows
actual vs. predicted values, which also points to good predictive power.
One of the advantages of tree-based regression is the possibility to
compute SHAP-values (Shapley Additive Explanatory Values). Initially a
concept of game theory, it increases the interpretability of tree-based
models. Below, a SHAP-summary plot is plotted.

    shap_values = kernelshap(model, test[,-1], bg_X = test)

    ## Kernel SHAP values by the hybrid strategy of degree 1

    ##   |                                                                              |                                                                      |   0%  |                                                                              |=                                                                     |   1%  |                                                                              |==                                                                    |   2%  |                                                                              |===                                                                   |   4%  |                                                                              |====                                                                  |   5%  |                                                                              |====                                                                  |   6%  |                                                                              |=====                                                                 |   8%  |                                                                              |======                                                                |   9%  |                                                                              |=======                                                               |  10%  |                                                                              |========                                                              |  11%  |                                                                              |=========                                                             |  12%  |                                                                              |==========                                                            |  14%  |                                                                              |==========                                                            |  15%  |                                                                              |===========                                                           |  16%  |                                                                              |============                                                          |  18%  |                                                                              |=============                                                         |  19%  |                                                                              |==============                                                        |  20%  |                                                                              |===============                                                       |  21%  |                                                                              |================                                                      |  22%  |                                                                              |=================                                                     |  24%  |                                                                              |==================                                                    |  25%  |                                                                              |==================                                                    |  26%  |                                                                              |===================                                                   |  28%  |                                                                              |====================                                                  |  29%  |                                                                              |=====================                                                 |  30%  |                                                                              |======================                                                |  31%  |                                                                              |=======================                                               |  32%  |                                                                              |========================                                              |  34%  |                                                                              |========================                                              |  35%  |                                                                              |=========================                                             |  36%  |                                                                              |==========================                                            |  38%  |                                                                              |===========================                                           |  39%  |                                                                              |============================                                          |  40%  |                                                                              |=============================                                         |  41%  |                                                                              |==============================                                        |  42%  |                                                                              |===============================                                       |  44%  |                                                                              |================================                                      |  45%  |                                                                              |================================                                      |  46%  |                                                                              |=================================                                     |  48%  |                                                                              |==================================                                    |  49%  |                                                                              |===================================                                   |  50%  |                                                                              |====================================                                  |  51%  |                                                                              |=====================================                                 |  52%  |                                                                              |======================================                                |  54%  |                                                                              |======================================                                |  55%  |                                                                              |=======================================                               |  56%  |                                                                              |========================================                              |  58%  |                                                                              |=========================================                             |  59%  |                                                                              |==========================================                            |  60%  |                                                                              |===========================================                           |  61%  |                                                                              |============================================                          |  62%  |                                                                              |=============================================                         |  64%  |                                                                              |==============================================                        |  65%  |                                                                              |==============================================                        |  66%  |                                                                              |===============================================                       |  68%  |                                                                              |================================================                      |  69%  |                                                                              |=================================================                     |  70%  |                                                                              |==================================================                    |  71%  |                                                                              |===================================================                   |  72%  |                                                                              |====================================================                  |  74%  |                                                                              |====================================================                  |  75%  |                                                                              |=====================================================                 |  76%  |                                                                              |======================================================                |  78%  |                                                                              |=======================================================               |  79%  |                                                                              |========================================================              |  80%  |                                                                              |=========================================================             |  81%  |                                                                              |==========================================================            |  82%  |                                                                              |===========================================================           |  84%  |                                                                              |============================================================          |  85%  |                                                                              |============================================================          |  86%  |                                                                              |=============================================================         |  88%  |                                                                              |==============================================================        |  89%  |                                                                              |===============================================================       |  90%  |                                                                              |================================================================      |  91%  |                                                                              |=================================================================     |  92%  |                                                                              |==================================================================    |  94%  |                                                                              |==================================================================    |  95%  |                                                                              |===================================================================   |  96%  |                                                                              |====================================================================  |  98%  |                                                                              |===================================================================== |  99%  |                                                                              |======================================================================| 100%

    sv <- shapviz(shap_values)
    sv_importance(sv, kind = "bee", max_display = 20)

![](EURO-2024-Simulation_files/figure-markdown_strict/unnamed-chunk-8-1.png)

The SHAP summary plot is an intuitive plot showing the importance of
each feature, and the directionality it impacts the model. As can be
seen, unexpectedly, scored goals is by far the most important feature
for predicting the total wins. The coloring of each dot represents the
magnitude, meaning that for scored goals, the more yellow (e.g.,
higher), the more wins are predicted. Other important variables include
the average dribble efficiency of players during the whole season (e.g.,
how often does a player lose a ball), as well as the average minutes
played by players during the whole season. All these variables are
intuitive, as they simply point to better players. Interestingly,
however, the rating variable appears to be negatively associated with
total wins, which points to the importance of team chemistry. It may not
simply be sufficient to have many good players. While the SHAP summary
plot provides good interpretability of the model, this project is about
simulating the EURO 2024. Hence, next a new data frame is computed by
taking the aggregate of the 2023 and 2024 season data, which serves as
baseline performance for the EURO 2024.

    recent_performance = hist %>%
      filter(Season %in% c(2023, 2024)) #Filter for 2023 and 2024 to retrieve recent performance

    recent_performance = recent_performance %>%
      group_by(Nation) %>%
      summarise(sum(Wins_Total), sum(Draws_Home), sum(Draws_Away), sum(Loses_Home), sum(Loses_Away), 
                sum(Goals_Scored), sum(Goals_Conceded), 
                mean(Height), mean(Weight), mean(Rating), 
                mean(Minutes_Played), mean(Total_Shots), mean(Player_Goals), mean(Total_Passes), mean(Key_Passes),
                mean(Accuracy_Passes), mean(Tackles), mean(Dribble_Efficiency), mean(Duel_Efficiency), 
                mean(Fouls_Drawn),mean(Fouls_Comitted), mean(Yellows), mean(Reds), 
                mean(Yellowreds), mean(Age)) #Calculate sum for last two seasons to get approximate performance for simulation

    colnames(recent_performance) = c("Nation", "Wins_Total", "Draws_Home", "Draws_Away", "Loses_Home",
                                     "Loses_Away", "Goals_Scored", "Goals_Conceded", 'Height', 
                                     'Weight', 'Rating', 'Minutes_Played', 'Total_Shots', 
                                     'Player_Goals', 'Total_Passes', 'Key_Passes', 'Accuracy_Passes',
                                     'Tackles', 'Dribble_Efficiency', 'Duel_Efficiency', 
                                     'Fouls_Drawn', 'Fouls_Comitted', 'Yellows', 'Reds', 
                                     'Yellowreds', 'Age') #Change column names
    recent_performance[is.na(recent_performance)] = 0

    recent_performance_summary = recent_performance %>%
      rowwise() %>%
      mutate(total_games = sum(Wins_Total, Draws_Home, Draws_Away, Loses_Home, Loses_Away)) #Aggregate the data frame to plot some distributions

    ggplot(data = recent_performance_summary, aes(total_games)) +
      geom_histogram(bins = 8, fill = '#4793AF', binwidth = 0.5) +
      labs(x = "Total Games in 2023/2024", y = "Count", 
           title = "Distribution of Recent Games") +
      theme_minimal()

![](EURO-2024-Simulation_files/figure-markdown_strict/unnamed-chunk-9-1.png)

The data is filtered for 2023 and 2024 and subsequently summed to gain a
good baseline level of performance for each participating team. The
histogram assesses the distribution of games played (sum of all wins,
draws and losses), in order to assure the balance between all teams. As
can be seen, it seems that all teams played between 10 and 13 games, so
no team played a lot more or less games than the rest. With that, the
tournament can be simulated

## Simulation

Two additional data frames are imported, which simply present the
tournament game plan for the group phase and knock-out stage,
respectively.

### Group Phase

    group = read.csv('https://raw.githubusercontent.com/lucasweyrich958/EURO2024_Simulation/main/EURO2024_Group_State.csv')
    ko = read.csv('https://raw.githubusercontent.com/lucasweyrich958/EURO2024_Simulation/main/EURO2024_KO.csv')

    # Initialize a dataframe to track match outcomes
    tournament_results = data.frame(Round = integer(), Home = character(), Away = character(), 
                                    Winner = character(), stringsAsFactors = FALSE)

    # Initialize a dataframe to track points for each nation
    nation_points = data.frame(Nation = nations$Nation, Points = 0, Group = nations$Group)

    # Function to update performance metrics after each match
    update_performance = function(match_result, recent_performance) {
      home_team = match_result$Home
      away_team = match_result$Away
      
      home_index = which(recent_performance$Nation == home_team)
      away_index = which(recent_performance$Nation == away_team)

      #Update wins
      if (!is.na(match_result$Winner)) {
        if (match_result$Winner == home_team) {
          recent_performance$Wins_Total[home_index] = recent_performance$Wins_Total[home_index] + 1
        } else {
          recent_performance$Wins_Total[away_index] = recent_performance$Wins_Total[away_index] + 1
        }
      }
      
      # Update losses
      if (is.na(match_result$Winner)) {
        recent_performance$Loses_Home[home_index] = recent_performance$Loses_Home[home_index] + 1
        recent_performance$Loses_Away[away_index] = recent_performance$Loses_Away[away_index] + 1
      } else if (match_result$Winner == home_team) {
        recent_performance$Loses_Away[away_index] = recent_performance$Loses_Away[away_index] + 1
      } else if (match_result$Winner == away_team) {
        recent_performance$Loses_Home[home_index] = recent_performance$Loses_Home[home_index] + 1
      }
      
      # Update draws
      if (is.na(match_result$Winner)) {
        recent_performance$Draws_Home[home_index] = recent_performance$Draws_Home[home_index] + 1
        recent_performance$Draws_Away[away_index] = recent_performance$Draws_Away[away_index] + 1
      }
     
      return(recent_performance)
    }

    # Function to update points after each match
    update_points = function(match_result, nation_points) {
      winner = match_result$Winner
      if (!is.na(winner)) {
        # Winner gets 3 points
        nation_points[nation_points$Nation == winner, "Points"] = 
          nation_points[nation_points$Nation == winner, "Points"] + 3
      } else {
        # For a tie, both teams get 1 point
        home_team <- match_result$Home
        away_team <- match_result$Away
        nation_points[nation_points$Nation == home_team, "Points"] = 
          nation_points[nation_points$Nation == home_team, "Points"] + 1
        
        nation_points[nation_points$Nation == away_team, "Points"] = 
          nation_points[nation_points$Nation == away_team, "Points"] + 1
      }
      return(nation_points)
    }

    # Function to simulate a match
    simulate_match = function(match, recent_performance, model) {
      home_team = match$Home
      away_team = match$Away
      
      # Extract recent performance metrics for home and away teams
      home_performance = recent_performance[recent_performance$Nation == home_team, ]
      away_performance = recent_performance[recent_performance$Nation == away_team, ]
      
      # Extract features for the match
      match_features = rbind(home_performance, away_performance)
      
      # Use random forest model to predict the outcome
      predicted_winner = ifelse(predict(model, newdata = match_features)[1] > 
                                  predict(model, newdata = match_features)[2], home_team, away_team)
      
      # Update tournament results dataframe
      match_result = data.frame(Group = match$Group, Home = match$Home, Away = match$Away, 
                                Winner = predicted_winner)
      tournament_results <<- rbind(tournament_results, match_result)
      
      # Update performance metrics
      recent_performance = update_performance(match_result, recent_performance)
    }

    # Simulate group stage matches
    for (i in 1:nrow(group)) {
      simulate_match(group[i, ], recent_performance, model)
    }

    # Update points after each match
    for (i in 1:nrow(tournament_results)) {
      nation_points = update_points(tournament_results[i, ], nation_points)
    }

    #Initialize a data frame to rank the group phase
    ranked_nations = data.frame()
    for (group in unique(nation_points$Group)) {
      group_nations = subset(nation_points, Group == group)
      group_nations = group_nations[order(-group_nations$Points), ]
      group_nations$Rank = 1:nrow(group_nations)
      ranked_nations = rbind(ranked_nations, group_nations)}

After importing the tournament structure and plan, several functions are
created to simulate the tournament. At first, a function to simulate a
match using the random forest model as predictor. Additionally, a
function is created that will update a new data frame with the group
phase results per game, giving a winner 3 points, the loser 0 points, or
1 point to each team for a draw. Lastly, using the results data frame, a
new data frame is created that ranks the teams by group and points.
Below is a table listing the group phase results.

    ranked_nations_logo = left_join(ranked_nations, team_data_scrape %>% distinct(name, .keep_all = TRUE), 
                                     by = c("Nation" = "name")) %>%
      select(Nation, Points, Group, Rank, logo) #Merge the team_scrape data frame for the logos

    #Create a table to show the group phase results
    group_table = gt(ranked_nations_logo) %>%
      tab_row_group(
        label = "Group F",
        rows = Group == "F") %>% 
      tab_row_group(
        label = "Group E",
        rows = Group == "E") %>% 
      tab_row_group(
        label = "Group D",
        rows = Group == "D") %>%
      tab_row_group(
        label = "Group C",
        rows = Group == "C") %>%
      tab_row_group(
        label = "Group B",
        rows = Group == "B") %>%
      tab_row_group(
        label = "Group A",
        rows = Group == "A") %>%
      cols_hide(c(Group, Rank)) %>%
      gt_img_rows(columns = logo, img_source = "web", height = 15)
    group_table = tab_style(group_table, cell_fill('#bac2ca'), 
                            cells_column_labels())
    group_table = tab_style(group_table, cell_fill('#E8EBED'), 
                            cells_row_groups())
    group_table = tab_header(group_table, "EURO 2024 Group Phase Standings")
    group_table

<div id="eqnunfnypy" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#eqnunfnypy table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#eqnunfnypy thead, #eqnunfnypy tbody, #eqnunfnypy tfoot, #eqnunfnypy tr, #eqnunfnypy td, #eqnunfnypy th {
  border-style: none;
}

#eqnunfnypy p {
  margin: 0;
  padding: 0;
}

#eqnunfnypy .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#eqnunfnypy .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#eqnunfnypy .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#eqnunfnypy .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#eqnunfnypy .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#eqnunfnypy .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#eqnunfnypy .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#eqnunfnypy .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#eqnunfnypy .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#eqnunfnypy .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#eqnunfnypy .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#eqnunfnypy .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#eqnunfnypy .gt_spanner_row {
  border-bottom-style: hidden;
}

#eqnunfnypy .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#eqnunfnypy .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#eqnunfnypy .gt_from_md > :first-child {
  margin-top: 0;
}

#eqnunfnypy .gt_from_md > :last-child {
  margin-bottom: 0;
}

#eqnunfnypy .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#eqnunfnypy .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#eqnunfnypy .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#eqnunfnypy .gt_row_group_first td {
  border-top-width: 2px;
}

#eqnunfnypy .gt_row_group_first th {
  border-top-width: 2px;
}

#eqnunfnypy .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#eqnunfnypy .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#eqnunfnypy .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#eqnunfnypy .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#eqnunfnypy .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#eqnunfnypy .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#eqnunfnypy .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#eqnunfnypy .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#eqnunfnypy .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#eqnunfnypy .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#eqnunfnypy .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#eqnunfnypy .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#eqnunfnypy .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#eqnunfnypy .gt_left {
  text-align: left;
}

#eqnunfnypy .gt_center {
  text-align: center;
}

#eqnunfnypy .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#eqnunfnypy .gt_font_normal {
  font-weight: normal;
}

#eqnunfnypy .gt_font_bold {
  font-weight: bold;
}

#eqnunfnypy .gt_font_italic {
  font-style: italic;
}

#eqnunfnypy .gt_super {
  font-size: 65%;
}

#eqnunfnypy .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#eqnunfnypy .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#eqnunfnypy .gt_indent_1 {
  text-indent: 5px;
}

#eqnunfnypy .gt_indent_2 {
  text-indent: 10px;
}

#eqnunfnypy .gt_indent_3 {
  text-indent: 15px;
}

#eqnunfnypy .gt_indent_4 {
  text-indent: 20px;
}

#eqnunfnypy .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="3" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>EURO 2024 Group Phase Standings</td>
    </tr>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" style="background-color: #BAC2CA;" scope="col" id="Nation">Nation</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="background-color: #BAC2CA;" scope="col" id="Points">Points</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" style="background-color: #BAC2CA;" scope="col" id="logo">logo</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr class="gt_group_heading_row">
      <th colspan="3" class="gt_group_heading" style="background-color: #E8EBED;" scope="colgroup" id="Group A">Group A</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Group A  Nation" class="gt_row gt_left">Germany</td>
<td headers="Group A  Points" class="gt_row gt_right">9</td>
<td headers="Group A  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/25.png" style="height:15px;"></td></tr>
    <tr><td headers="Group A  Nation" class="gt_row gt_left">Hungary</td>
<td headers="Group A  Points" class="gt_row gt_right">6</td>
<td headers="Group A  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/769.png" style="height:15px;"></td></tr>
    <tr><td headers="Group A  Nation" class="gt_row gt_left">Scotland</td>
<td headers="Group A  Points" class="gt_row gt_right">3</td>
<td headers="Group A  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/1108.png" style="height:15px;"></td></tr>
    <tr><td headers="Group A  Nation" class="gt_row gt_left">Switzerland</td>
<td headers="Group A  Points" class="gt_row gt_right">0</td>
<td headers="Group A  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/15.png" style="height:15px;"></td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="3" class="gt_group_heading" style="background-color: #E8EBED;" scope="colgroup" id="Group B">Group B</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Group B  Nation" class="gt_row gt_left">Spain</td>
<td headers="Group B  Points" class="gt_row gt_right">9</td>
<td headers="Group B  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/9.png" style="height:15px;"></td></tr>
    <tr><td headers="Group B  Nation" class="gt_row gt_left">Italy</td>
<td headers="Group B  Points" class="gt_row gt_right">6</td>
<td headers="Group B  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/768.png" style="height:15px;"></td></tr>
    <tr><td headers="Group B  Nation" class="gt_row gt_left">Croatia</td>
<td headers="Group B  Points" class="gt_row gt_right">3</td>
<td headers="Group B  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/3.png" style="height:15px;"></td></tr>
    <tr><td headers="Group B  Nation" class="gt_row gt_left">Albania</td>
<td headers="Group B  Points" class="gt_row gt_right">0</td>
<td headers="Group B  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/778.png" style="height:15px;"></td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="3" class="gt_group_heading" style="background-color: #E8EBED;" scope="colgroup" id="Group C">Group C</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Group C  Nation" class="gt_row gt_left">Slovenia</td>
<td headers="Group C  Points" class="gt_row gt_right">9</td>
<td headers="Group C  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/1091.png" style="height:15px;"></td></tr>
    <tr><td headers="Group C  Nation" class="gt_row gt_left">England</td>
<td headers="Group C  Points" class="gt_row gt_right">6</td>
<td headers="Group C  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/10.png" style="height:15px;"></td></tr>
    <tr><td headers="Group C  Nation" class="gt_row gt_left">Denmark</td>
<td headers="Group C  Points" class="gt_row gt_right">3</td>
<td headers="Group C  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/21.png" style="height:15px;"></td></tr>
    <tr><td headers="Group C  Nation" class="gt_row gt_left">Serbia</td>
<td headers="Group C  Points" class="gt_row gt_right">0</td>
<td headers="Group C  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/14.png" style="height:15px;"></td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="3" class="gt_group_heading" style="background-color: #E8EBED;" scope="colgroup" id="Group D">Group D</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Group D  Nation" class="gt_row gt_left">France</td>
<td headers="Group D  Points" class="gt_row gt_right">9</td>
<td headers="Group D  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/2.png" style="height:15px;"></td></tr>
    <tr><td headers="Group D  Nation" class="gt_row gt_left">Austria</td>
<td headers="Group D  Points" class="gt_row gt_right">6</td>
<td headers="Group D  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/775.png" style="height:15px;"></td></tr>
    <tr><td headers="Group D  Nation" class="gt_row gt_left">Netherlands</td>
<td headers="Group D  Points" class="gt_row gt_right">3</td>
<td headers="Group D  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/1118.png" style="height:15px;"></td></tr>
    <tr><td headers="Group D  Nation" class="gt_row gt_left">Poland</td>
<td headers="Group D  Points" class="gt_row gt_right">0</td>
<td headers="Group D  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/24.png" style="height:15px;"></td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="3" class="gt_group_heading" style="background-color: #E8EBED;" scope="colgroup" id="Group E">Group E</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Group E  Nation" class="gt_row gt_left">Belgium</td>
<td headers="Group E  Points" class="gt_row gt_right">9</td>
<td headers="Group E  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/1.png" style="height:15px;"></td></tr>
    <tr><td headers="Group E  Nation" class="gt_row gt_left">Ukraine</td>
<td headers="Group E  Points" class="gt_row gt_right">6</td>
<td headers="Group E  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/772.png" style="height:15px;"></td></tr>
    <tr><td headers="Group E  Nation" class="gt_row gt_left">Romania</td>
<td headers="Group E  Points" class="gt_row gt_right">3</td>
<td headers="Group E  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/774.png" style="height:15px;"></td></tr>
    <tr><td headers="Group E  Nation" class="gt_row gt_left">Slovakia</td>
<td headers="Group E  Points" class="gt_row gt_right">0</td>
<td headers="Group E  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/773.png" style="height:15px;"></td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="3" class="gt_group_heading" style="background-color: #E8EBED;" scope="colgroup" id="Group F">Group F</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Group F  Nation" class="gt_row gt_left">Portugal</td>
<td headers="Group F  Points" class="gt_row gt_right">9</td>
<td headers="Group F  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/27.png" style="height:15px;"></td></tr>
    <tr><td headers="Group F  Nation" class="gt_row gt_left">Czech Republic</td>
<td headers="Group F  Points" class="gt_row gt_right">6</td>
<td headers="Group F  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/770.png" style="height:15px;"></td></tr>
    <tr><td headers="Group F  Nation" class="gt_row gt_left">Georgia</td>
<td headers="Group F  Points" class="gt_row gt_right">3</td>
<td headers="Group F  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/1104.png" style="height:15px;"></td></tr>
    <tr><td headers="Group F  Nation" class="gt_row gt_left">Turkey</td>
<td headers="Group F  Points" class="gt_row gt_right">0</td>
<td headers="Group F  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/777.png" style="height:15px;"></td></tr>
  </tbody>
  
  
</table>
</div>

### Knock-Out Rounds

Using the group-phase results, the knock-out round can be simulated.
Because the KO-rounds are based on the results of the group phase, and
then subsequent rounds are based on the previous rounds, it is more
challenging to code this efficiently. Firstly, the round of 16 is
prepared using the group-stage results.

    #Create seed column that combines rank and group for KO plan
    ranked_nations$seed = paste0(ranked_nations$Rank, ranked_nations$Group, sep = "")
    ko_1 = ko[1:8, ]
    ko_2 = ko[9:nrow(ko), 1:4] #Split Round of 16 with other rounds

    ko_1 = ko_1 %>%
      left_join(ranked_nations, by = c("Home" = "seed")) %>%
      mutate(Home = Nation) #Merge Rof16 home teams based on seed

    ko_1 = ko_1 %>%
      left_join(ranked_nations, by = c("Away" = "seed")) %>%
      mutate(Away = Nation.y) #Merge Rof16 away teams based on seed

    ko_1 = ko_1[, 1:4]
    ko = rbind(ko_1, ko_2) 
    ko$Game = as.character(ko$Game)

    round16 = ko[1:8, ] #Split dataframe into each round
    quarterfinal = ko[9:12, ]
    semifinal = ko[13:14, ]
    final = ko[15, ]

After using the seed from the group-stage to fill the games for the
round of 16, the KO-rounds will be simulated below

    ko_result = data.frame(Game = integer(), Home = character(), 
                            Away = character(), Winner = character(), 
                            stringsAsFactors = FALSE) #Initialize KO Result data frame


    # Adapt function to simulate a match for KO round
    simulate_ko_match = function(match, recent_performance, model) {
      home_team = match$Home
      away_team = match$Away
      
      # Extract recent performance metrics for home and away teams
      home_performance = recent_performance[recent_performance$Nation == home_team, ]
      away_performance = recent_performance[recent_performance$Nation == away_team, ]
      
      # Extract features for the match
      match_features = rbind(home_performance, away_performance)
      
      # Use random forest model to predict the outcome
      predicted_winner = ifelse(predict(model, newdata = match_features)[1] > 
                                  predict(model, newdata = match_features)[2], home_team, away_team)
      
      # Update tournament results dataframe
      match_result = data.frame(Game = match$Game, Home = match$Home, Away = match$Away, Winner = predicted_winner)
      ko_result <<- rbind(ko_result, match_result)
    }

    #Function to fill subsequent KO rounds based on results
    fill_ko_round = function(ko_result, next_round) {
      next_round = next_round %>%
        left_join(ko_result, by = c("Home" = "Game")) %>%
        mutate(Home = Winner) %>%
        left_join(ko_result, by = c("Away.x" = "Game")) %>%
        mutate(Away.x = Winner.y) %>%
        select(1:4)%>%
        rename(Away = `Away.x`,
               Home = `Home.x`)
      return(next_round)
    }

    #Loop through each KO round fill KO result data frame
    for (i in 1:nrow(round16)) {
      simulate_ko_match(round16[i, ], recent_performance, model)
    }

    quarterfinal = fill_ko_round(ko_result, quarterfinal)

    for (i in 1:nrow(quarterfinal)) {
      simulate_ko_match(quarterfinal[i, ], recent_performance, model)
    }

    semifinal = fill_ko_round(ko_result, semifinal)

    for (i in 1:nrow(semifinal)) {
      simulate_ko_match(semifinal[i, ], recent_performance, model)
    }

    final = fill_ko_round(ko_result, final)

    for (i in 1:nrow(final)) {
      simulate_ko_match(final[i, ], recent_performance, model)
    }

A new data frame for the KO-round results is initialized, followed by a
new function for the KO-game simulation. This had be defined anew as
there are a few differences in how the results data frame is updated,
while the prediciton itself stays the same. Since each subsequent
KO-round relies on the one before that, it was difficult to build one
single loop. Hence, a function was created to fill each round’s
indiviual data frames with the appropraite winners from the previous
rounds as participants. This was done from round of 16 all the way to
the final. Below is a table with the KO-round results and the predicted
EURO 2024 winner

    ko_result$Round = c('Round of 16','Round of 16','Round of 16','Round of 16','Round of 16',
                        'Round of 16','Round of 16','Round of 16','Quarterfinal','Quarterfinal',
                        'Quarterfinal','Quarterfinal','Semifinal','Semifinal','Final') #Add round column manually

    ko_result_logo = left_join(ko_result, team_data_scrape %>% distinct(name, .keep_all = TRUE), 
                                by = c("Winner" = "name")) %>%
      select(Game, Home, Away, Winner, Round, logo) #Merge with logo from team_scrape data frame

    #Create table for KO round results
    ko_table = gt(ko_result_logo) %>%
      tab_row_group(
        label = "Final",
        rows = 15) %>% 
      tab_row_group(
        label = "Semifinals",
        rows = 13:14) %>% 
      tab_row_group(
        label = "Quarterfinals",
        rows = 9:12) %>%
      tab_row_group(
        label = "Round of 16",
        rows = 1:8) %>%
      cols_hide(c(Game, Round)) %>%
      gt_img_rows(columns = logo, img_source = "web", height = 15)
    ko_table = tab_style(ko_table, cell_fill('#bac2ca'), 
                            cells_column_labels())
    ko_table = tab_style(ko_table, cell_fill('#E8EBED'), 
                            cells_row_groups())
    ko_table = tab_header(ko_table, "EURO 2024 Knock-Out Round Results")
    ko_table

<div id="yjiqgabhdv" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#yjiqgabhdv table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#yjiqgabhdv thead, #yjiqgabhdv tbody, #yjiqgabhdv tfoot, #yjiqgabhdv tr, #yjiqgabhdv td, #yjiqgabhdv th {
  border-style: none;
}

#yjiqgabhdv p {
  margin: 0;
  padding: 0;
}

#yjiqgabhdv .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#yjiqgabhdv .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#yjiqgabhdv .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#yjiqgabhdv .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#yjiqgabhdv .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#yjiqgabhdv .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#yjiqgabhdv .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#yjiqgabhdv .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#yjiqgabhdv .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#yjiqgabhdv .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#yjiqgabhdv .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#yjiqgabhdv .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#yjiqgabhdv .gt_spanner_row {
  border-bottom-style: hidden;
}

#yjiqgabhdv .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#yjiqgabhdv .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#yjiqgabhdv .gt_from_md > :first-child {
  margin-top: 0;
}

#yjiqgabhdv .gt_from_md > :last-child {
  margin-bottom: 0;
}

#yjiqgabhdv .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#yjiqgabhdv .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#yjiqgabhdv .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#yjiqgabhdv .gt_row_group_first td {
  border-top-width: 2px;
}

#yjiqgabhdv .gt_row_group_first th {
  border-top-width: 2px;
}

#yjiqgabhdv .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#yjiqgabhdv .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#yjiqgabhdv .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#yjiqgabhdv .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#yjiqgabhdv .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#yjiqgabhdv .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#yjiqgabhdv .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#yjiqgabhdv .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#yjiqgabhdv .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#yjiqgabhdv .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#yjiqgabhdv .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#yjiqgabhdv .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#yjiqgabhdv .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#yjiqgabhdv .gt_left {
  text-align: left;
}

#yjiqgabhdv .gt_center {
  text-align: center;
}

#yjiqgabhdv .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#yjiqgabhdv .gt_font_normal {
  font-weight: normal;
}

#yjiqgabhdv .gt_font_bold {
  font-weight: bold;
}

#yjiqgabhdv .gt_font_italic {
  font-style: italic;
}

#yjiqgabhdv .gt_super {
  font-size: 65%;
}

#yjiqgabhdv .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#yjiqgabhdv .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#yjiqgabhdv .gt_indent_1 {
  text-indent: 5px;
}

#yjiqgabhdv .gt_indent_2 {
  text-indent: 10px;
}

#yjiqgabhdv .gt_indent_3 {
  text-indent: 15px;
}

#yjiqgabhdv .gt_indent_4 {
  text-indent: 20px;
}

#yjiqgabhdv .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="4" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>EURO 2024 Knock-Out Round Results</td>
    </tr>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" style="background-color: #BAC2CA;" scope="col" id="Home">Home</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" style="background-color: #BAC2CA;" scope="col" id="Away">Away</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" style="background-color: #BAC2CA;" scope="col" id="Winner">Winner</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" style="background-color: #BAC2CA;" scope="col" id="logo">logo</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" style="background-color: #E8EBED;" scope="colgroup" id="Round of 16">Round of 16</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Round of 16  Home" class="gt_row gt_left">Hungary</td>
<td headers="Round of 16  Away" class="gt_row gt_left">Italy</td>
<td headers="Round of 16  Winner" class="gt_row gt_left">Italy</td>
<td headers="Round of 16  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/768.png" style="height:15px;"></td></tr>
    <tr><td headers="Round of 16  Home" class="gt_row gt_left">Germany</td>
<td headers="Round of 16  Away" class="gt_row gt_left">England</td>
<td headers="Round of 16  Winner" class="gt_row gt_left">England</td>
<td headers="Round of 16  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/10.png" style="height:15px;"></td></tr>
    <tr><td headers="Round of 16  Home" class="gt_row gt_left">Slovenia</td>
<td headers="Round of 16  Away" class="gt_row gt_left">Netherlands</td>
<td headers="Round of 16  Winner" class="gt_row gt_left">Slovenia</td>
<td headers="Round of 16  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/1091.png" style="height:15px;"></td></tr>
    <tr><td headers="Round of 16  Home" class="gt_row gt_left">Spain</td>
<td headers="Round of 16  Away" class="gt_row gt_left">Scotland</td>
<td headers="Round of 16  Winner" class="gt_row gt_left">Spain</td>
<td headers="Round of 16  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/9.png" style="height:15px;"></td></tr>
    <tr><td headers="Round of 16  Home" class="gt_row gt_left">Austria</td>
<td headers="Round of 16  Away" class="gt_row gt_left">Ukraine</td>
<td headers="Round of 16  Winner" class="gt_row gt_left">Austria</td>
<td headers="Round of 16  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/775.png" style="height:15px;"></td></tr>
    <tr><td headers="Round of 16  Home" class="gt_row gt_left">Portugal</td>
<td headers="Round of 16  Away" class="gt_row gt_left">Croatia</td>
<td headers="Round of 16  Winner" class="gt_row gt_left">Portugal</td>
<td headers="Round of 16  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/27.png" style="height:15px;"></td></tr>
    <tr><td headers="Round of 16  Home" class="gt_row gt_left">Belgium</td>
<td headers="Round of 16  Away" class="gt_row gt_left">Denmark</td>
<td headers="Round of 16  Winner" class="gt_row gt_left">Belgium</td>
<td headers="Round of 16  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/1.png" style="height:15px;"></td></tr>
    <tr><td headers="Round of 16  Home" class="gt_row gt_left">France</td>
<td headers="Round of 16  Away" class="gt_row gt_left">Czech Republic</td>
<td headers="Round of 16  Winner" class="gt_row gt_left">France</td>
<td headers="Round of 16  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/2.png" style="height:15px;"></td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" style="background-color: #E8EBED;" scope="colgroup" id="Quarterfinals">Quarterfinals</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Quarterfinals  Home" class="gt_row gt_left">Spain</td>
<td headers="Quarterfinals  Away" class="gt_row gt_left">England</td>
<td headers="Quarterfinals  Winner" class="gt_row gt_left">Spain</td>
<td headers="Quarterfinals  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/9.png" style="height:15px;"></td></tr>
    <tr><td headers="Quarterfinals  Home" class="gt_row gt_left">Portugal</td>
<td headers="Quarterfinals  Away" class="gt_row gt_left">Austria</td>
<td headers="Quarterfinals  Winner" class="gt_row gt_left">Portugal</td>
<td headers="Quarterfinals  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/27.png" style="height:15px;"></td></tr>
    <tr><td headers="Quarterfinals  Home" class="gt_row gt_left">Slovenia</td>
<td headers="Quarterfinals  Away" class="gt_row gt_left">Italy</td>
<td headers="Quarterfinals  Winner" class="gt_row gt_left">Slovenia</td>
<td headers="Quarterfinals  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/1091.png" style="height:15px;"></td></tr>
    <tr><td headers="Quarterfinals  Home" class="gt_row gt_left">Belgium</td>
<td headers="Quarterfinals  Away" class="gt_row gt_left">France</td>
<td headers="Quarterfinals  Winner" class="gt_row gt_left">France</td>
<td headers="Quarterfinals  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/2.png" style="height:15px;"></td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" style="background-color: #E8EBED;" scope="colgroup" id="Semifinals">Semifinals</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Semifinals  Home" class="gt_row gt_left">Spain</td>
<td headers="Semifinals  Away" class="gt_row gt_left">Portugal</td>
<td headers="Semifinals  Winner" class="gt_row gt_left">Portugal</td>
<td headers="Semifinals  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/27.png" style="height:15px;"></td></tr>
    <tr><td headers="Semifinals  Home" class="gt_row gt_left">France</td>
<td headers="Semifinals  Away" class="gt_row gt_left">Slovenia</td>
<td headers="Semifinals  Winner" class="gt_row gt_left">France</td>
<td headers="Semifinals  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/2.png" style="height:15px;"></td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" style="background-color: #E8EBED;" scope="colgroup" id="Final">Final</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Final  Home" class="gt_row gt_left">Portugal</td>
<td headers="Final  Away" class="gt_row gt_left">France</td>
<td headers="Final  Winner" class="gt_row gt_left">Portugal</td>
<td headers="Final  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/27.png" style="height:15px;"></td></tr>
  </tbody>
  
  
</table>
</div>

As the table shows, the final consisted of France versus Portugal, with
Portugal as the winner. Below is a print of Portugal’s features of the
23/24 seasons, to understand further what made them the winner.

    portugal = recent_performance %>%
      filter(Nation == 'Portugal')
    portugal = as.character(portugal)

    portugal = data.frame(names = c("Nation", "Wins_Total", "Draws_Home", "Draws_Away", "Loses_Home",
                                     "Loses_Away", "Goals_Scored", "Goals_Conceded", 'Height', 
                                     'Weight', 'Rating', 'Minutes_Played', 'Total_Shots', 
                                     'Player_Goals', 'Total_Passes', 'Key_Passes', 'Accuracy_Passes',
                                     'Tackles', 'Dribble_Efficiency', 'Duel_Efficiency', 
                                     'Fouls_Drawn', 'Fouls_Comitted', 'Yellows', 'Reds', 
                                     'Yellowreds', 'Age'),
                      vars = portugal)

    porto_table = gt(portugal) %>%
        cols_label(names = '', vars = '')

    porto_table = tab_header(porto_table, "Portugal Features 2023/2024")
    porto_table

<div id="wrwurlrsbp" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#wrwurlrsbp table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#wrwurlrsbp thead, #wrwurlrsbp tbody, #wrwurlrsbp tfoot, #wrwurlrsbp tr, #wrwurlrsbp td, #wrwurlrsbp th {
  border-style: none;
}

#wrwurlrsbp p {
  margin: 0;
  padding: 0;
}

#wrwurlrsbp .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#wrwurlrsbp .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#wrwurlrsbp .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#wrwurlrsbp .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#wrwurlrsbp .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#wrwurlrsbp .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#wrwurlrsbp .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#wrwurlrsbp .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#wrwurlrsbp .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#wrwurlrsbp .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#wrwurlrsbp .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#wrwurlrsbp .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#wrwurlrsbp .gt_spanner_row {
  border-bottom-style: hidden;
}

#wrwurlrsbp .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#wrwurlrsbp .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#wrwurlrsbp .gt_from_md > :first-child {
  margin-top: 0;
}

#wrwurlrsbp .gt_from_md > :last-child {
  margin-bottom: 0;
}

#wrwurlrsbp .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#wrwurlrsbp .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#wrwurlrsbp .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#wrwurlrsbp .gt_row_group_first td {
  border-top-width: 2px;
}

#wrwurlrsbp .gt_row_group_first th {
  border-top-width: 2px;
}

#wrwurlrsbp .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#wrwurlrsbp .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#wrwurlrsbp .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#wrwurlrsbp .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#wrwurlrsbp .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#wrwurlrsbp .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#wrwurlrsbp .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#wrwurlrsbp .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#wrwurlrsbp .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#wrwurlrsbp .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#wrwurlrsbp .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#wrwurlrsbp .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#wrwurlrsbp .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#wrwurlrsbp .gt_left {
  text-align: left;
}

#wrwurlrsbp .gt_center {
  text-align: center;
}

#wrwurlrsbp .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#wrwurlrsbp .gt_font_normal {
  font-weight: normal;
}

#wrwurlrsbp .gt_font_bold {
  font-weight: bold;
}

#wrwurlrsbp .gt_font_italic {
  font-style: italic;
}

#wrwurlrsbp .gt_super {
  font-size: 65%;
}

#wrwurlrsbp .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#wrwurlrsbp .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#wrwurlrsbp .gt_indent_1 {
  text-indent: 5px;
}

#wrwurlrsbp .gt_indent_2 {
  text-indent: 10px;
}

#wrwurlrsbp .gt_indent_3 {
  text-indent: 15px;
}

#wrwurlrsbp .gt_indent_4 {
  text-indent: 20px;
}

#wrwurlrsbp .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="2" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Portugal Features 2023/2024</td>
    </tr>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id=""></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id=""></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="names" class="gt_row gt_left">Nation</td>
<td headers="vars" class="gt_row gt_left">Portugal</td></tr>
    <tr><td headers="names" class="gt_row gt_left">Wins_Total</td>
<td headers="vars" class="gt_row gt_left">11</td></tr>
    <tr><td headers="names" class="gt_row gt_left">Draws_Home</td>
<td headers="vars" class="gt_row gt_left">0</td></tr>
    <tr><td headers="names" class="gt_row gt_left">Draws_Away</td>
<td headers="vars" class="gt_row gt_left">0</td></tr>
    <tr><td headers="names" class="gt_row gt_left">Loses_Home</td>
<td headers="vars" class="gt_row gt_left">0</td></tr>
    <tr><td headers="names" class="gt_row gt_left">Loses_Away</td>
<td headers="vars" class="gt_row gt_left">1</td></tr>
    <tr><td headers="names" class="gt_row gt_left">Goals_Scored</td>
<td headers="vars" class="gt_row gt_left">41</td></tr>
    <tr><td headers="names" class="gt_row gt_left">Goals_Conceded</td>
<td headers="vars" class="gt_row gt_left">6</td></tr>
    <tr><td headers="names" class="gt_row gt_left">Height</td>
<td headers="vars" class="gt_row gt_left">181.405193236715</td></tr>
    <tr><td headers="names" class="gt_row gt_left">Weight</td>
<td headers="vars" class="gt_row gt_left">73.7161835748792</td></tr>
    <tr><td headers="names" class="gt_row gt_left">Rating</td>
<td headers="vars" class="gt_row gt_left">0</td></tr>
    <tr><td headers="names" class="gt_row gt_left">Minutes_Played</td>
<td headers="vars" class="gt_row gt_left">1583.59903381643</td></tr>
    <tr><td headers="names" class="gt_row gt_left">Total_Shots</td>
<td headers="vars" class="gt_row gt_left">15.8152173913043</td></tr>
    <tr><td headers="names" class="gt_row gt_left">Player_Goals</td>
<td headers="vars" class="gt_row gt_left">3.65942028985507</td></tr>
    <tr><td headers="names" class="gt_row gt_left">Total_Passes</td>
<td headers="vars" class="gt_row gt_left">792.641304347826</td></tr>
    <tr><td headers="names" class="gt_row gt_left">Key_Passes</td>
<td headers="vars" class="gt_row gt_left">17.7717391304348</td></tr>
    <tr><td headers="names" class="gt_row gt_left">Accuracy_Passes</td>
<td headers="vars" class="gt_row gt_left">69.5434782608696</td></tr>
    <tr><td headers="names" class="gt_row gt_left">Tackles</td>
<td headers="vars" class="gt_row gt_left">23.9021739130435</td></tr>
    <tr><td headers="names" class="gt_row gt_left">Dribble_Efficiency</td>
<td headers="vars" class="gt_row gt_left">0</td></tr>
    <tr><td headers="names" class="gt_row gt_left">Duel_Efficiency</td>
<td headers="vars" class="gt_row gt_left">0</td></tr>
    <tr><td headers="names" class="gt_row gt_left">Fouls_Drawn</td>
<td headers="vars" class="gt_row gt_left">14.6195652173913</td></tr>
    <tr><td headers="names" class="gt_row gt_left">Fouls_Comitted</td>
<td headers="vars" class="gt_row gt_left">15.3586956521739</td></tr>
    <tr><td headers="names" class="gt_row gt_left">Yellows</td>
<td headers="vars" class="gt_row gt_left">2.89251207729469</td></tr>
    <tr><td headers="names" class="gt_row gt_left">Reds</td>
<td headers="vars" class="gt_row gt_left">0.0434782608695652</td></tr>
    <tr><td headers="names" class="gt_row gt_left">Yellowreds</td>
<td headers="vars" class="gt_row gt_left">0.0652173913043478</td></tr>
    <tr><td headers="names" class="gt_row gt_left">Age</td>
<td headers="vars" class="gt_row gt_left">27.0132875007017</td></tr>
  </tbody>
  
  
</table>
</div>

The high count of scored goals, 41, appears to be a big reason of
Portugal winning this EURO 2024, but other factors, such as 11 wins and
only 1 loss or an average of 1583 minutes played for the 23/24 seasons
by the players also likely play a role. Interestingly, the average age
is 27, which suggests that experience may be more important than the
generally thought-of young athletisism (although 27 is by no means old,
except in sports). For completeness, a table with all group-phase games
results is shown as well.

    group_results_table = tournament_results %>%
      gt() %>%
      tab_row_group(
        label = "Group F",
        rows = Group == "F") %>%
      tab_row_group(
        label = "Group E",
        rows = Group == "E") %>%
      tab_row_group(
        label = "Group D",
        rows = Group == "D") %>%
      tab_row_group(
        label = "Group C",
        rows = Group == "C") %>%
      tab_row_group(
        label = "Group B",
        rows = Group == "B") %>%
      tab_row_group(
        label = "Group A",
        rows = Group == "A") %>%
      cols_hide(Group)
    group_results_table = tab_style(group_results_table, cell_fill('#bac2ca'), 
                         cells_column_labels())
    group_results_table = tab_style(group_results_table, cell_fill('#E8EBED'), 
                         cells_row_groups())
    group_results_table = tab_header(group_results_table, "EURO 2024 Group Phase Results")
    group_results_table

<div id="qstdcwrtuz" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#qstdcwrtuz table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#qstdcwrtuz thead, #qstdcwrtuz tbody, #qstdcwrtuz tfoot, #qstdcwrtuz tr, #qstdcwrtuz td, #qstdcwrtuz th {
  border-style: none;
}

#qstdcwrtuz p {
  margin: 0;
  padding: 0;
}

#qstdcwrtuz .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#qstdcwrtuz .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#qstdcwrtuz .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#qstdcwrtuz .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#qstdcwrtuz .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#qstdcwrtuz .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#qstdcwrtuz .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#qstdcwrtuz .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#qstdcwrtuz .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#qstdcwrtuz .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#qstdcwrtuz .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#qstdcwrtuz .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#qstdcwrtuz .gt_spanner_row {
  border-bottom-style: hidden;
}

#qstdcwrtuz .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#qstdcwrtuz .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#qstdcwrtuz .gt_from_md > :first-child {
  margin-top: 0;
}

#qstdcwrtuz .gt_from_md > :last-child {
  margin-bottom: 0;
}

#qstdcwrtuz .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#qstdcwrtuz .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#qstdcwrtuz .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#qstdcwrtuz .gt_row_group_first td {
  border-top-width: 2px;
}

#qstdcwrtuz .gt_row_group_first th {
  border-top-width: 2px;
}

#qstdcwrtuz .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#qstdcwrtuz .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#qstdcwrtuz .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#qstdcwrtuz .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#qstdcwrtuz .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#qstdcwrtuz .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#qstdcwrtuz .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#qstdcwrtuz .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#qstdcwrtuz .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#qstdcwrtuz .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#qstdcwrtuz .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#qstdcwrtuz .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#qstdcwrtuz .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#qstdcwrtuz .gt_left {
  text-align: left;
}

#qstdcwrtuz .gt_center {
  text-align: center;
}

#qstdcwrtuz .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#qstdcwrtuz .gt_font_normal {
  font-weight: normal;
}

#qstdcwrtuz .gt_font_bold {
  font-weight: bold;
}

#qstdcwrtuz .gt_font_italic {
  font-style: italic;
}

#qstdcwrtuz .gt_super {
  font-size: 65%;
}

#qstdcwrtuz .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#qstdcwrtuz .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#qstdcwrtuz .gt_indent_1 {
  text-indent: 5px;
}

#qstdcwrtuz .gt_indent_2 {
  text-indent: 10px;
}

#qstdcwrtuz .gt_indent_3 {
  text-indent: 15px;
}

#qstdcwrtuz .gt_indent_4 {
  text-indent: 20px;
}

#qstdcwrtuz .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="3" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>EURO 2024 Group Phase Results</td>
    </tr>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" style="background-color: #BAC2CA;" scope="col" id="Home">Home</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" style="background-color: #BAC2CA;" scope="col" id="Away">Away</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" style="background-color: #BAC2CA;" scope="col" id="Winner">Winner</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr class="gt_group_heading_row">
      <th colspan="3" class="gt_group_heading" style="background-color: #E8EBED;" scope="colgroup" id="Group A">Group A</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Group A  Home" class="gt_row gt_left">Germany</td>
<td headers="Group A  Away" class="gt_row gt_left">Scotland</td>
<td headers="Group A  Winner" class="gt_row gt_left">Germany</td></tr>
    <tr><td headers="Group A  Home" class="gt_row gt_left">Hungary</td>
<td headers="Group A  Away" class="gt_row gt_left">Switzerland</td>
<td headers="Group A  Winner" class="gt_row gt_left">Hungary</td></tr>
    <tr><td headers="Group A  Home" class="gt_row gt_left">Germany</td>
<td headers="Group A  Away" class="gt_row gt_left">Hungary</td>
<td headers="Group A  Winner" class="gt_row gt_left">Germany</td></tr>
    <tr><td headers="Group A  Home" class="gt_row gt_left">Scotland</td>
<td headers="Group A  Away" class="gt_row gt_left">Switzerland</td>
<td headers="Group A  Winner" class="gt_row gt_left">Scotland</td></tr>
    <tr><td headers="Group A  Home" class="gt_row gt_left">Switzerland</td>
<td headers="Group A  Away" class="gt_row gt_left">Germany</td>
<td headers="Group A  Winner" class="gt_row gt_left">Germany</td></tr>
    <tr><td headers="Group A  Home" class="gt_row gt_left">Scotland</td>
<td headers="Group A  Away" class="gt_row gt_left">Hungary</td>
<td headers="Group A  Winner" class="gt_row gt_left">Hungary</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="3" class="gt_group_heading" style="background-color: #E8EBED;" scope="colgroup" id="Group B">Group B</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Group B  Home" class="gt_row gt_left">Spain</td>
<td headers="Group B  Away" class="gt_row gt_left">Croatia</td>
<td headers="Group B  Winner" class="gt_row gt_left">Spain</td></tr>
    <tr><td headers="Group B  Home" class="gt_row gt_left">Italy</td>
<td headers="Group B  Away" class="gt_row gt_left">Albania</td>
<td headers="Group B  Winner" class="gt_row gt_left">Italy</td></tr>
    <tr><td headers="Group B  Home" class="gt_row gt_left">Croatia</td>
<td headers="Group B  Away" class="gt_row gt_left">Albania</td>
<td headers="Group B  Winner" class="gt_row gt_left">Croatia</td></tr>
    <tr><td headers="Group B  Home" class="gt_row gt_left">Spain</td>
<td headers="Group B  Away" class="gt_row gt_left">Italy</td>
<td headers="Group B  Winner" class="gt_row gt_left">Spain</td></tr>
    <tr><td headers="Group B  Home" class="gt_row gt_left">Croatia</td>
<td headers="Group B  Away" class="gt_row gt_left">Italy</td>
<td headers="Group B  Winner" class="gt_row gt_left">Italy</td></tr>
    <tr><td headers="Group B  Home" class="gt_row gt_left">Albania</td>
<td headers="Group B  Away" class="gt_row gt_left">Spain</td>
<td headers="Group B  Winner" class="gt_row gt_left">Spain</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="3" class="gt_group_heading" style="background-color: #E8EBED;" scope="colgroup" id="Group C">Group C</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Group C  Home" class="gt_row gt_left">Slovenia</td>
<td headers="Group C  Away" class="gt_row gt_left">Denmark</td>
<td headers="Group C  Winner" class="gt_row gt_left">Slovenia</td></tr>
    <tr><td headers="Group C  Home" class="gt_row gt_left">Serbia</td>
<td headers="Group C  Away" class="gt_row gt_left">England</td>
<td headers="Group C  Winner" class="gt_row gt_left">England</td></tr>
    <tr><td headers="Group C  Home" class="gt_row gt_left">Slovenia</td>
<td headers="Group C  Away" class="gt_row gt_left">Serbia</td>
<td headers="Group C  Winner" class="gt_row gt_left">Slovenia</td></tr>
    <tr><td headers="Group C  Home" class="gt_row gt_left">Denmark</td>
<td headers="Group C  Away" class="gt_row gt_left">England</td>
<td headers="Group C  Winner" class="gt_row gt_left">England</td></tr>
    <tr><td headers="Group C  Home" class="gt_row gt_left">England</td>
<td headers="Group C  Away" class="gt_row gt_left">Slovenia</td>
<td headers="Group C  Winner" class="gt_row gt_left">Slovenia</td></tr>
    <tr><td headers="Group C  Home" class="gt_row gt_left">Denmark</td>
<td headers="Group C  Away" class="gt_row gt_left">Serbia</td>
<td headers="Group C  Winner" class="gt_row gt_left">Denmark</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="3" class="gt_group_heading" style="background-color: #E8EBED;" scope="colgroup" id="Group D">Group D</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Group D  Home" class="gt_row gt_left">Poland</td>
<td headers="Group D  Away" class="gt_row gt_left">Netherlands</td>
<td headers="Group D  Winner" class="gt_row gt_left">Netherlands</td></tr>
    <tr><td headers="Group D  Home" class="gt_row gt_left">Austria</td>
<td headers="Group D  Away" class="gt_row gt_left">France</td>
<td headers="Group D  Winner" class="gt_row gt_left">France</td></tr>
    <tr><td headers="Group D  Home" class="gt_row gt_left">Poland</td>
<td headers="Group D  Away" class="gt_row gt_left">Austria</td>
<td headers="Group D  Winner" class="gt_row gt_left">Austria</td></tr>
    <tr><td headers="Group D  Home" class="gt_row gt_left">Netherlands</td>
<td headers="Group D  Away" class="gt_row gt_left">France</td>
<td headers="Group D  Winner" class="gt_row gt_left">France</td></tr>
    <tr><td headers="Group D  Home" class="gt_row gt_left">Netherlands</td>
<td headers="Group D  Away" class="gt_row gt_left">Austria</td>
<td headers="Group D  Winner" class="gt_row gt_left">Austria</td></tr>
    <tr><td headers="Group D  Home" class="gt_row gt_left">France</td>
<td headers="Group D  Away" class="gt_row gt_left">Poland</td>
<td headers="Group D  Winner" class="gt_row gt_left">France</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="3" class="gt_group_heading" style="background-color: #E8EBED;" scope="colgroup" id="Group E">Group E</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Group E  Home" class="gt_row gt_left">Romania</td>
<td headers="Group E  Away" class="gt_row gt_left">Ukraine</td>
<td headers="Group E  Winner" class="gt_row gt_left">Ukraine</td></tr>
    <tr><td headers="Group E  Home" class="gt_row gt_left">Belgium</td>
<td headers="Group E  Away" class="gt_row gt_left">Slovakia</td>
<td headers="Group E  Winner" class="gt_row gt_left">Belgium</td></tr>
    <tr><td headers="Group E  Home" class="gt_row gt_left">Slovakia</td>
<td headers="Group E  Away" class="gt_row gt_left">Ukraine</td>
<td headers="Group E  Winner" class="gt_row gt_left">Ukraine</td></tr>
    <tr><td headers="Group E  Home" class="gt_row gt_left">Belgium</td>
<td headers="Group E  Away" class="gt_row gt_left">Romania</td>
<td headers="Group E  Winner" class="gt_row gt_left">Belgium</td></tr>
    <tr><td headers="Group E  Home" class="gt_row gt_left">Slovakia</td>
<td headers="Group E  Away" class="gt_row gt_left">Romania</td>
<td headers="Group E  Winner" class="gt_row gt_left">Romania</td></tr>
    <tr><td headers="Group E  Home" class="gt_row gt_left">Ukraine</td>
<td headers="Group E  Away" class="gt_row gt_left">Belgium</td>
<td headers="Group E  Winner" class="gt_row gt_left">Belgium</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="3" class="gt_group_heading" style="background-color: #E8EBED;" scope="colgroup" id="Group F">Group F</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Group F  Home" class="gt_row gt_left">Turkey</td>
<td headers="Group F  Away" class="gt_row gt_left">Georgia</td>
<td headers="Group F  Winner" class="gt_row gt_left">Georgia</td></tr>
    <tr><td headers="Group F  Home" class="gt_row gt_left">Portugal</td>
<td headers="Group F  Away" class="gt_row gt_left">Czech Republic</td>
<td headers="Group F  Winner" class="gt_row gt_left">Portugal</td></tr>
    <tr><td headers="Group F  Home" class="gt_row gt_left">Georgia</td>
<td headers="Group F  Away" class="gt_row gt_left">Czech Republic</td>
<td headers="Group F  Winner" class="gt_row gt_left">Czech Republic</td></tr>
    <tr><td headers="Group F  Home" class="gt_row gt_left">Turkey</td>
<td headers="Group F  Away" class="gt_row gt_left">Portugal</td>
<td headers="Group F  Winner" class="gt_row gt_left">Portugal</td></tr>
    <tr><td headers="Group F  Home" class="gt_row gt_left">Czech Republic</td>
<td headers="Group F  Away" class="gt_row gt_left">Turkey</td>
<td headers="Group F  Winner" class="gt_row gt_left">Czech Republic</td></tr>
    <tr><td headers="Group F  Home" class="gt_row gt_left">Georgia</td>
<td headers="Group F  Away" class="gt_row gt_left">Portugal</td>
<td headers="Group F  Winner" class="gt_row gt_left">Portugal</td></tr>
  </tbody>
  
  
</table>
</div>

## Model Comparison

In order to compare the simulation with the trained random forest model,
below is the same simulation performed, but with FIFA rank as
explanatory variable. This means that per game, the winner will be
whoever has a lower FIFA rank. Certainly, utilizing only the FIFA rank
as foundation for a simulation grossly oversimplifies the complexity of
soccer. The FIFA ranks were manually entered into the nations data
frame, and are based on 04/27/2024
(<https://inside.fifa.com/fifa-world-ranking/men>). Scraping was
attempted, however, the FIFA website utilizes not only HTML but Java
coding, which turned out to be too complex. Keeping in mind that a
scraping methodology would allow a dynamic model implementation.

In order to run the simulation, the “simulate match” function has to be
tweaked a bit, as well as the KO functions, to include the FIFA ranking
instead of the random forest model. The same tables as shown above are
included again here, to assess the results of group and KO phase.

    group = read.csv('https://raw.githubusercontent.com/lucasweyrich958/EURO2024_Simulation/main/EURO2024_Group_State.csv')

    tournament_results_FIFA = data.frame(Round = integer(), Home = character(), Away = character(), 
                                    Winner = character(), stringsAsFactors = FALSE)

    # Initialize a dataframe to track points for each nation
    nation_points_FIFA = data.frame(Nation = nations$Nation, Points = 0, Group = nations$Group)

    #Create function to simulate match based on FIFA rank
    simulate_match_FIFA = function(match, nations) {
      home_team = match$Home
      away_team = match$Away
      
      # Extract recent performance metrics for home and away teams
      home_rank = nations[nations$Nation == home_team, ]
      away_rank = nations[nations$Nation == away_team, ]
      
      # Use random forest model to predict the outcome
      predicted_winner = ifelse(home_rank$FIFA_Rank < 
                                  away_rank$FIFA_Rank, home_team, away_team)
      
      # Update tournament results dataframe
      match_result = data.frame(Group = match$Group, Home = match$Home, Away = match$Away, 
                                Winner = predicted_winner)
      tournament_results_FIFA <<- rbind(tournament_results, match_result)
    }

    #Simulate matches by looping through nations data frame that includes FIFA ranks
    for (i in 1:nrow(group)) {
      simulate_match_FIFA(group[i, ], nations)
    }

    for (i in 1:nrow(tournament_results_FIFA)) {
      nation_points_FIFA = update_points(tournament_results_FIFA[i, ], nation_points_FIFA)
    }

    #Initialize a data frame to rank the group phase
    ranked_nations_FIFA = data.frame()
    for (group in unique(nation_points_FIFA$Group)) {
      group_nations_FIFA = subset(nation_points_FIFA, Group == group)
      group_nations_FIFA = group_nations_FIFA[order(-group_nations_FIFA$Points), ]
      group_nations_FIFA$Rank = 1:nrow(group_nations_FIFA)
      ranked_nations_FIFA = rbind(ranked_nations_FIFA, group_nations_FIFA)}

    ranked_nations_logo_FIFA = left_join(ranked_nations_FIFA, team_data_scrape %>% distinct(name, .keep_all = TRUE), 
                                    by = c("Nation" = "name")) %>%
      select(Nation, Points, Group, Rank, logo) #Merge the team_scrape data frame for the logos

    #Create a table to show the group phase results
    group_table_FIFA = gt(ranked_nations_logo_FIFA) %>%
      tab_row_group(
        label = "Group F",
        rows = Group == "F") %>% 
      tab_row_group(
        label = "Group E",
        rows = Group == "E") %>% 
      tab_row_group(
        label = "Group D",
        rows = Group == "D") %>%
      tab_row_group(
        label = "Group C",
        rows = Group == "C") %>%
      tab_row_group(
        label = "Group B",
        rows = Group == "B") %>%
      tab_row_group(
        label = "Group A",
        rows = Group == "A") %>%
      cols_hide(c(Group, Rank)) %>%
      gt_img_rows(columns = logo, img_source = "web", height = 15)
    group_table_FIFA = tab_style(group_table_FIFA, cell_fill('#bac2ca'), 
                            cells_column_labels())
    group_table_FIFA = tab_style(group_table_FIFA, cell_fill('#E8EBED'), 
                            cells_row_groups())
    group_table_FIFA = tab_header(group_table, "EURO 2024 Group Phase Standings - FIFA Ranking")
    group_table_FIFA

<div id="gwmabzluad" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#gwmabzluad table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#gwmabzluad thead, #gwmabzluad tbody, #gwmabzluad tfoot, #gwmabzluad tr, #gwmabzluad td, #gwmabzluad th {
  border-style: none;
}

#gwmabzluad p {
  margin: 0;
  padding: 0;
}

#gwmabzluad .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#gwmabzluad .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#gwmabzluad .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#gwmabzluad .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#gwmabzluad .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#gwmabzluad .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#gwmabzluad .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#gwmabzluad .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#gwmabzluad .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#gwmabzluad .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#gwmabzluad .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#gwmabzluad .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#gwmabzluad .gt_spanner_row {
  border-bottom-style: hidden;
}

#gwmabzluad .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#gwmabzluad .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#gwmabzluad .gt_from_md > :first-child {
  margin-top: 0;
}

#gwmabzluad .gt_from_md > :last-child {
  margin-bottom: 0;
}

#gwmabzluad .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#gwmabzluad .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#gwmabzluad .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#gwmabzluad .gt_row_group_first td {
  border-top-width: 2px;
}

#gwmabzluad .gt_row_group_first th {
  border-top-width: 2px;
}

#gwmabzluad .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#gwmabzluad .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#gwmabzluad .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#gwmabzluad .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#gwmabzluad .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#gwmabzluad .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#gwmabzluad .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#gwmabzluad .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#gwmabzluad .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#gwmabzluad .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#gwmabzluad .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#gwmabzluad .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#gwmabzluad .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#gwmabzluad .gt_left {
  text-align: left;
}

#gwmabzluad .gt_center {
  text-align: center;
}

#gwmabzluad .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#gwmabzluad .gt_font_normal {
  font-weight: normal;
}

#gwmabzluad .gt_font_bold {
  font-weight: bold;
}

#gwmabzluad .gt_font_italic {
  font-style: italic;
}

#gwmabzluad .gt_super {
  font-size: 65%;
}

#gwmabzluad .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#gwmabzluad .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#gwmabzluad .gt_indent_1 {
  text-indent: 5px;
}

#gwmabzluad .gt_indent_2 {
  text-indent: 10px;
}

#gwmabzluad .gt_indent_3 {
  text-indent: 15px;
}

#gwmabzluad .gt_indent_4 {
  text-indent: 20px;
}

#gwmabzluad .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="3" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>EURO 2024 Group Phase Standings - FIFA Ranking</td>
    </tr>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" style="background-color: #BAC2CA;" scope="col" id="Nation">Nation</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="background-color: #BAC2CA;" scope="col" id="Points">Points</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" style="background-color: #BAC2CA;" scope="col" id="logo">logo</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr class="gt_group_heading_row">
      <th colspan="3" class="gt_group_heading" style="background-color: #E8EBED;" scope="colgroup" id="Group A">Group A</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Group A  Nation" class="gt_row gt_left">Germany</td>
<td headers="Group A  Points" class="gt_row gt_right">9</td>
<td headers="Group A  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/25.png" style="height:15px;"></td></tr>
    <tr><td headers="Group A  Nation" class="gt_row gt_left">Hungary</td>
<td headers="Group A  Points" class="gt_row gt_right">6</td>
<td headers="Group A  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/769.png" style="height:15px;"></td></tr>
    <tr><td headers="Group A  Nation" class="gt_row gt_left">Scotland</td>
<td headers="Group A  Points" class="gt_row gt_right">3</td>
<td headers="Group A  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/1108.png" style="height:15px;"></td></tr>
    <tr><td headers="Group A  Nation" class="gt_row gt_left">Switzerland</td>
<td headers="Group A  Points" class="gt_row gt_right">0</td>
<td headers="Group A  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/15.png" style="height:15px;"></td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="3" class="gt_group_heading" style="background-color: #E8EBED;" scope="colgroup" id="Group B">Group B</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Group B  Nation" class="gt_row gt_left">Spain</td>
<td headers="Group B  Points" class="gt_row gt_right">9</td>
<td headers="Group B  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/9.png" style="height:15px;"></td></tr>
    <tr><td headers="Group B  Nation" class="gt_row gt_left">Italy</td>
<td headers="Group B  Points" class="gt_row gt_right">6</td>
<td headers="Group B  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/768.png" style="height:15px;"></td></tr>
    <tr><td headers="Group B  Nation" class="gt_row gt_left">Croatia</td>
<td headers="Group B  Points" class="gt_row gt_right">3</td>
<td headers="Group B  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/3.png" style="height:15px;"></td></tr>
    <tr><td headers="Group B  Nation" class="gt_row gt_left">Albania</td>
<td headers="Group B  Points" class="gt_row gt_right">0</td>
<td headers="Group B  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/778.png" style="height:15px;"></td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="3" class="gt_group_heading" style="background-color: #E8EBED;" scope="colgroup" id="Group C">Group C</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Group C  Nation" class="gt_row gt_left">Slovenia</td>
<td headers="Group C  Points" class="gt_row gt_right">9</td>
<td headers="Group C  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/1091.png" style="height:15px;"></td></tr>
    <tr><td headers="Group C  Nation" class="gt_row gt_left">England</td>
<td headers="Group C  Points" class="gt_row gt_right">6</td>
<td headers="Group C  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/10.png" style="height:15px;"></td></tr>
    <tr><td headers="Group C  Nation" class="gt_row gt_left">Denmark</td>
<td headers="Group C  Points" class="gt_row gt_right">3</td>
<td headers="Group C  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/21.png" style="height:15px;"></td></tr>
    <tr><td headers="Group C  Nation" class="gt_row gt_left">Serbia</td>
<td headers="Group C  Points" class="gt_row gt_right">0</td>
<td headers="Group C  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/14.png" style="height:15px;"></td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="3" class="gt_group_heading" style="background-color: #E8EBED;" scope="colgroup" id="Group D">Group D</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Group D  Nation" class="gt_row gt_left">France</td>
<td headers="Group D  Points" class="gt_row gt_right">9</td>
<td headers="Group D  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/2.png" style="height:15px;"></td></tr>
    <tr><td headers="Group D  Nation" class="gt_row gt_left">Austria</td>
<td headers="Group D  Points" class="gt_row gt_right">6</td>
<td headers="Group D  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/775.png" style="height:15px;"></td></tr>
    <tr><td headers="Group D  Nation" class="gt_row gt_left">Netherlands</td>
<td headers="Group D  Points" class="gt_row gt_right">3</td>
<td headers="Group D  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/1118.png" style="height:15px;"></td></tr>
    <tr><td headers="Group D  Nation" class="gt_row gt_left">Poland</td>
<td headers="Group D  Points" class="gt_row gt_right">0</td>
<td headers="Group D  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/24.png" style="height:15px;"></td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="3" class="gt_group_heading" style="background-color: #E8EBED;" scope="colgroup" id="Group E">Group E</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Group E  Nation" class="gt_row gt_left">Belgium</td>
<td headers="Group E  Points" class="gt_row gt_right">9</td>
<td headers="Group E  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/1.png" style="height:15px;"></td></tr>
    <tr><td headers="Group E  Nation" class="gt_row gt_left">Ukraine</td>
<td headers="Group E  Points" class="gt_row gt_right">6</td>
<td headers="Group E  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/772.png" style="height:15px;"></td></tr>
    <tr><td headers="Group E  Nation" class="gt_row gt_left">Romania</td>
<td headers="Group E  Points" class="gt_row gt_right">3</td>
<td headers="Group E  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/774.png" style="height:15px;"></td></tr>
    <tr><td headers="Group E  Nation" class="gt_row gt_left">Slovakia</td>
<td headers="Group E  Points" class="gt_row gt_right">0</td>
<td headers="Group E  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/773.png" style="height:15px;"></td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="3" class="gt_group_heading" style="background-color: #E8EBED;" scope="colgroup" id="Group F">Group F</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Group F  Nation" class="gt_row gt_left">Portugal</td>
<td headers="Group F  Points" class="gt_row gt_right">9</td>
<td headers="Group F  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/27.png" style="height:15px;"></td></tr>
    <tr><td headers="Group F  Nation" class="gt_row gt_left">Czech Republic</td>
<td headers="Group F  Points" class="gt_row gt_right">6</td>
<td headers="Group F  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/770.png" style="height:15px;"></td></tr>
    <tr><td headers="Group F  Nation" class="gt_row gt_left">Georgia</td>
<td headers="Group F  Points" class="gt_row gt_right">3</td>
<td headers="Group F  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/1104.png" style="height:15px;"></td></tr>
    <tr><td headers="Group F  Nation" class="gt_row gt_left">Turkey</td>
<td headers="Group F  Points" class="gt_row gt_right">0</td>
<td headers="Group F  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/777.png" style="height:15px;"></td></tr>
  </tbody>
  
  
</table>
</div>

    #Create seed column that combines rank and group for KO plan
    ko_FIFA = read.csv('https://raw.githubusercontent.com/lucasweyrich958/EURO2024_Simulation/main/EURO2024_KO.csv')

    ranked_nations_FIFA$seed = paste0(ranked_nations_FIFA$Rank, ranked_nations_FIFA$Group, sep = "")
    ko_1_FIFA = ko_FIFA[1:8, ]
    ko_2_FIFA = ko_FIFA[9:nrow(ko_FIFA), 1:4] #Split Round of 16 with other rounds

    ko_1_FIFA = ko_1_FIFA %>%
      left_join(ranked_nations_FIFA, by = c("Home" = "seed")) %>%
      mutate(Home = Nation) #Merge Rof16 home teams based on seed

    ko_1_FIFA = ko_1_FIFA %>%
      left_join(ranked_nations_FIFA, by = c("Away" = "seed")) %>%
      mutate(Away = Nation.y) #Merge Rof16 away teams based on seed

    ko_1_FIFA = ko_1_FIFA[, 1:4]
    ko_FIFA = rbind(ko_1_FIFA, ko_2_FIFA) 
    ko_FIFA$Game = as.character(ko_FIFA$Game)

    round16_FIFA = ko_FIFA[1:8, ] #Split dataframe into each round
    quarterfinal_FIFA = ko_FIFA[9:12, ]
    semifinal_FIFA = ko_FIFA[13:14, ]
    final_FIFA = ko_FIFA[15, ]

    ko_result_FIFA = data.frame(Game = integer(), Home = character(), 
                           Away = character(), Winner = character(), 
                           stringsAsFactors = FALSE) #Initialize KO Result data frame

    simulate_ko_match = function(match, nation) {
      home_team = match$Home
      away_team = match$Away
      
      home_rank = nations[nations$Nation == home_team, ]
      away_rank = nations[nations$Nation == away_team, ]
      
      # Use random forest model to predict the outcome
      predicted_winner = ifelse(home_rank$FIFA_Rank < 
                                  away_rank$FIFA_Rank, home_team, away_team)
      
      # Update tournament results dataframe
      match_result = data.frame(Game = match$Game, Home = match$Home, Away = match$Away, Winner = predicted_winner)
      ko_result_FIFA <<- rbind(ko_result_FIFA, match_result)
    }

    #Loop through each KO round fill KO result data frame
    for (i in 1:nrow(round16_FIFA)) {
      simulate_ko_match(round16_FIFA[i, ], nations)
    }

    quarterfinal_FIFA = fill_ko_round(ko_result_FIFA, quarterfinal_FIFA)

    for (i in 1:nrow(quarterfinal_FIFA)) {
      simulate_ko_match(quarterfinal_FIFA[i, ], nations)
    }

    semifinal_FIFA = fill_ko_round(ko_result_FIFA, semifinal_FIFA)

    for (i in 1:nrow(semifinal_FIFA)) {
      simulate_ko_match(semifinal_FIFA[i, ], nations)
    }

    final_FIFA = fill_ko_round(ko_result_FIFA, final_FIFA)

    for (i in 1:nrow(final_FIFA)) {
      simulate_ko_match(final_FIFA[i, ], nations)
    }

    ko_result_FIFA$Round = c('Round of 16','Round of 16','Round of 16','Round of 16','Round of 16',
                        'Round of 16','Round of 16','Round of 16','Quarterfinal','Quarterfinal',
                        'Quarterfinal','Quarterfinal','Semifinal','Semifinal','Final') #Add round column manually

    ko_result_logo_FIFA = left_join(ko_result_FIFA, team_data_scrape %>% distinct(name, .keep_all = TRUE), 
                               by = c("Winner" = "name")) %>%
      select(Game, Home, Away, Winner, Round, logo) #Merge with logo from team_scrape data frame

    #Create table for KO round results
    ko_table_FIFA = gt(ko_result_logo_FIFA) %>%
      tab_row_group(
        label = "Final",
        rows = 15) %>% 
      tab_row_group(
        label = "Semifinals",
        rows = 13:14) %>% 
      tab_row_group(
        label = "Quarterfinals",
        rows = 9:12) %>%
      tab_row_group(
        label = "Round of 16",
        rows = 1:8) %>%
      cols_hide(c(Game, Round)) %>%
      gt_img_rows(columns = logo, img_source = "web", height = 15)
    ko_table_FIFA = tab_style(ko_table_FIFA, cell_fill('#bac2ca'), 
                         cells_column_labels())
    ko_table_FIFA = tab_style(ko_table_FIFA, cell_fill('#E8EBED'), 
                         cells_row_groups())
    ko_table_FIFA = tab_header(ko_table_FIFA, "EURO 2024 Knock-Out Round Results - FIFA Ranking")
    ko_table_FIFA

<div id="gsldqtkdbp" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#gsldqtkdbp table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#gsldqtkdbp thead, #gsldqtkdbp tbody, #gsldqtkdbp tfoot, #gsldqtkdbp tr, #gsldqtkdbp td, #gsldqtkdbp th {
  border-style: none;
}

#gsldqtkdbp p {
  margin: 0;
  padding: 0;
}

#gsldqtkdbp .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#gsldqtkdbp .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#gsldqtkdbp .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#gsldqtkdbp .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#gsldqtkdbp .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#gsldqtkdbp .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#gsldqtkdbp .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#gsldqtkdbp .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#gsldqtkdbp .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#gsldqtkdbp .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#gsldqtkdbp .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#gsldqtkdbp .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#gsldqtkdbp .gt_spanner_row {
  border-bottom-style: hidden;
}

#gsldqtkdbp .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#gsldqtkdbp .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#gsldqtkdbp .gt_from_md > :first-child {
  margin-top: 0;
}

#gsldqtkdbp .gt_from_md > :last-child {
  margin-bottom: 0;
}

#gsldqtkdbp .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#gsldqtkdbp .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#gsldqtkdbp .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#gsldqtkdbp .gt_row_group_first td {
  border-top-width: 2px;
}

#gsldqtkdbp .gt_row_group_first th {
  border-top-width: 2px;
}

#gsldqtkdbp .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#gsldqtkdbp .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#gsldqtkdbp .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#gsldqtkdbp .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#gsldqtkdbp .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#gsldqtkdbp .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#gsldqtkdbp .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#gsldqtkdbp .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#gsldqtkdbp .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#gsldqtkdbp .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#gsldqtkdbp .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#gsldqtkdbp .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#gsldqtkdbp .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#gsldqtkdbp .gt_left {
  text-align: left;
}

#gsldqtkdbp .gt_center {
  text-align: center;
}

#gsldqtkdbp .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#gsldqtkdbp .gt_font_normal {
  font-weight: normal;
}

#gsldqtkdbp .gt_font_bold {
  font-weight: bold;
}

#gsldqtkdbp .gt_font_italic {
  font-style: italic;
}

#gsldqtkdbp .gt_super {
  font-size: 65%;
}

#gsldqtkdbp .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#gsldqtkdbp .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#gsldqtkdbp .gt_indent_1 {
  text-indent: 5px;
}

#gsldqtkdbp .gt_indent_2 {
  text-indent: 10px;
}

#gsldqtkdbp .gt_indent_3 {
  text-indent: 15px;
}

#gsldqtkdbp .gt_indent_4 {
  text-indent: 20px;
}

#gsldqtkdbp .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="4" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>EURO 2024 Knock-Out Round Results - FIFA Ranking</td>
    </tr>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" style="background-color: #BAC2CA;" scope="col" id="Home">Home</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" style="background-color: #BAC2CA;" scope="col" id="Away">Away</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" style="background-color: #BAC2CA;" scope="col" id="Winner">Winner</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" style="background-color: #BAC2CA;" scope="col" id="logo">logo</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" style="background-color: #E8EBED;" scope="colgroup" id="Round of 16">Round of 16</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Round of 16  Home" class="gt_row gt_left">Hungary</td>
<td headers="Round of 16  Away" class="gt_row gt_left">Italy</td>
<td headers="Round of 16  Winner" class="gt_row gt_left">Italy</td>
<td headers="Round of 16  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/768.png" style="height:15px;"></td></tr>
    <tr><td headers="Round of 16  Home" class="gt_row gt_left">Germany</td>
<td headers="Round of 16  Away" class="gt_row gt_left">England</td>
<td headers="Round of 16  Winner" class="gt_row gt_left">England</td>
<td headers="Round of 16  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/10.png" style="height:15px;"></td></tr>
    <tr><td headers="Round of 16  Home" class="gt_row gt_left">Slovenia</td>
<td headers="Round of 16  Away" class="gt_row gt_left">Netherlands</td>
<td headers="Round of 16  Winner" class="gt_row gt_left">Netherlands</td>
<td headers="Round of 16  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/1118.png" style="height:15px;"></td></tr>
    <tr><td headers="Round of 16  Home" class="gt_row gt_left">Spain</td>
<td headers="Round of 16  Away" class="gt_row gt_left">Scotland</td>
<td headers="Round of 16  Winner" class="gt_row gt_left">Spain</td>
<td headers="Round of 16  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/9.png" style="height:15px;"></td></tr>
    <tr><td headers="Round of 16  Home" class="gt_row gt_left">Austria</td>
<td headers="Round of 16  Away" class="gt_row gt_left">Ukraine</td>
<td headers="Round of 16  Winner" class="gt_row gt_left">Ukraine</td>
<td headers="Round of 16  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/772.png" style="height:15px;"></td></tr>
    <tr><td headers="Round of 16  Home" class="gt_row gt_left">Portugal</td>
<td headers="Round of 16  Away" class="gt_row gt_left">Croatia</td>
<td headers="Round of 16  Winner" class="gt_row gt_left">Portugal</td>
<td headers="Round of 16  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/27.png" style="height:15px;"></td></tr>
    <tr><td headers="Round of 16  Home" class="gt_row gt_left">Belgium</td>
<td headers="Round of 16  Away" class="gt_row gt_left">Denmark</td>
<td headers="Round of 16  Winner" class="gt_row gt_left">Belgium</td>
<td headers="Round of 16  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/1.png" style="height:15px;"></td></tr>
    <tr><td headers="Round of 16  Home" class="gt_row gt_left">France</td>
<td headers="Round of 16  Away" class="gt_row gt_left">Czech Republic</td>
<td headers="Round of 16  Winner" class="gt_row gt_left">France</td>
<td headers="Round of 16  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/2.png" style="height:15px;"></td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" style="background-color: #E8EBED;" scope="colgroup" id="Quarterfinals">Quarterfinals</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Quarterfinals  Home" class="gt_row gt_left">Spain</td>
<td headers="Quarterfinals  Away" class="gt_row gt_left">England</td>
<td headers="Quarterfinals  Winner" class="gt_row gt_left">England</td>
<td headers="Quarterfinals  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/10.png" style="height:15px;"></td></tr>
    <tr><td headers="Quarterfinals  Home" class="gt_row gt_left">Portugal</td>
<td headers="Quarterfinals  Away" class="gt_row gt_left">Ukraine</td>
<td headers="Quarterfinals  Winner" class="gt_row gt_left">Portugal</td>
<td headers="Quarterfinals  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/27.png" style="height:15px;"></td></tr>
    <tr><td headers="Quarterfinals  Home" class="gt_row gt_left">Netherlands</td>
<td headers="Quarterfinals  Away" class="gt_row gt_left">Italy</td>
<td headers="Quarterfinals  Winner" class="gt_row gt_left">Netherlands</td>
<td headers="Quarterfinals  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/1118.png" style="height:15px;"></td></tr>
    <tr><td headers="Quarterfinals  Home" class="gt_row gt_left">Belgium</td>
<td headers="Quarterfinals  Away" class="gt_row gt_left">France</td>
<td headers="Quarterfinals  Winner" class="gt_row gt_left">France</td>
<td headers="Quarterfinals  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/2.png" style="height:15px;"></td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" style="background-color: #E8EBED;" scope="colgroup" id="Semifinals">Semifinals</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Semifinals  Home" class="gt_row gt_left">England</td>
<td headers="Semifinals  Away" class="gt_row gt_left">Portugal</td>
<td headers="Semifinals  Winner" class="gt_row gt_left">England</td>
<td headers="Semifinals  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/10.png" style="height:15px;"></td></tr>
    <tr><td headers="Semifinals  Home" class="gt_row gt_left">France</td>
<td headers="Semifinals  Away" class="gt_row gt_left">Netherlands</td>
<td headers="Semifinals  Winner" class="gt_row gt_left">France</td>
<td headers="Semifinals  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/2.png" style="height:15px;"></td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" style="background-color: #E8EBED;" scope="colgroup" id="Final">Final</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Final  Home" class="gt_row gt_left">England</td>
<td headers="Final  Away" class="gt_row gt_left">France</td>
<td headers="Final  Winner" class="gt_row gt_left">France</td>
<td headers="Final  logo" class="gt_row gt_left"><img src="https://media.api-sports.io/football/teams/2.png" style="height:15px;"></td></tr>
  </tbody>
  
  
</table>
</div>

Using only the FIFA rank, the predicted winner is France. This is
unsurprising, since France is ranked 2nd at the moment, making it
highest ranked EURO 2024 participant. While the KO-round looks
differently compared to the simulation above, the group phase seems to
look similar.

## Conclusion

This project aimed to simulate the UEFA European Championship 2024 and
predict the winner using a random forest regression model, trained with
historical team- and player-level data. The data was scraped from the
Football API, using functions systematically. First, team-level data
scraped, followed by player IDs based on the team rosters for each
season, followed by player-level data for the respective seasons.

The simulation predicted France vs. Portugal to play in the final, where
Portugal wins, therefore being the 2024 European champion.

Additionally, SHAP values were computed for the random forest model to
evaluate the impact of each feature, which showed interesting things,
such that high values of fouls committed predicting more wins. This may
suggest that a more aggressive and physical strategy is more successful
than passive. Although not without the risk of receiving yellow or even
red cards, then potentially receiving a suspension.

To validate the model further, a separate simulation was run using FIFA
rank as predictor, which showed similar results in the group phase but
different outcomes in the KO phase.

Lastly, a few limitations of this model. The target variable of total
wins was selected. This is an oversimplification of a soccer game, as
the outcome is decided by whoever has more goals scored. The SHAP values
showed that. Given the amount of data available for national teams, who
on average play 8 - 11 games per year, the random forest model did not
predict goals scored well. Using more data by players, possibly, would
allow to predict that better. Additionally, it was not possible to keep
the API alive due to costs, not allowing the model to be continuously
updated.

In sum, this project simulated the EURO 2024, predicting Portugal as a
winner. Now it’s time to set a bet for Portugal to win and enjoy
watching the EURO 2024. Even though my personal favorite is Germany this
year.
