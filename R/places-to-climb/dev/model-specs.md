## Goal: 
* Build quarto app (app.qmd) using R to find fun places to climb
* Design should use common R packages for dashboard building
    * bslib cards should be used for the ability to expand cards to full screen
* Ideally, app should work for mobile but code should not be overly complicated to do so

## Design:
Dashboard:
* sidebar:
    * checkbox group buttons to indicate if can be used for boulder, sport, top rope, trad
    * slider for `avg_stars`
    * slider of `grade_int`
    * switch to aggregate points or not

* main area:
    * map (left): location of climbing routes (x, y) 
        * tooltip for individual points (x, y) should show the route name, grade, and regions 1-3
        * if aggregation is active, points should aggregate when zoomed out and show individual points when zoomed in. The aggregation when zoomed out should progressively go from region_1 to region_2 to region_3 to route_id
    * bar chart (right-top):
        * an expandable plotly bar graph:
            * drilldown area > region_1 > region_2 > region_3
            * bars colored by `simple_grade` (see below)
    * data table (right-bottom):
        * display list of routes and star rating

* Interaction:
    * double clicking on bar
    * clicking map should filter bar chart to region_3 and individual point should highlight appropriate section of chart
    * clicking bar should filter map to region_3
    * if possible, map should be able to locate user:
        ```
        tags$script('
            navigator.geolocation.getCurrentPosition(function(position) {
                Shiny.onInputChange("lat", position.coords.latitude);
                Shiny.onInputChange("long", position.coords.longitude);
            });
        ')
        ```
            
  

## Data dictionary:
The data `crags/areas/all_routes.csv` comes from mountain project with these fields:
* area = state
* route_id = unique id of route
* route = route name
* url = link to mountain project
* avg_stars = rating by other users
* grade = most detailed climibing grade V1+ or 5.10a/b
* grade_int = grade as integer V0 = 0, 5.10a = 10
* grade_simple = grade V1+ is V1, 5.10a/b PG13 is 5.10a
* pitches = # of pitches in a multipitch climb
* length = length in feet of pitch/boulder
* x = longitude
* y = latitude
* region_1 = After state (area) next biggest geographic grouper
* region_2 = After region_1 next biggest geographic grouper
* region_3 = After region_3 next biggest geographic grouper
* description = text description of route
* protection = describes # of bolts, type of anchor, crash pad needed, etc
* bad_tags = comma separated list of less desirable features: choss, scary, risky, run-out 
* feature_tags = comma separated list of rock features: arete, slab, dihedral, sloper
* skill = comma separated list of skills used: heel hook, mantle, dyno, etc
* good_tags = comma separated list of desirable features: fun, interesting, scenic, beatuiful
* bad_ind = indicates that route has bad tags
* good_ind = indicates that route has good tags
* fun_ind = indicates that route has tags indicating fun
* pretty_ind = indicates that route has tags describing nice views
* boulder_ind = indicates route can be used as a boulder
* rope_ind = indicates route can be used for ropes (top rope, trad, sport)
* trad_ind = indicates route is used for trad climbing
* sport_ind = indicates route is used for sport climbing
* tr_ind = indicates route is used for top rope climbing

## Grade colors:
Color by `grade_simple`:
* 5.3 - dark green
* 5.4 - dark green
* 5.5 - dark green
* 5.7 - dark green
* 5.8 - medium green
* 5.9 - light green
* 5.10 - dark blue
* 5.10a - dark blue
* 5.10b - medium blue
* 5.10c - light blue
* 5.10d - light blue
* 5.11 - dark orange
* 5.11a - dark orange
* 5.11b - medium orange
* 5.11c - light orange
* 5.11d - light orange
* 5.12 - red
* 5.12b - red
* 5.14a - red
* v0 - dark green
* v1 - medium green
* v2 - light green
* v3 - dark blue
* v4 - medium blue 
* v5 - light blue
* v6 - dark orange
* v7 - medium orange
* v8 - light orange
* v9 - red
* v10 - red
* v11 - red
* v12 - red
* v13 - red