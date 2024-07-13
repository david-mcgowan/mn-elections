# MN 2022 Election Results App

## by David McGowan

This will be a Shiny app for anyone interested in exploring the results of the 2022 midterm elections in Minnesota. Instead of having to scour the Internet in search of results for multiple elections, users will be able to see all election results in one place!

This app will use election data available from the MN Secretary of State's website. All data comes within a very big spreadsheet, and some shapefiles are also available, which I may or may not use.

Right now, I'm envisioning a multiple-tab Shiny app, where **one tab** looks at statewide races: the user can select one of the statewide positions, and then they'll see a table showing the votes earned by all major candidates, alongside a map of Minnesota, with counties colored by their candidate preference. **Another tab** will show state legislative election results: the user can select a district and, as before, be presented with a table of results and hopefully a map of the district, with precincts shaded appropriately. Since there are so many legislative districts, I might include a selector input, where the user can choose to be presented with metro districts, non-metro districts, or all districts. Also, maybe I'll divide this into one tab for state senate seats and another tab for state house seats.

This project won't involve automation, since the 2022 election data won't be changing anytime soon... But there will be plenty of interactivity, as described above. This will involve choosing districts and races, and an option for what districts to be presented as options. Maybe I could also include an option to remove any candidates who are not Republicans or DFLers.

Working with the big spreadsheet of precinct results might take some time, but I don't think it'll be too difficult *per se*. Same with the construction of the Shiny app -- I feel confident about that. I think the big challenge for me will be making maps -- especially if I decide to try including maps of state legislative districts! And troubleshooting these maps will be especially difficult since they're part of a Shiny app...

No division of labor is involved, since I'm taking on this project alone.