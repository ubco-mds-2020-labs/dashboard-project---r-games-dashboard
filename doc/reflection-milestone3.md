## Done so far
After transferring the dashboard from the Python version to the R version, we decided to continue building the R version. This was because we felt Plotly had a better feel and was easier to work with than Altair. On top of fully transferring the features in the Python version, we have added additional filter (Genre, Platform, Publisher and Year) on the side bar. Our plots are now positioned in the tab we saw most appropriate. We also introduced Text based summaries on Tab 3. 

## To do
Things we still need to do:
* Add descriptors for: Sidebar (How to use, Dataset), Tabs (Explanation)
* New title plots
* Outlines for Tabs 2/3 (Put into cards)
* Change plot colours
* Hook up plot 4 and 5 (Tab 2, 2nd plot and Tab 3) to filters on side bar
* Fix plot 5 - tab 3 (tooltip as well as ordering of Genres from greatest to least)
* Move plot 5 - tab 3 next to the cards
* Certain combination of filters results in no data but the plots still display the last visualization 

## Limitations
The underlying data is very limited, as it doesn't allow us to view more recent information or sales numbers in currency, only in units sold.
We looked into scraping updated numbers from https://www.vgchartz.com/ but due to the current state of the website (very slow), this does not seem possible. For example, loading a page on the website takes approximately 15 seconds. With this estimate, it would take approximately 4 days to re-scrape out data. 

