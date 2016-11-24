# shiny-pagespeed-graphing
This is a shiny application (R, bootstrap js and css combination). It graphs the google's pagespeed metrics for a website. The data was saved off to a database and then served back as a service (java app in Mule). The application calls the Mule service endpoints and parses the json data to R plots. 

The user can choose a number of days back to graph. The information being graphed in the density graphs are the bounce rates for users and user activity. The lower graphs show how many resources make up the page itself - js, css, html files and the like. Two images are included to see what the code is suppose to return.
