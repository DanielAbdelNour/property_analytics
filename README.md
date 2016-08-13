# Property Analytics Project

## About the project
I wrote this tool to help me in finding a suitable home to buy by analysing various suburb statistics and training a number of machine learning algorithms to predict the likely sale price, and capital gain potential of advertised properties.

## Features
* Transform unstructured lines of text into a structured dataframe with a row for each property with attributes corrosponding to sale price, date, number of rooms, number of bathrooms, land size, building size, street name, garage spaces, and number of ensuites.
* Predict a suburbs growth by cross validating various machine learning algorithms including Random Forests, Neural Networks, and XGBoost. Each algorithm's accuracy is tested against a test set of the suburb's profile
* Predict a house's sale price given user defined inputs such as number of rooms, bathrooms, ensuites, land size, building size, suburb, etc.
* Predict and suggest the best method of increasing a property's value by calculating how significant various attributes are in affecting sale price

## WIP
* Add a visual layer to the tool using Shiny and Plotly
* Optimise algorithm selection
* Comment and clean code
