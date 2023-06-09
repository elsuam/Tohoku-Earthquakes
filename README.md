# Tohoku-Earthquakes
*This is an app built to understand and develop in Shiny as the app gets more and more complex.  Originally a stem from my Master's thesis,
I update it to include new ideas and improve its performance as I learn more about Shiny.  As the user will see, the models surveyed are
not particularly appropriate for the task of extrapolating extreme results.  Rather, they are meant show some differences between traditional machine learning models and neural networks.*

The Fukushima nuclear reactor in Japan was built to withstand earthquakes up to a magnitude 8.6.  An earthquake of such magnitude had not been recorded in the area; nor was it ever expected to.  However, on March 11, 2011 a magnitude 9.1 hit off the east coast of the Tohoku Region of Japan, resulting in a tsunami that caused the Fukushima Daiichi Nuclear Disaster and around 19,500 total deaths.  The earthquake is one of five of the world's largest recorded since modern record keeping.

This app is not suggestive of how decision makers concluded the frequency of catastrophic earthquakes in real time.  Rather, it offers a real-world example of how one *could* justify their predictions of such.  Earthquakes, especially those of considerable magnitude, occur infrequently and thus data for predicting them is sparse.
Using historical data from the USGS' Earthquake Catalog (ComCat), the average annual frequencies of earthquakes in the area of relative magnitude are plotted,
dating back as far as 1965 up to the most recent earthquake *before* the Great Earthquake that devastated the nation.  The app allows the user to make predictions based
on varying levels of model capacity, ultimately showing how a model that is overfit does not make useful predictions.


### Information on the Earthquake:
https://world-nuclear.org/information-library/safety-and-security/safety-of-plants/fukushima-daiichi-accident.aspx
https://www.history.com/this-day-in-history/fukushima-nuclear-disaster-japan

### ComCat Search Engine:
https://earthquake.usgs.gov/earthquakes/search/
