# Text Analysis with @WeAreRLadies

This repo contains materials from two presentations where we analyzed the text from @WeAreRLadies' tweets:    
  
 - A [talk](https://www.meetup.com/rladies-chicago/events/263406265/) presented to [R-Ladies Chicago](https://rladieschicago.org/) on August 14, 2019  
 - A lightning talk presented at [rstudio::conf(2020)](https://rstudio.com/conference/) on January 30, 2020  
    

The script follows tutorials from [Text Mining with R](https://www.tidytextmining.com/).  
  
  
[@WeAreRLadies](https://twitter.com/WeAreRLadies) is the rotating curation account of [R-Ladies Global](https://rladies.org/).     
  
    
    
## The Data  
  
There are two data files (one used for each talk):  
  
 - `rocur_tweets_thru_June2019.csv` was used for the talk at R-Ladies Chicago  
 - `rocur_tweets_thru_Dec2019.csv` was used at rstudio::conf(2020)    

  
 The data files have the following columns:  
  
- Tweet.id  
- Tweet.permalink  
- Tweet.text  
- date  
- time_only  
- time (timestamp with both date and time)  
- impressions  
- engagements  
- engagement.rate (calculated as engagement / impressions)  
- retweets  
- replies  
- likes  
- Start (curator start date)  
- End (curator end date)  
- Curator (curator name)  
- Twitter (curator twitter handle)  
- Student (self-reported student status, 1=Student; 0=Not a student)  
  
