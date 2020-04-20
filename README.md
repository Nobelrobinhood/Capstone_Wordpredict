# R-Capstone
## Coursera - Data Science Specialization Capstone
* Author:  Kolesnikov Sergey
* Date: 29.09.2016  
* [Email](mailto:scitator@gmail.com)  
* [GitHub url](https://github.com/Scitator/R-Capstone)
* [Presentation url](http://rpubs.com/Scitator/NextWordPredictionApp)
* [App](https://scitator.shinyapps.io/NextWordPredictionApp)

## Coursera Data Science Capstone Project
The Coursera Data Science Specialization Capstone project from Johns Hopkins University (JHU) allows 
students to create a usable public data product that can show their skills to potential 
employers. Projects are drawn from real-world problems and are conducted with industry, government, 
and academic partners. In this part of course JHU is partnering with SwiftKey 
(https://swiftkey.com/) to apply data science in the area of natural language processing.

## Next word prediction app (using N-gram models)
- based on N-gram model with "Stupid Backoff" ([Brants et al 2007](http://www.cs.columbia.edu/~smaskey/CS6998-0412/supportmaterial/langmodel_mapreduce.pdf))
- checks if highest-order (in this case, n=4) n-gram has been seen. If not "degrades" to a lower-order model (n=3, 2)
- build on `cracklib-small` dictionary

For additional prediction accuracy were also used:
- Good-Turing Smoothing
- MLE
- linear-regression
- linguistic tweaks

## Dataset
The underlying dataset for this word prediction app was gathered from three sources:

* Blogs
* News
* Twitter

The original english corpus combined over 580 MB of language information. Which summed up to over half a billion characters. After processing the data our model consists out of **millions of ngram tokens**.

### The project consisted on several parts:
  1. Getting the data
  2. Data splitting
  3. Data cleaning and tokenization
  4. Building n-gram model
  5. Building predictive model
  6. Building shiny app
  
### How to run the code:
- Simply run shipy app server from `shiny` dir.

- Or build your own prediction model first. In order to do this just go to the "scripts" folder, configure "config.r" and run "run.r". This will take a while.

### Additional information
All R scripts, files, presentations etc. in this repository are materials for the capstone project of the Coursera Data Science specialization held by professors of the Johns Hopkins University and in cooperation with SwiftKey.

- Learn more about the Coursera Data Science Specialization: [https://www.coursera.org/specialization/jhudatascience](https://www.coursera.org/specialization/jhudatascience)