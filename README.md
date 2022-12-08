# ECO365-Project
Final project for eco365

To download the source code click on <>Code then download zip. 

***DO NOT SKIP THIS STEP: Extract the files all files from zip file. Then open the "ECO365 Final project.Rproj" file from the extracted folder; Opening the Rproj file will open the directory needed to continue the project without issue.***

This Project was meant to be used as an Analysis of trends using the Twitter Api. By default once the program runs it will set that project folder as the working directory, thus making it easier to compile the dashboard.


**IT IS IMPORTANT BEFORE THE USER DOES ANYTHING WITH THIS PROJECT THAT THEY RUN THE CODE TO INSTALL ALL NECESSARY PACKAGES BEFORE PROCEEDING OTHERWISE THE PROJECT WILL RETURN ERRORS. ONCE THE USER INSTALLS ALL THE NECESSARY PACKAGES PROVIDED TO THEM ON LINES 4-6 ON "FINAL PROJECT.R" FILE, THE USER SHOULD THEN EITHER DELETE OR COMMENT THOSE LINES OF CODE OUT TO ENSURE EASY RE-USE OF THE PROGRAM.
*************************************************************************************************************************************************************


There are two ways to use this program. 

First and the best way is to open the dashboards.Rmd file within the zip using Rstudio. Once opened, all you need to do is click Knit, and it should automatically run the whole code and turn it into an HTML file which can now be analyzed and interacted with. Take note that the time the run time of the code varies based on the users computer specs and how many tweets are being extracted. 

The user of this program is able to go into the source code "Final Project.R" to change the parameters of your search. The default parameterized search is 15,000 tweets. At its current stage, the program will handle up to 18,000 tweets in a 15 minute period. That being said, the code is built to sustain higher numbers of tweets should the user wish to make the search larger than 18,000 tweets. However, the run time of the program will be significantly slowed should the user choose to extract more than 18,000 tweets. 

In addition, the default search term for this program is based off the current highest trending phrase or hashtag word on twitter at the moment the user runs the script. To change the search term go onto line 103 and 106, then add a string literal in place of the word Lookup. *NOTE: A string literal is a word or phrase that is surrounded by parenthesis.* 

The second way to run this program is for the user to do it straight from the source file "Final project.R" and run the project line by line. It is advised that the user run the source file line by line to better help them understand how to program works. Concurrently, the user will see how the data is transformed in the global environment, and see the visualizations created one at a time as they proceed through the code. 

If the user chooses to run the code in the source file "Final project.R" all at once, it will compile the whole project. In doing so, the user will have to go into their viewer tab and use the back arrow to cycle through the visualizations which is not in its intended order. Additionally, the user will have to click on the plots tab to view the word cloud visualization as it is a separate image. This is why it is best to run the program line by line, or through knitting the "dashboard.Rmd" file.

This program has many visualization tools that allows the user with natural language processing. The first visualization presented is a time series chart that shows the timeframe from which the tweets were extracted from, and how many tweets were made per hour. The second visualization is a vertical barchart with aggregated sentiment values of negative neutral and positive. With this visualization you can see how the sentiment of the extracted tweets are. Subsequently, the next visualization is a histogram which shows the assigned value distributions on the tweets. Tweets were scored based on an algorigthm that uses lexical data to determine the overall value of each tweet. Next is a Word cloud that shows the top 100 words used in the tweets based on their frequency. Afterwards, a chart is created to see the top ten emojis used within all collective tweets. To see how the scores are better distributed, the next visualization is the top ten frequency of both positive and negative words. Lastly is a frequency chart for the most frequent hashtags that are associated with the original keyword searched. This allows the user to rerun the program with new insight on what to search for additional information on the trend.

If there are any issues with authentication, you are able to click this link below to see the dashboard with previously extracted data:
https://rpubs.com/Jbossone4/981127
