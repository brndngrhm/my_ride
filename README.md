### **About**

#### **What is this app for? Why should I use this instead of something like Strava, Training Peaks, or Garmin Connect?**  
The free versions of these apps aren't very helpful for analyzing intervals. While they can tell you your overall average power, heart rate or cadence for a ride it's harder if not impossible to get at the average *during an interval or hard effort*, which is probably what you care about more so than the overall average. **This app automatically identifies interval efforts and displays the average power, heart rate, cadence, or speed in these intervals.**
<br>  

#### **How do I use the chart? What are the green and blue lines telling me?**  
Let's say you want to analyze your power during a recent training ride. You upload a file, and select "Power" and a chart appears. The Green Line is showing you your average power at each minute of your ride. The Blue Line is detects changes in your effort and tells you the average power within each detected change point. It is a result of a [changepoint detection algorithm](http://members.cbio.mines-paristech.fr/~thocking/change-tutorial/RK-CptWorkshop.html), the sensitivity of which you can control with the slider in the sidebar.
<br>  

#### **Ok but what if the Blue Line isn't lining up with my intervals?**
The Blue Line tries to detect changes automatically, but can sometimes miss them. The good news is you can adjust the number of changepoints to search for with the slider - the lower the number the less sensitive and the higher the number the more sensitive it becomes. The goal is to strike a balance, where the blue line changes in step with your your intervals but doesn't change *too much* to the point where it reacts to every small fluctuation within an interval. Usually setting it somewhere between 10 and 20 should do the trick.
<br>  

#### **Why do I need to specify if I used a power meter or not??**
The .tcx files are structured differently when you use a power meter than when you don't. You have to select the Yes or No so the app processes the data correctly. 
<br>  

#### **Why do I need to upload a file at all? Why can't this connect to Strava or Garmin Connect??**
This might be possible in the future! Currently I don't know how to extract anything other than ride summary statistics from Strava and Garmin Connect's API has an insane fee of $5,000.

[How to download a .tcx file from Garmin Connect](https://support.strava.com/hc/en-us/articles/216917807-Exporting-files-from-Garmin-Connect)

[How to download a .tcx file from Strava](https://support.strava.com/hc/en-us/articles/216918437-Exporting-your-Data-and-Bulk-Export#TCX)
