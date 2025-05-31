# Video Game Sales Dashboard - Deployment Guide

## Files

1. **app.R** - The main Shiny application (code provided above)
2. **vgsales.csv** - Your video game sales dataset

## Deployment to shinyapps.io

### 1. Install Required Packages
```r
# Install deployment package
install.packages("rsconnect")

# Install all required packages for the app
install.packages(c("shiny", "shinydashboard", "DT", "plotly", 
                   "dplyr", "ggplot2", "reshape2", "RColorBrewer", "viridis"))
```

### 2. Set Up Your Account
```r
library(rsconnect)

# Configure your account 
rsconnect::setAccountInfo(...)
```

### 3. Prepare Your Files
Create a folder with these files:
- `app.R` (containing the dashboard code)
- `vgsales.csv` (your dataset)

### 4. Deploy the Application
```r
# Set working directory to your app folder
setwd("path/to/your/app/folder")

# Deploy the app
rsconnect::deployApp(appName = "video-game-sales-dashboard")
```


## Dashboard Features

### 📊 Charts Included (13+ charts, 7+ chart types):

**Overview Tab:**
- Bar Chart: Global Sales by Genre
- Pie Chart: Market Share by Region
- Histogram: Sales Distribution Overview

**Regional Analysis Tab:**
- Heatmap: Regional Genre Preferences
- Scatter Plot: Regional Sales Comparison
- Bar Chart: Genre Preferences by Region

**Time Trends Tab:**
- Line Chart: Sales Trends Over Time
- Area Chart: Genre Evolution Over Decades

**Platform Analysis Tab:**
- Bar Chart: Top Platforms by Sales
- Pie Chart: Platform Market Share
- Line Chart: Platform Popularity Timeline

**Top Performers Tab:**
- Bar Chart: Top 20 Games by Global Sales
- Bar Chart: Top Publishers
- Bar Chart: Regional Champions

**Data Explorer Tab:**
- Interactive Data Table

### 🎯 Key Research Questions Answered:

1. **Regional Genre Preferences**: Which genres are popular in different regions?
2. **Temporal Trends**: How have gaming preferences changed over time?
3. **Platform Evolution**: Which gaming platforms dominated different eras?
4. **Market Leaders**: Who are the top games, publishers, and regional champions?
5. **Sales Patterns**: What are the distribution patterns of video game sales?

### 🎮 Interactive Features:

- **Region Comparison**: Compare sales between different regions
- **Genre Selection**: Focus on specific genres
- **Time Period Filtering**: Analyze different time periods
- **Regional Champions**: See top games in specific regions
- **Data Exploration**: Browse the full dataset
