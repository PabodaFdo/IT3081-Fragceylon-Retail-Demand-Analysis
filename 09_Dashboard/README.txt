TASK 10 - FRAGCEYLON R SHINY DASHBOARD
=======================================

FILES
-----
app.R
    Main R Shiny dashboard.

00_install_dashboard_packages.R
    Installs the packages required by app.R.

FOLDER LOCATION
---------------
Place both files inside:

09_Dashboard/

Recommended final structure:

09_Dashboard/
├── app.R
├── 00_install_dashboard_packages.R
└── Screenshots/

HOW TO RUN
----------
1. Open the project in RStudio.
2. If required, run:
   09_Dashboard/00_install_dashboard_packages.R
3. Open:
   09_Dashboard/app.R
4. Click:
   Run App

The dashboard reads:
04_Cleaned_Data/Fragceylon_Regular_Orders.xlsx

DASHBOARD PAGES
---------------
1. Executive Overview
2. Product Demand
3. Seasonal Analysis
4. Channel & Market
5. Predictive Insights
6. Decision Support
7. About & Limitations

IMPORTANT LIMITATION
--------------------
The dashboard does NOT calculate exact reorder quantities.

The current dataset does not include complete:
- inventory-on-hand history
- supplier lead time
- stock-out history
- purchase/restock quantity
- holding cost
- service-level targets

Decision-support flags are monitoring indicators only.

SCREENSHOTS TO SAVE FOR REPORT
------------------------------
After the app runs, create the folder:

09_Dashboard/Screenshots/

Recommended screenshots:
1. Task10_01_Executive_Overview.png
2. Task10_02_Product_Demand.png
3. Task10_03_Seasonal_Analysis.png
4. Task10_04_Channel_Market.png
5. Task10_05_Predictive_Insights.png
6. Task10_06_Decision_Support.png

These can later be added to the Task 10 report and presentation.
