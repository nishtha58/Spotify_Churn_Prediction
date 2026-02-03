Spotify User Churn Analysis & Prediction

**Project Overview**

This project analyzes and predicts user churn for Spotify using behavioral, demographic, and subscription data. The objective is to identify high-risk users, understand the key drivers of churn, and translate analytical insights into actionable business recommendations for improving user retention.
The project combines supervised and unsupervised learning techniques with a strong emphasis on interpretability and business deployment.

Note: This was a team-based academic project completed as part of the Data Science for Business course at Duke University, Fuqua School of Business.

**Data Description**

Source: Kaggle (synthetically generated dataset)
Unit of analysis: Individual Spotify users
Key feature groups:
Demographics: age, gender, country
Subscription: subscription type, device type, ads listened per week
Engagement: listening time, skip rate, songs played per day, offline listening
Because the data is synthetic, results are intended to demonstrate general behavioral patterns and modeling approaches, rather than exact real-world churn estimates

**Repository Structure**

Churn_Prediction_Rcode.R
1)End-to-end analysis pipeline
2)Data preparation, modeling, evaluation, and visualization

DataAnalyticsFunctions.R
Custom reusable R functions for:
  • Model performance evaluation
  • Metric calculation (AUC, RMSE, accuracy, log-loss)
  • Visualization and diagnostic plots

final_dataset.csv
1)Dataset used for analysis
2)Synthetically generated Spotify user data sourced from Kaggle

Project_Report.pdf
1)Detailed technical and business report

Churn_Prediction_Spotify.pdf
Summary Style presentation focused on insights and recommendations

README.md

**Team & Acknowledgements**

This project was a collaborative effort completed with fellow classmates as part of the Data Science for Business course at Duke Fuqua.
Team Members:
Weihan Liu, Sai Maradugu, Jannat Mubarik, Allie Sutter, Nishtha Wakankar
