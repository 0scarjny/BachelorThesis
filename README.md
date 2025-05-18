# BachelorThesis
Github repository containing the code and data used in my Bachelor Thesis.



## 📁 Code

### Preprocessing 
- [Dataset Merging](code/Dataset_merging_final.ipynb) - Notebook used to merge the datasets from the different sources
- [Feature Engineering](code/ML_preprocessing.ipynb) - Notebook used for the preprocessing of the dataset for the different models. Added lag features and temporal features

### Models
- [LightGBM](code/Forecast_lightgbm.ipynb) - Notebook used for training and forecasting of the LightGBM model.
- [LSTM](code/Forecast_lstm.ipynb) - Notebook used for training and forecasting of the LSTM model.
- [SARIMA]code/SARIMA_forecasts.R) -R code used for training and forecasting of the SARIMA model.

### ANALYSIS
- [Data Exploration](code/eda.ipynb) - Notebook used for the creation of some of the plots as well as for analyisng the data.
- [Results Analysis](code/results.ipynb) - Notebook used for the comparison of the model performance. Include the code for the Diebold-Mariano test.



## 📊 Data
- [Dataset](data/data.csv) - Final dataset used in the analysis in csv format.
- [Predictions](data/combined_predictions.csv) - One step-ahead predictions and actual value for the test period.


