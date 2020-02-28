#!/bin/bash

## Install Kaggle
pip3 install --user kaggle

## Set Kaggle in PATH
export PATH="~/.local/bin:$PATH"
source ~/.bashrc

# Download Kaggle Data
kaggle competitions download favorita-grocery-sales-forecasting -p Data/Raw_Data
kaggle competitions download recruit-restaurant-visitor-forecasting -p Data/Raw_Data
kaggle competitions download rossmann-store-sales -p Data/Raw_Data
kaggle competitions download walmart-recruiting-store-sales-forecasting -p Data/Raw_Data
kaggle competitions download walmart-recruiting-sales-in-stormy-weather -p Data/Raw_Data
kaggle competitions download web-traffic-time-series-forecasting -p Data/Raw_Data