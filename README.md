# 📈 Weekly Appointments Forecasting – STAT 370 Final Project

Forecasting **weekly attendance** and **cancellations** for MacEwan University’s Career Office using classical **time-series models** (AR, MA, ARMA).  
This project demonstrates the complete workflow of data preparation, exploratory analysis, model selection, diagnostics, and forecast interpretation.

---

## 🎯 Project Objective

The goal of this study is to model and predict weekly engagement patterns at the Career Office to help plan staffing, appointment slots, and outreach.  
We apply and compare AR(1), MA(1), and ARMA(1,1) family models to identify the most reliable short-term forecasting approach.

---

## 🧾 Dataset Overview

| Feature | Description |
|----------|--------------|
| **Time Range** | Sept 2023 → Sept 2024 (52 weeks) |
| **Variables** | Weekly Attendance, Weekly Cancellations |
| **Unit** | Number of appointments per week |
| **Source** | Career Office administrative logs (MacEwan University) |
| **Pre-processing** | Aggregated daily logs → weekly totals; numbered weeks (Week 35–Week 86) |

The dataset was cleaned, summarized by week, and analyzed separately for attendance and cancellations.

---

## 🔍 Exploratory Analysis

- **Autocorrelation:** Weak overall but noticeable short-term dependency (lag ≈ 4).  
- **Seasonality:** Minimal seasonal repetition within one-year data window.  
- **Distribution:** Cancellations right-skewed → log transformation applied.  
- **Outliers:** Week 41 detected as structural outlier (possible event disruption).

---

## 🧠 Modeling Approach

| Series | Candidate Models | Selection Criteria |
|---------|------------------|--------------------|
| **Attendance** | AR(1), MA(1), ARMA(1,1) | AIC, BIC, Residual Diagnostics |
| **Cancellations (log)** | MA(3), ARMA(1,1), ARMA(2,1) | AIC, MSE, MAE |

### ✅ Best-Fit Models
- **Attendance:** MA(1) — lowest MSE/MAE, balanced residuals.  
- **Cancellations:** ARMA(1,1) — best performance after log transform and outlier adjustment.

---

## 📊 Model Diagnostics

- Residuals centered near 0 with no clear autocorrelation.  
- Q-Q and histogram plots confirm approximate normality.  
- Forecast confidence intervals widen appropriately with horizon length, indicating stable uncertainty estimates.

---

## 📈 Forecast Insights

- **Attendance Forecast:** Weekly attendance expected to remain stable with minor fluctuations (~±10 appointments).  
- **Cancellations Forecast:** Slight downward trend post-Week 45, potentially reflecting improved scheduling discipline.  
- **Operational Use:** Reliable 3–4 week outlook for resource allocation and reminder campaigns.

---

## ⚙️ Technologies & Libraries

- **Language:** R  
- **Packages:** `tseries`, `forecast`, `stats`, `ggplot2`, `TSA`  
- **Environment:** RStudio / Jupyter with IRkernel  

---

## ⚠️ Limitations

- One academic year (52 weeks) limits detection of annual seasonality.  
- Forecast horizon beyond 6 weeks becomes uncertain due to limited lag depth.  
- Cancellations required log transformation and manual outlier correction.  
- Findings specific to Career Office; generalization to other departments may vary.

---

## 💡 Future Work

- Extend dataset to multiple years for seasonal modeling (SARIMA).  
- Compare classical models with machine-learning approaches (Prophet, LSTM).  
- Automate AO/IO outlier detection and data cleaning pipeline.  
- Integrate additional predictors such as exam periods or weather events.

---

## 👩🏽‍💻 Authors

**Paramdeep Nijjer**, Anandika Aggarwal, Avishek Paudel  
📍 *STAT 370 – Applied Time Series Analysis, MacEwan University*  
🗓️ Fall 

---

## 📄 License
MIT License — freely available for educational and research use.

---

⭐ *If this project helped you learn time-series modeling, give it a star on GitHub!*
