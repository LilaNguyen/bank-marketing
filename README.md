# bank-marketing

| Member              | Email                      |
| ------------------- | -------------------------- |
| Lila Nguyen         | lila.nguyen@sjsu.edu       |
| Tien Nguyen         | tien.b.nguyen@sjsu.edu     |
| Jason Pham          | jason.pham01@sjsu.edu      |
| Tirth Thakkar       | tirth.thakkar@sjsu.edu     |

---

## Project Overview

This project analyzes the [Bank Marketing dataset](https://archive.ics.uci.edu/dataset/222/bank+marketing) to investigate factors associated with client subscription outcomes during direct marketing campaigns conducted by a Portuguese banking institution.

The analysis includes:
- Data cleaning and preprocessing
- Descriptive statistics
- Exploratory Data Analysis (EDA)
- Visualization of trends and relationships among variables
- Hypothesis testing, Chi Square test, and t-test
- Logistic Regression modeling and evaluation

---

## Dataset

Dataset:
- Bank Marketing Dataset
- Source: UCI Machine Learning Repository

The dataset contains information related to:
- Client demographics
- Contact history
- Campaign interactions
- Macroeconomic indicators
- Previous campaign outcomes

### Dataset Dimensions

- 41,188 observations
- 21 variables

---

## Project Structure

```text
bank-marketing/
│
├── data/
│   └── bank-additional-full.csv
│
├── plots/
│   ├── bar_education.png
│   ├── bar_job.png
│   ├── bar_month.png
│   └── ...
│
├── scripts/
│   └── 02_hypothesis_testing
│   └── 01_data_cleaning_and_eda.R
│
├── README.md
└── .gitignore
