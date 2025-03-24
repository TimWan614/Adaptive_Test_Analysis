# Adaptive Test Analysis using IRT

This repository contains the code and analysis for an **Item Response Theory (IRT) calibration** of an adaptive English proficiency test dataset. The project evaluates item difficulty, discrimination, and model selection using **1PL (Rasch) and 2PL IRT models**.

---

## 📌 Project Overview
The goal of this analysis is to examine the psychometric properties of test items and assess the **adaptive algorithm's effectiveness**. The analysis follows these key steps:
- **Data Cleaning & Preprocessing** (removal of parent items, wide-format transformation)
- **Item Calibration using IRT** (1PL vs. 2PL model comparison)
- **Model Selection** using **AIC/BIC**
- **Item Characteristic Curve (ICC) Analysis** for flagged items (e.g., negative discrimination, extreme difficulty)
- **Reliability Analysis** using **Cronbach’s Alpha**
- **Comparison of Raw Difficulty vs. IRT Difficulty**

---

## 📂 Repository Structure

📁 `Data/` → Folder to store the dataset (not included for privacy)  
📄 `IRTModel.R` → Performs **IRT model fitting**, **ICC plotting**, and **difficulty comparisons**  
📄 `Model Selection.R` → Runs **1PL vs. 2PL model selection** using **AIC/BIC**  
📄 `README.md` → This file  

---

## 🚀 How to Run the Code

1️⃣ Clone this repository:
```sh
git clone https://github.com/TimWan614/Adaptive_Test_Analysis.git
cd Adaptive_Test_Analysis
```
2️⃣ Ensure you have the required R libraries installed:
```r
install.packages(c("ltm", "dplyr", "tidyr", "ggplot2", "psych"))
```
3️⃣ Place the dataset (`Simulated_Adaptive_Test_Data.csv`) inside the `/Data` folder  
4️⃣ Run the **IRT model analysis** using:
```r
source("IRTModel.R")
```
5️⃣ Run the **model selection script**:
```r
source("Model Selection.R")
```

---

## 📊 Key Findings
- The **2PL model** was preferred over 1PL based on **AIC/BIC values**
- Items with **negative discrimination** were flagged for review
- The correlation between **IRT difficulty & raw difficulty improved** after **outlier removal**
- **Cronbach’s Alpha = 0.82**, indicating good internal consistency

---

## 🤖 Future Enhancements
- **Machine Learning** integration to improve item selection
- **Automated flagging** of problematic test items
- **Bayesian adaptive testing models** for more dynamic adjustments

---

## 🔗 Author
👤 **Siu Tim Wan**  
📧 Contact: s.wan2@aston.ac.uk
🔗 GitHub: [https://github.com/TimWan614](https://github.com/TimWan614)  

