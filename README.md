# RetailScope: An Interactive E-Commerce Sales Dashboard 🚀

Welcome to **RetailScope**!  
This project is an **R package** that builds and launches a user-friendly **Shiny dashboard** to explore a large e-commerce dataset.  

I built this as a **portfolio piece** to showcase my skills in creating clean, reusable R code and turning complex data into an interactive and insightful tool.  

---

## Features

- **Interactive Dashboard** – Explore sales trends, filter data by **date range** and **state**, and switch between **total revenue** and **number of orders**.  
- **Clean & Reusable Code** – Data loading and cleaning are wrapped into functions, making the workflow neat and reproducible.  
- **Self-Contained Package** – The entire app, including the dataset, comes bundled in one easy-to-install R package.  

---

##  Quick Start

You can run this dashboard on your own machine in **two simple steps**:

### Install the Package
```r
# First, you might need the 'devtools' package
# install.packages("devtools")

devtools::install_github("alenkarakkattil/Retail-Scope")
```

### Run the App
```r
library(RetailScope)
run_app()
```
