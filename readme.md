# Average Order Value (AOV) Algorithm in Haskell

## 📚 Overview

This project implements the **Average Order Value (AOV)** algorithm in Haskell, a machine learning algorithm commonly used for analyzing transaction patterns in datasets. The implementation allows for training and testing models using a provided dataset, as well as extracting and displaying the best rules based on support metrics.

The AOV algorithm is crucial in determining the average value of orders, often used in market basket analysis to gain insights into customer purchasing behaviors.

## 🛠 Files and Directories

- **reglas_DORE_MARTIN.hs**: The Haskell source code implementing the AOV algorithm.
- **12_crx.csv**: A sample dataset that can be used to train and test the AOV model.

## 🚀 How to Use

1. **Load the Haskell file**:
   ```bash
   :l reglas_DORE_MARTIN.hs
   ```

2. **Train and test the model**:
   Execute the following Haskell commands:

   ```haskell
   (cab:dat) <- loadCSV "12_crx.csv"          -- Load the data
   (train,test) = splitData 0.8 dat           -- Split data into training and testing sets
   modelo = entrenarModelo train              -- Train the model on the training set
   accuracy = testarModelo modelo test        -- Test the model on the test set
   accuracy2 = testarModelo2 modelo test      -- Test model2 (predict2) on the test set
   ```

   **Display accuracy**:

   ```haskell
   putStrLn $ "Accuracy: " ++ show (accuracy * 100) ++ "%"
   putStrLn $ "Accuracy_2: " ++ show (accuracy2 * 100) ++ "%"
   ```

3. **Print the best rules**:
   To extract and display the best rules based on support values, execute the following commands:

   ```haskell
   rs2 = qs mejorRegla modelo                                  -- Sort the ruleset
   reglasMejores = [(sop, a, b) | (sop, a, b)<-rs2, sop >= 4]  -- Obtain the best rules with support >= 4
   saltoLinea a b = a ++ "\n" ++ b                             -- Define function to display rules
   putStrLn $ foldr (saltoLinea.show) "" reglasMejores         -- Print the best rules
   ```

## 📂 Project Structure

```
├── reglas_DORE_MARTIN.hs   # Haskell source code for AOV implementation
├── 12_crx.csv              # Sample dataset
└── README.md               # Project description
```

## 📝 Future Enhancements

- Implement more advanced metrics and evaluation techniques for model testing.
- Expand support for other datasets and provide detailed analysis reports.
- Add additional predictive models and compare results with the AOV model.
