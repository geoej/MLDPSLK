## Table X: Hyperparameter Tuning Details

Hyperparameter tuning details for machine learning models. All models were tuned using 10-fold repeated cross-validation (3 repeats) with `tuneLength = 3`.

| Model | Hyperparameter | Range Explored | Optimal Value | Description |
|-------|----------------|----------------|---------------|-------------|
| Random Forest | mtry | ~5-10 (varies by caret version) | 2 | Number of variables sampled at each split |
| XGBoost | nrounds | 50-200 | 50 | Boosting iterations |
|  | max_depth | 1-6 | 2 | Maximum tree depth |
|  | eta | 0.1-0.4 | 0.3000 | Learning rate |
|  | gamma | 0-0.1 | 0 | Minimum loss reduction |
|  | colsample_bytree | 0.6-1.0 | 0.8000 | Column subsampling ratio |
|  | min_child_weight | 0-10 | 1 | Minimum child weight |
|  | subsample | 0.5-1.0 | 1 | Row subsampling ratio |
| Cubist | committees | 1-100 | 10 | Number of model trees |
|  | neighbors | 0-9 | 0 | Nearest neighbors for corrections |
| SVM (Radial) | sigma | Auto-estimated (3 values around estimate) | 0.0561 | RBF kernel width |
|  | C | 0.25-4.0 (log scale) | 0.5000 | Cost parameter |

