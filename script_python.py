import numpy as np 
import hyperimpute


np.random.seed(0)
X_full = np.random.rand(100, 5)
M = np.random.rand(100, 5) < 0.1
X = X_full.copy()
X[M] = np.nan


from hyperimpute.plugins.imputers import Imputers

# List of hyperimpute methods:

imputers = Imputers()

methods = imputers.list()

all_imputations = {}

for method in methods:
    imputer = Imputers().get(method)
    X_imputed = imputer.fit_transform(X.copy())
    all_imputations[method] = X_imputed