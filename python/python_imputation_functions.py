from hyperimpute.plugins.imputers import Imputers
from warnings import filterwarnings

def hyperimpute_imp(X, method):
    filterwarnings('ignore')
    imputer = Imputers().get(method)
    X_imputed = imputer.fit_transform(X.copy())
    return X_imputed
