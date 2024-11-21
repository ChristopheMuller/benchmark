import hyperimpute_script
from hyperimpute.plugins.imputers import Imputers

def hyperimpute_python(X, method):
    imputer = Imputers().get(method)
    X_imputed = imputer.fit_transform(X.copy())
    return X_imputed