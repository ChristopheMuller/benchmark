from hyperimpute.plugins.imputers import Imputers
from warnings import filterwarnings
from sklearn.experimental import enable_iterative_imputer
from sklearn.impute import IterativeImputer
from remasker.remasker_impute import ReMasker

def hyperimpute_imp(X, method):
    filterwarnings('ignore')
    imputer = Imputers().get(method)
    X_imputed = imputer.fit_transform(X.copy())
    return X_imputed


def iterative_imp(X, post):
    filterwarnings('ignore')
    imputer = IterativeImputer(sample_posterior=post)
    X_imputed = imputer.fit_transform(X.copy())
    return X_imputed

def remasker_imp(X):
    filterwarnings('ignore')
    imputer = ReMasker()
    X_imputed = imputer.fit_transform(X.copy())
    return X_imputed
