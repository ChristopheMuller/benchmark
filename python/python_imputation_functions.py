from hyperimpute.plugins.imputers import Imputers
from warnings import filterwarnings
from sklearn.experimental import enable_iterative_imputer
from sklearn.impute import IterativeImputer
from remasker.remasker_impute import ReMasker
from timeout_function_decorator import timeout


@timeout(3600)
def hyperimpute_imp(X, method, seed):
    filterwarnings('ignore')
    imputer = Imputers().get(method)
    imputer.__init__(random_state=seed)
    X_imputed = imputer.fit_transform(X.copy())
    return X_imputed

@timeout(3600)
def iterative_imp(X, post):
    filterwarnings('ignore')
    imputer = IterativeImputer(sample_posterior=post)
    X_imputed = imputer.fit_transform(X.copy())
    return X_imputed

@timeout(3600)
def remasker_imp(X, seed):
    filterwarnings('ignore')
    imputer = ReMasker()
    imputer.set_seed(seed)
    X_imputed = imputer.fit_transform(X.copy())
    return X_imputed
