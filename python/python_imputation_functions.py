from hyperimpute.plugins.imputers import Imputers
from warnings import filterwarnings
from sklearn.experimental import enable_iterative_imputer
from sklearn.impute import IterativeImputer
from remasker.remasker_impute import ReMasker
from timeout_function_decorator import timeout
from auto_complete.fit import impute_data as AutoCompleteImpute


@timeout(10800)
def hyperimpute_imp(X, method, seed):
    filterwarnings('ignore')
    imputer = Imputers().get(method)
    imputer.__init__(random_state=seed)
    X_imputed = imputer.fit_transform(X.copy())
    return X_imputed

@timeout(10800)
def iterative_imp(X, post):
    filterwarnings('ignore')
    imputer = IterativeImputer(sample_posterior=post)
    X_imputed = imputer.fit_transform(X.copy())
    return X_imputed

@timeout(10800)
def remasker_imp(X, seed):
    filterwarnings('ignore')
    imputer = ReMasker()
    imputer.set_seed(seed)
    X_imputed = imputer.fit_transform(X.copy())
    return X_imputed

@timeout(10800)
def autocomplete_imp(X, batch_size=2048, epochs=200, 
                lr=0.1, encoding_ratio=1, depth=1, copymask_amount=0.3, 
                val_split=0.8, verbose=False, seed=None):
    filterwarnings('ignore')
    X_numpy = X.values
    X_imputed = AutoCompleteImpute(X_numpy, batch_size=batch_size, epochs=epochs, 
                lr=lr, encoding_ratio=encoding_ratio, depth=depth, copymask_amount=copymask_amount, 
                val_split=val_split, verbose=verbose, seed=seed)
    return X_imputed
