import numpy as np
import torch
from engression import engression
from tqdm import tqdm
from geomloss import SamplesLoss
import pandas as pd

def compute_energy_loss(X_pred, X_true):
    """
    Compute energy score between predicted and true values.
    
    Parameters:
    -----------
    X_pred : torch.Tensor
        Predicted/imputed values
    X_true : torch.Tensor
        True values
        
    Returns:
    --------
    float
        Energy score
    """

    if isinstance(X_pred, np.ndarray):
        X_pred = torch.tensor(X_pred, dtype=torch.float32)
    if isinstance(X_true, np.ndarray):
        X_true = torch.tensor(X_true, dtype=torch.float32)

    Loss = SamplesLoss("energy")
    return Loss(X_pred, X_true).item()

def engressimpute(X, M=5, K=5, verbose=True, X_full=None, init_epochs=400, update_epochs=75):
    """
    Tensor-based MICE implementation using engression for imputation
    
    Parameters:
    -----------
    X : torch.Tensor or numpy.ndarray
        Data matrix with missing values (NaN)
    M : int
        Number of imputations to perform
    K : int
        Number of iterations for each imputation
    verbose : bool
        Whether to show progress bars
    X_full : torch.Tensor or numpy.ndarray, optional
        True/complete data matrix for loss computation
        
    Returns:
    --------
    list of torch.Tensor
        List of M imputed datasets
    """
    # Convert inputs to tensors if they're numpy arrays
    if isinstance(X, np.ndarray):
        X = torch.tensor(X, dtype=torch.float32)
    if isinstance(X, pd.DataFrame):
        X = torch.tensor(X.values, dtype=torch.float32)
    if X_full is not None and isinstance(X_full, np.ndarray):
        X_full = torch.tensor(X_full, dtype=torch.float32)
    
    n_samples, n_features = X.shape
    all_imputations = []
    
    for m in range(M):
        if verbose:
            print(f"\nImputation {m+1}" + "-"*20)
        
        # Initialize imputation with mean values
        X_imputed = X.clone()
        means = torch.nanmean(X, dim=0)
        nan_mask = torch.isnan(X_imputed)
        for j in range(n_features):
            X_imputed[nan_mask[:, j], j] = means[j]
            
        if X_full is not None:
            initial_loss = compute_energy_loss(X_imputed, X_full)
            if verbose:
                print(f"\nImputation {m+1} - Initial Loss: {initial_loss:.4f}")

        engressors = []
        for j in range(n_features):
            if verbose:
                print(f"..{j+1}", end='', flush=True)

            predictors = torch.cat([X_imputed[:, :j], X_imputed[:, j+1:]], dim=1)
            missing_mask = torch.isnan(X[:, j])
            X_train = predictors[~missing_mask]
            y_train = X[~missing_mask, j].reshape(-1, 1)
            X_predict = predictors[missing_mask]
            engressor = engression(X_train, y_train, num_epochs=init_epochs, verbose=False)
            engressors.append(engressor)
            X_imputed[missing_mask, j] = engressor.sample(X_predict, sample_size=1).flatten()
        if verbose:
            print()
        
        for k in range(1,K):
            for j in range(n_features):

                # Find rows where column j has missing values
                missing_mask = torch.isnan(X[:, j])
                if not torch.any(missing_mask):
                    continue
                
                # Create predictor matrix (all columns except j)
                predictors = torch.cat([X_imputed[:, :j], X_imputed[:, j+1:]], dim=1)
                
                # Split into train (observed) and predict (missing) sets
                X_train = predictors[~missing_mask]
                y_train = X[~missing_mask, j]
                X_predict = predictors[missing_mask]
                
                # Train engression model and update imputed values
                engressors[j].train(X_train, y_train, num_epochs=update_epochs, verbose=False)
                predictions = engressors[j].sample(X_predict, sample_size=1).flatten()
                X_imputed[missing_mask, j] = predictions
            
            if X_full is not None:
                current_loss = compute_energy_loss(X_imputed, X_full)
                print(f"Imputation {m+1}, Iteration {k+1} - Loss: {current_loss:.4f}")
        
        all_imputations.append(X_imputed)
    
    return all_imputations