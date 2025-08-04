import torch
from torch.utils.data import Dataset, DataLoader
from .dataset import CopymaskDataset
from .ac import AutoComplete
import torch.nn as nn
from torch.optim.lr_scheduler import ReduceLROnPlateau
import numpy as np


def detect_feature_types(data):
    """
    Detect binary and continuous features using number of unique values.
    
    Args:
        data: numpy array with missing values as np.nan
        
    Returns:
        binary_mask: boolean array indicating which features are binary
        feature_order: array of indices for ordered features (continuous then binary)
    """
    n_unique = np.array([len(np.unique(data[:, i][~np.isnan(data[:, i])])) for i in range(data.shape[1])])
    binary_mask = n_unique == 2
    
    # Get indices for continuous then binary features
    continuous_idx = np.where(~binary_mask)[0]
    binary_idx = np.where(binary_mask)[0]
    feature_order = np.concatenate([continuous_idx, binary_idx])
    
    return binary_mask, feature_order


def impute_data(incomplete_data, device='cpu', batch_size=2048, epochs=200, 
                lr=0.1, encoding_ratio=1, depth=1, copymask_amount=0.3, 
                val_split=0.8, verbose=True, seed=None):
    """
    Impute missing values in a numpy array using an autoencoder model.
    
    Args:
        incomplete_data: numpy array with missing values as np.nan
        device: torch device to use ('cpu' or 'cuda')
        batch_size: batch size for training
        epochs: number of training epochs
        lr: learning rate
        encoding_ratio: ratio of encoding dimension to input dimension
        depth: number of layers in encoder/decoder
        copymask_amount: probability of masking existing values during training
        val_split: fraction of data to use for validation
        verbose: whether to print progress information
        
    Returns:
        numpy array with imputed values
    """
    # verify if int when needed:
    if type(batch_size) != int:
        batch_size = int(batch_size)
    if type(epochs) != int:
        epochs = int(epochs)
    if type(depth) != int:
        depth = int(depth)


    if seed is not None:
        np.random.seed(seed)
        torch.manual_seed(seed)

    # Make a copy of the data to avoid modifying the input
    data = incomplete_data.copy()
    
    # Detect binary features and get feature order
    binary_mask, feature_order = detect_feature_types(data)
    binary_features_count = np.sum(binary_mask)
    
    if verbose:
        print(f"Features detected: continuous={len(data[0]) - binary_features_count}, binary={binary_features_count}")
    
    # Reorder features to have continuous features first, then binary
    data = data[:, feature_order]

    # # make data int for binary features
    # data[:, -binary_features_count:] = np.round(data[:, -binary_features_count:]).astype(int)
    
    # Calculate statistics for normalization
    train_mean = np.nanmean(data, axis=0)
    train_std = np.nanstd(data - train_mean, axis=0)
    train_std[train_std == 0] = 1  # Avoid division by zero
    
    # Normalize data
    normed_data = (data - train_mean) / train_std
    
    # Create train/val split
    val_ind = int(len(normed_data) * val_split)
    train_data = normed_data[:val_ind]
    val_data = normed_data[val_ind:]
    
    # Create dataloaders
    train_loader = DataLoader(
        CopymaskDataset(train_data, 'train', copymask_amount=copymask_amount),
        batch_size=batch_size, shuffle=True
    )
    val_loader = DataLoader(
        CopymaskDataset(val_data, 'val', copymask_amount=copymask_amount),
        batch_size=batch_size, shuffle=False
    )
    
    # Initialize model
    feature_dim = data.shape[1]
    model = AutoComplete(
        indim=feature_dim,
        width=1/encoding_ratio,
        n_depth=depth
    ).to(device)
    
    # Loss functions and optimizer
    cont_crit = nn.MSELoss()
    binary_crit = nn.BCEWithLogitsLoss()
    optimizer = torch.optim.SGD(model.parameters(), lr=lr, momentum=0.9)
    scheduler = ReduceLROnPlateau(optimizer, factor=0.5, threshold=1e-10, patience=20)
    
    # Training loop
    best_val_loss = float('inf')
    best_model_state = None
    
    for epoch in range(epochs):
        for phase in ['train', 'val']:
            model.train() if phase == 'train' else model.eval()
            dataloader = train_loader if phase == 'train' else val_loader
            epoch_losses = []
            
            for batch in dataloader:
                datarow, nan_inds, train_inds = batch
                datarow = datarow.float().to(device)
                masked_data = datarow.clone()
                masked_data[train_inds] = 0
                
                existing_inds = ~nan_inds
                score_inds = existing_inds.to(device)
                
                optimizer.zero_grad()
                with torch.set_grad_enabled(phase == 'train'):
                    yhat = model(masked_data)
                    
                    # Split continuous and binary features
                    cont_split = feature_dim - binary_features_count
                    
                    l_cont = torch.tensor(0.0).to(device)
                    l_binary = torch.tensor(0.0).to(device)
                    
                    if cont_split > 0:
                        l_cont = cont_crit(
                            (yhat * score_inds)[:, :cont_split],
                            (datarow * score_inds)[:, :cont_split]
                        )
                    
                    if binary_features_count > 0:
                        binarized = ((datarow * score_inds)[:, cont_split:] > 0.5).float()
                        l_binary = binary_crit(
                            (yhat * score_inds)[:, cont_split:],
                            binarized
                        )
                    
                    loss = l_cont + l_binary
                    epoch_losses.append(loss.item())
                    
                    if phase == 'train':
                        loss.backward()
                        torch.nn.utils.clip_grad_norm_(model.parameters(), 10.0)
                        optimizer.step()
            
            epoch_loss = np.mean(epoch_losses)
            if phase == 'val':
                scheduler.step(epoch_loss)
                if epoch_loss < best_val_loss:
                    best_val_loss = epoch_loss
                    best_model_state = model.state_dict().copy()
        
        if verbose and (epoch + 1) % 10 == 0:
            print(f'Epoch {epoch + 1}/{epochs}, Loss: {epoch_loss:.4f}')
    
    # Load best model
    model.load_state_dict(best_model_state)
    model.eval()
    
    # Impute full dataset
    full_loader = DataLoader(
        CopymaskDataset(normed_data, 'final'),
        batch_size=batch_size, shuffle=False
    )
    
    predictions = []
    with torch.no_grad():
        for batch in full_loader:
            datarow, _, _ = batch
            datarow = datarow.float().to(device)
            yhat = model(datarow)
            
            # Apply sigmoid to binary features
            if binary_features_count > 0:
                yhat = torch.cat([
                    yhat[:, :-binary_features_count],
                    torch.sigmoid(yhat[:, -binary_features_count:])
                ], dim=1)
                
            predictions.append(yhat.cpu().numpy())
    
    # Combine predictions and denormalize
    imputed_data = np.concatenate(predictions)
    imputed_data = imputed_data * train_std + train_mean
    
    # Round binary features to 0/1
    if binary_features_count > 0:
        imputed_data[:, -binary_features_count:] = np.round(imputed_data[:, -binary_features_count:])
    
    # Only replace missing values
    result = data.copy()
    result[np.isnan(result)] = imputed_data[np.isnan(result)]
    
    # Restore original feature order
    inverse_order = np.argsort(feature_order)
    result = result[:, inverse_order]
    
    return result
