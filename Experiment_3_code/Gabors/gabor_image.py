import numpy as np

def  gabor_image(weights, gabor_array):
    
    assert weights.shape[0] == gabor_array['gabor_bank'].shape[1], 'incorrect number of weights for the given gabor set'
    
    image = np.zeros(gabor_array['gabor_bank'].shape);
        
    image = np.multiply(gabor_array['gabor_bank'], np.repeat([weights], gabor_array['gabor_bank'].shape[0], axis=0));
        
    
    image = np.mean(image, 1);
    
    image = image - np.min(image);
    
    image = image / np.max(image);
    
    image = np.reshape(image, (round(np.sqrt(gabor_array['gabor_bank'].shape[0])), round(np.sqrt(gabor_array['gabor_bank'].shape[0]))));
    
    return image
    