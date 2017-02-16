from create_gabor import *
import numpy as np

def gabor_bank(imSize, spatial_freqs, orientations, phases):
    
    num_wavelets = np.sum(np.asarray(spatial_freqs)**2 * len(orientations) * len( phases));
    
    gabors = np.zeros((np.asarray(imSize)**2, num_wavelets));
    
    iter = 0;
    
    for sfind in range(0, len(spatial_freqs)):
        sf = spatial_freqs[sfind];
        for orntsind in range(0, len(orientations)):
            ornts = orientations[orntsind];
            for phaseind in range(1, len(phases)):
                phase = phases[phaseind];
                
                end_iter = iter + sf**2;
                                
                count = 1;
        
                for i in range(1, sf+1):
                    for j in range(1, sf + 1):

                        xloc = float(i)*float(imSize)/float((sf+1));
                        yloc = float(j)*float(imSize)/float((sf+1));
                        
                        gabors[:,iter + count] = np.reshape(create_gabor(imSize, imSize/sf, ornts, imSize/(sf*2), phase, 0, xloc, yloc), imSize**2);

                        count = count+1;

                iter = end_iter;
                
    
    gabor_array = {'spatial_freqs':spatial_freqs, 'orientations':orientations, 'phases':phases,'gabor_bank':gabors}
    
    return gabor_array
    