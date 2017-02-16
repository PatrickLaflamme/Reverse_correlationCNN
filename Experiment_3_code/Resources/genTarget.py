from __future__ import division
import numpy as np
import scipy.misc
import matplotlib.pyplot as plt

def genTarget(dim, targType):
	
	width = 2/5;
	
	if type(dim)==int:
		dim = [dim, dim]
	
	tempdim = dim*2;
	
	targetimage = np.ones(tempdim);
	
	if targType == '+':
		targetimage[np.floor(tempdim[1]/2)-np.floor(tempdim[1]/(dim[1]*width)):(1+np.floor(tempdim[1]/2)+np.floor(tempdim[1]/(dim[1]*width))),np.floor(tempdim[2]/2)-np.floor(dim[2]*4/10)):(np.floor(tempdim[2]/2)+np.floor(dim[2]*4/10))] = 0
		targetimage[np.arange((np.floor(tempdim[1]/2)-np.floor(dim[1]*4/10)),(np.floor(tempdim[1]/2)+np.floor(dim[1]*4/10))+1),np.arange(np.floor(tempdim[2]/2)-np.floor(tempdim[2]/(dim[1]*width)),np.floor(tempdim[2]/2)+np.ceil(tempdim[2]/(dim[1]*width))+1)] = 0;
	
	elif targType == '+i':        
		
		targetimage[np.arange(np.floor(tempdim[1]/2)-np.floor(dim[1]*4/10), (np.floor(tempdim[1]/2)+np.floor(dim[1]*4/10))+1),np.arange(np.floor(tempdim[2]/2)-np.floor(tempdim[2]/(dim[1]*width)), (np.floor(tempdim[2]/2)+np.ceil(tempdim[2]/(dim[1]*width)))+1)] = 0;
		targetimage = misc.imrotate(targetimage, -5, 'nearest', 'crop');
		targetimage[np.arange(np.floor(tempdim[1]/2)-np.floor(tempdim[1]/(dim[1]*width)), np.floor(tempdim[1]/2)+np.ceil(tempdim[1]/(dim[1]*width))+1),np.arange((np.floor(tempdim[2]/2)-np.floor(dim[2]*4/10)), (np.floor(tempdim[2]/2)+np.floor(dim[2]*4/10))+1)] = 0;	
	
	elif targType == 'x':
		
		targetimage[np.arange(np.floor(tempdim[1]/2)-np.floor(tempdim[1]/(dim[1]*width)), np.floor(tempdim[1]/2)+np.ceil(tempdim[1]/(dim[1]*width))+1),np.arange((np.floor(tempdim[2]/2)-np.floor(dim[2]*4/10)),(np.floor(tempdim[2]/2)+np.floor(dim[2]*4/10))+1)] = 0;
		targetimage[np.arange((np.floor(tempdim[1]/2)-np.floor(dim[1]*4/10)),(np.floor(tempdim[1]/2)+np.floor(dim[1]*4/10)),np.floor(tempdim[2]/2)-np.floor(tempdim[2]/(dim[1]*width))),np.arange(np.floor(tempdim[2]/2)+np.ceil(tempdim[2]/(dim[1]*width))+1)] = 0;
	
		targetimage = misc.imrotate(targetimage, -45, 'nearest','crop');
	
	elif targType == 'xi':
		
		targetimage[np.arange(np.floor(tempdim[1]/2)-np.floor(tempdim[1]/(dim[1]*width)), np.floor(tempdim[1]/2)+np.ceil(tempdim[1]/(dim[1]*width))+1),np.arange((np.floor(tempdim[2]/2)-np.floor(dim[2]*4/10)),(np.floor(tempdim[2]/2)+np.floor(dim[2]*4/10))+1)] = 0;
	
		targetimage = misc.imrotate(targetimage, -30, 'nearest','crop');
	
		targetimage[np.arange((np.floor(tempdim[1]/2)-np.floor(dim[1]*4/10)), (np.floor(tempdim[1]/2)+np.floor(dim[1]*4/10))+1),np.arange(np.floor(tempdim[2]/2)-np.floor(tempdim[2]/(dim[1]*width)),np.floor(tempdim[2]/2)+np.ceil(tempdim[2]/(dim[1]*width))+1)] = 0;
			
		targetimage = misc.imrotate(targetimage, -30, 'nearest','crop');
		
		
	targetimage = targetimage[np.arange((dim[1]*0.5+1), dim[1]*1.5 +1), np.arange((dim[2]*0.5+1),dim[2]*1.5)];   




plt.imshow(genTarget(50,'+i'))
plt.show()
	  