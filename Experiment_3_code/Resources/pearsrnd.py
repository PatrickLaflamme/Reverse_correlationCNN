import numpy as np
import scipy.stats as stat

def pearsrnd(mu, sigma, skew, kurt, sizeTuple):
	
	beta1 = skew**2
	beta2 = kurt
	
	if (sigma < 0) OR (beta2 <= beta1 + 1)
		
		r = np.empty(sizeTuple).fill(np.nan)
		type = np.nan
		coefs = np.empty((1,3)).fill(np.nan)
		
	#classify the distributions and find the roots of c0 + c1*x + c2*x^2
	c0 = (4*beta2 - 3*beta1)
	c1 = skew * (beta2 + 3)
	c2 = (2*beta2 - 3*beta1 - 6)
	
	if c1 == 0
		
		if beta2 == 3
			
			type = 0
		
		else
			if beta2 < 3
				type = 2
			elif beta2 > 3
				type = 7
			
			a1 = -sqrt(abs(c0/c2))
			a2 = -a1
			
	elif c2 == 0
		
		type = 3
		a1 = -c0 / c1
	
	else 
		kappa = c1**2 / (4*c0*c2)
		
		if kappa < 0
			type = 1
		elif kappa < 1 - eps
			type = 4
		elif kappa <= 1+eps
			type = 5
		else
			type = 6
		
		tmp = -(c1 + np.sign(c1).*sqrt(c1**2 - 4*c0*c2)) / 2
		a1 = tmp / c2
		a2 = c0 / tmp
		
		if (np.real(a1) > np.real(a2))
			tmp = a1
			a1 = a2
			a2 = tmp
			
	denom = (10*beta2 - 12*beta1 - 18)