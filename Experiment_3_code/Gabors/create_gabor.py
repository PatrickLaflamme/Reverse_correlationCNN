import matplotlib.pyplot as plt
import numpy as np

def create_gabor(imSize, lamda, theta, sigma, phase, trim, xloc, yloc, plot_val=False):

	X = np.arange(1, imSize+1);                           # X is a vector from 1 to imageSize
	Xx = (X - xloc)/float(imSize);                 # rescale X -> -.5 to .5
	Xy = (X - yloc)/float(imSize);
	freq = float(imSize)/float(lamda);                    # compute frequency from wavelength
	phaseRad = (float(phase)/360 * 2 * np.pi);         # convert to radians: 0 -> 2*pi

	[Xm, Ym] = np.meshgrid(Xx, Xy);            # 2D matrices


	thetaRad = (float(theta) / 360) * 2 * np.pi; # convert theta (orientation) to radians
	Xt = Xm * np.cos(thetaRad);                # compute proportion of Xm for given orientation
	Yt = Ym * np.sin(thetaRad);                # compute proportion of Ym for given orientation
	XYt = Xt + Yt ;                      # sum X and Y components
	XYf = XYt * freq * float(2)* np.pi;                # convert to radians and scale by frequency
	grating = np.sin( XYf + phaseRad);         # make 2D sinewave

	s = float(sigma) / float(imSize);                     # gaussian width as fraction of imageSize

	gauss = np.exp( -(((Xm**2)+(Ym**2)) / (2 * s**2)) ); # formula for 2D gaussian
	gauss[gauss < trim] = 0;                # trim around edges (for 8-bit colour displays)
	gabor = grating * gauss;               # use .* dot-product

	if plot_val==True:
		fig = plt.imshow( gabor,cmap='Greys_r')               # display
		fig.axes.get_xaxis().set_visible(False)
		fig.axes.get_yaxis().set_visible(False)
		plt.show()
	
	return gabor


