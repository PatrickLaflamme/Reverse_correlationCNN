import numpy as np

def gabor_weights(gabor_array, img, lamda):

	transpose = gabor_array['gabor_bank'].T;

	self_multiply = np.dot(transpose, gabor_array['gabor_bank']);

	identity = np.identity(self_multiply.shape);

	test1 = np.dot(transpose, np.reshape(img, gabor_array['gabor_bank'].shape[0],img.shape[2]));

	test2 = ((lamda * identity) + self_multiply);

	weights = np.dot(test2**-1, test1);