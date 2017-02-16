
filter_set = create_gabor_bank(50, [3,6,11], [0,45,90,135],[0,90]);

targets = genTarget([50,50], 'x');
targets(:,:,2) = genTarget([50,50], 'xi');
targets(:,:,3) = genTarget([50,50], '+');
targets(:,:,4) = genTarget([50,50], '+i');

targetweights = gen_gabor_estimate(filter_set, targets, 0.001);

target_estimate = gen_gabor_image(targetweights(:,1), filter_set);
target_estimate(:,:,2) = gen_gabor_image(targetweights(:,2), filter_set);
target_estimate(:,:,3) = gen_gabor_image(targetweights(:,3), filter_set);
target_estimate(:,:,4) = gen_gabor_image(targetweights(:,4), filter_set);

for i=1:4
	subplot(1,4,i), imshow(target_estimate(:,:,i));
end


file = '/Users/patricklaflamme/Dropbox/WSEFEP_HQ_042016/FaceOnly/';
imsize = 128;
faceimgs = load_images(file, imsize, false);

large_filters = create_gabor_bank(128,[3,6,11],[0,45,90,135],[0,90]);
faceweights = gen_gabor_estimate(large_filters, imgs, 0.001);

subplot(1,2,1), imshow(gen_gabor_image(faceweights(:,1), large_filters));
subplot(1,2,2), imshow(gen_gabor_image(faceweights(:,1), filter_set));

subplot(1,3,1), imshow(imgs(:,:,1232));
subplot(1,3,2), imshow(gen_gabor_image(weights(:,1232), filters));
subplot(1,3,3), imshow(gen_gabor_image((normrnd(0,1,[1328,1]).*stdweights)+meanweights, filters));