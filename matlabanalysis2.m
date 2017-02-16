for i=1:8

    file1 = [num2str(i) '_1'];
    file2 = [num2str(i) '_2'];
    maxnum = 0;

    Dataloc = '/Volumes/KINGSTON/Experiment_Data/Reverse_Correlation_EXP1/';

    if strcmp(file1(1),'1') || strcmp(file1(1), '2') || strcmp(file1(1), '5')|| strcmp(file1(1), '6')
        condition = file1(end);
    else 
        condition = file1(end-1:end);
    end

    [white, resp] = catdata(file1, file2, Dataloc);
    %[white2, resp2] = catdata('2_1_+','2_2_+');
    %white = cat(3, white, white2);
    %resp = cat(1, resp, resp2);
    %ordervec = randperm(length(resp(161:end,2)));
    respvec{i} = resp;

    [ci(:,:,i), CI(:,:,i), avyesim(:,:,i),avnoim(:,:,i)] = genCI(white(:,:,:), resp(161:end,3));
    [len,consist] = calcConsist(resp);

    for j=(maxnum+1):size(white,3)



        pred(j-maxnum,(i)) = SSIM(white(:,:,j),ci(:,:,i));

    end

    test{i} = resp(161:end, 3);

    correct_vec = resp([41:80,121:160],3)-resp([41:80,121:160],4);

    d_prime(i) = norminv(1-(sum(correct_vec==-1)/80))-norminv(sum(correct_vec==1)/80);
    response_consistency(i) = sum((consist(:,1)-consist(:,2))==0)/len;
    RT(i) = mean(resp(161:end,5));

end

for i=11:18

    file1 = [num2str(i) '_1'];
    file2 = [num2str(i) '_2'];
    maxnum = 0;

    Dataloc = '/Volumes/KINGSTON/Experiment_Data/Reverse_Correlation_EXP2/';

    if strcmp(file1(1),'1') || strcmp(file1(1), '2') || strcmp(file1(1), '5')|| strcmp(file1(1), '6')
        condition = file1(end);
    else 
        condition = file1(end-1:end);
    end

    [white, resp] = catdata(file1, file2, Dataloc);
    %[white2, resp2] = catdata('2_1_+','2_2_+');
    %white = cat(3, white, white2);
    %resp = cat(1, resp, resp2);
    %ordervec = randperm(length(resp(161:end,2)));
    respvec{i} = resp;

    [ci(:,:,i), CI(:,:,i), avyesim(:,:,i),avnoim(:,:,i)] = genCI(white(:,:,:), resp(161:end,3));
    [len,consist] = calcConsist(resp);

    for j=(maxnum+1):size(white,3)



        pred(j-maxnum,(i)) = corr2(white(:,:,j),ci(:,:,i));

    end

    test{i} = resp(161:end, 3);

    correct_vec = resp([41:80,121:160],3)-resp([41:80,121:160],4);

    d_prime(i) = norminv(1-(sum(correct_vec==-1)/80))-norminv(sum(correct_vec==1)/80);
    response_consistency(i) = sum((consist(:,1)-consist(:,2))==0)/len;
    RT(i) = mean(resp(161:end,5));

end