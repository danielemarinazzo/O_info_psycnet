clear;clc

ntrials=100;
maxsize=4;
n_best_e=30;
n_best_sim=15;


load('C:\Users\daniele\Dropbox\code\O_information\plot\data\psychological\Briganti2017.mat');
sampletot=50:100:length(data);
syntot=zeros(ntrials,maxsize-2,length(sampletot));redtot=syntot;
for isample=1:length(sampletot)
    for itrials=1:ntrials
        ind=randperm(length(data));
        [Otot, O_val_size_tot] = hoi_exhaustive_loop_zerolag(data(ind(1:sampletot(isample)),:), maxsize, n_best_e, 1);
        for isize=3:maxsize
            syntot(itrials,isize-2,isample)=length(Otot(isize).index_var_syn);
            redtot(itrials,isize-2,isample)=length(Otot(isize).index_var_red);
        end
    end
end
syn=squeeze(mean(syntot,1));red=squeeze(mean(redtot,1));
save samples_Empathy syn red

load('C:\Users\daniele\Dropbox\code\O_information\plot\data\simulated\Ex1_red.mat')
sampletot=50:100:length(data);
syntot=zeros(ntrials,maxsize-2,length(sampletot));redtot=syntot;
for isample=1:length(sampletot)
    for itrials=1:ntrials
        ind=randperm(length(data));
        [Otot, O_val_size_tot] = hoi_exhaustive_loop_zerolag(data(ind(1:sampletot(isample)),:), maxsize, n_best_sim, 1);
        for isize=3:maxsize
            syntot(itrials,isize-2,isample)=length(Otot(isize).index_var_syn);
            redtot(itrials,isize-2,isample)=length(Otot(isize).index_var_red);
        end
    end
end
syn=squeeze(mean(syntot,1));red=squeeze(mean(redtot,1));
save samples_Ex1_red syn red

load('C:\Users\daniele\Dropbox\code\O_information\plot\data\simulated\Ex1_syn.mat')
syntot=zeros(ntrials,maxsize-2,length(sampletot));redtot=syntot;
for isample=1:length(sampletot)
    for itrials=1:ntrials
        ind=randperm(length(data));
        [Otot, O_val_size_tot] = hoi_exhaustive_loop_zerolag(data(ind(1:sampletot(isample)),:), maxsize, n_best_sim, 1);
        for isize=3:maxsize
            syntot(itrials,isize-2,isample)=length(Otot(isize).index_var_syn);
            redtot(itrials,isize-2,isample)=length(Otot(isize).index_var_red);
        end
    end
end
syn=squeeze(mean(syntot,1));red=squeeze(mean(redtot,1));
save samples_Ex1_syn syn red

load('C:\Users\daniele\Dropbox\code\O_information\plot\data\simulated\Ex1_zero.mat')
syntot=zeros(ntrials,maxsize-2,length(sampletot));redtot=syntot;
for isample=1:length(sampletot)
    for itrials=1:ntrials
        ind=randperm(length(data));
        [Otot, O_val_size_tot] = hoi_exhaustive_loop_zerolag(data(ind(1:sampletot(isample)),:), maxsize, n_best_sim, 1);
        for isize=3:maxsize
            syntot(itrials,isize-2,isample)=length(Otot(isize).index_var_syn);
            redtot(itrials,isize-2,isample)=length(Otot(isize).index_var_red);
        end
    end
end
syn=squeeze(mean(syntot,1));red=squeeze(mean(redtot,1));
save samples_Ex1_zero syn red

load('C:\Users\daniele\Dropbox\code\O_information\plot\data\simulated\Ex2_red.mat')
sampletot=50:100:length(data);
syntot=zeros(ntrials,maxsize-2,length(sampletot));redtot=syntot;
for isample=1:length(sampletot)
    for itrials=1:ntrials
        ind=randperm(length(data));
        [Otot, O_val_size_tot] = hoi_exhaustive_loop_zerolag(data(ind(1:sampletot(isample)),:), maxsize, n_best_sim, 1);
        for isize=3:maxsize
            syntot(itrials,isize-2,isample)=length(Otot(isize).index_var_syn);
            redtot(itrials,isize-2,isample)=length(Otot(isize).index_var_red);
        end
    end
end
syn=squeeze(mean(syntot,1));red=squeeze(mean(redtot,1));
save samples_Ex2_red syn red

load('C:\Users\daniele\Dropbox\code\O_information\plot\data\simulated\Ex2_syn.mat')
syntot=zeros(ntrials,maxsize-2,length(sampletot));redtot=syntot;
for isample=1:length(sampletot)
    for itrials=1:ntrials
        ind=randperm(length(data));
        [Otot, O_val_size_tot] = hoi_exhaustive_loop_zerolag(data(ind(1:sampletot(isample)),:), maxsize, n_best_sim, 1);
        for isize=3:maxsize
            syntot(itrials,isize-2,isample)=length(Otot(isize).index_var_syn);
            redtot(itrials,isize-2,isample)=length(Otot(isize).index_var_red);
        end
    end
end
syn=squeeze(mean(syntot,1));red=squeeze(mean(redtot,1));
save samples_Ex2_syn syn red

load('C:\Users\daniele\Dropbox\code\O_information\plot\data\simulated\Ex2_zero.mat')
syntot=zeros(ntrials,maxsize-2,length(sampletot));redtot=syntot;
for isample=1:length(sampletot)
    for itrials=1:ntrials
        ind=randperm(length(data));
        [Otot, O_val_size_tot] = hoi_exhaustive_loop_zerolag(data(ind(1:sampletot(isample)),:), maxsize, n_best_sim, 1);
        for isize=3:maxsize
            syntot(itrials,isize-2,isample)=length(Otot(isize).index_var_syn);
            redtot(itrials,isize-2,isample)=length(Otot(isize).index_var_red);
        end
    end
end
syn=squeeze(mean(syntot,1));red=squeeze(mean(redtot,1));
save samples_Ex2_zero syn red
