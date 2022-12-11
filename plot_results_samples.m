%%
figure
load('samples_Empathy_nbest.mat')
% semilogx(sampletot,red(1,:),'-o','LineWidth',2,'color','#D95319');hold on;
% semilogx(sampletot,syn(1,:),'-d','LineWidth',2,'color','#0072BD');
% semilogx(sampletot,red(2,:),'-s','LineWidth',2,'color','#EDB120');
% semilogx(sampletot,syn(2,:),'-^','LineWidth',2,'color','#4DBEEE');
plot(sampletot,red(1,:),'-o','LineWidth',2,'color','#D95319');hold on;
plot(sampletot,syn(1,:),'-d','LineWidth',2,'color','#0072BD');
plot(sampletot,red(2,:),'-s','LineWidth',2,'color','#EDB120');
plot(sampletot,syn(2,:),'-^','LineWidth',2,'color','#4DBEEE');
legend('red, order 3','syn, order 3','red, order 4','syn, order 4')
ylabel('# multiplets');xlabel('samples')
set(gca,'FontSize',30);xlim([30 2020]);ylim([-2 52]);legend boxoff

%%
make_it_tight = true;
subplot = @(m,n,p) subtightplot (m, n, p, [0.05 0.05], [0.1 0.05], [0.1 0.05]);
if ~make_it_tight,  clear subplot;  end
figure
load('samples_Ex1_zero.mat')
subplot(3,2,1);
plot(sampletot,red(1,:),'-o','LineWidth',2,'color','#D95319');hold on;
plot(sampletot,syn(1,:),'-d','LineWidth',2,'color','#0072BD');
plot(sampletot,red(2,:),'-s','LineWidth',2,'color','#EDB120');
plot(sampletot,syn(2,:),'-^','LineWidth',2,'color','#4DBEEE');
title('independent triplets, II~0');set(gca,'XTickLabel',[],'FontSize',20)
load('samples_Ex2_zero.mat')
subplot(3,2,2);
plot(sampletot,red(1,:),'-o','LineWidth',2,'color','#D95319');hold on;
plot(sampletot,syn(1,:),'-d','LineWidth',2,'color','#0072BD');
plot(sampletot,red(2,:),'-s','LineWidth',2,'color','#EDB120');
plot(sampletot,syn(2,:),'-^','LineWidth',2,'color','#4DBEEE');
title('coupled triplets, II~0');set(gca,'XTickLabel',[],'YTickLabel',[],'FontSize',20)
subplot(3,2,3);
load('samples_Ex1_red.mat')
plot(sampletot,red(1,:),'-o','LineWidth',2,'color','#D95319');hold on;
plot(sampletot,syn(1,:),'-d','LineWidth',2,'color','#0072BD');
plot(sampletot,red(2,:),'-s','LineWidth',2,'color','#EDB120');
plot(sampletot,syn(2,:),'-^','LineWidth',2,'color','#4DBEEE');
title('independent triplets, II>0');set(gca,'XTickLabel',[],'FontSize',20)
subplot(3,2,4);
load('samples_Ex2_red.mat')
plot(sampletot,red(1,:),'-o','LineWidth',2,'color','#D95319');hold on;
plot(sampletot,syn(1,:),'-d','LineWidth',2,'color','#0072BD');
plot(sampletot,red(2,:),'-s','LineWidth',2,'color','#EDB120');
plot(sampletot,syn(2,:),'-^','LineWidth',2,'color','#4DBEEE');
title('coupled triplets, II>0');set(gca,'XTickLabel',[],'YTickLabel',[],'FontSize',20)
subplot(3,2,5);
load('samples_Ex1_syn.mat')
plot(sampletot,red(1,:),'-o','LineWidth',2,'color','#D95319');hold on;
plot(sampletot,syn(1,:),'-d','LineWidth',2,'color','#0072BD');
plot(sampletot,red(2,:),'-s','LineWidth',2,'color','#EDB120');
plot(sampletot,syn(2,:),'-^','LineWidth',2,'color','#4DBEEE');
title('independent triplets, II<0');set(gca,'FontSize',20)
subplot(3,2,6);
load('samples_Ex2_syn.mat')
plot(sampletot,red(1,:),'-o','LineWidth',2,'color','#D95319');hold on;
plot(sampletot,syn(1,:),'-d','LineWidth',2,'color','#0072BD');
plot(sampletot,red(2,:),'-s','LineWidth',2,'color','#EDB120');
plot(sampletot,syn(2,:),'-^','LineWidth',2,'color','#4DBEEE');
title('coupled triplets, II<0');set(gca,'YTickLabel',[],'FontSize',20)
for i=1:6;subplot(3,2,i);ylim([0 15]);end
plot(sampletot,red(1,:),'-o','LineWidth',2,'color','#D95319');hold on;
plot(sampletot,syn(1,:),'-d','LineWidth',2,'color','#0072BD');
plot(sampletot,red(2,:),'-s','LineWidth',2,'color','#EDB120');
plot(sampletot,syn(2,:),'-^','LineWidth',2,'color','#4DBEEE');
legend('red, order 3','syn, order 3','red, order 4','syn, order 4');legend boxoff
subplot(3,2,1);ylabel('# multiplets');subplot(3,2,3);ylabel('# multiplets');
subplot(3,2,5);ylabel('# multiplets');xlabel('samples')
subplot(3,2,6);xlabel('samples')
clear subplot