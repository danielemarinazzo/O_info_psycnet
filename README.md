# O_info_psycnet
description of code and tools to compute higher order psychological networks

How to:
---------------------------
1. Input: 2D data (observations x variables). Real data [here](https://eiko-fried.com/data/) you find plenty, or simulations (see below)
2. Clone [this repo](https://github.com/danielemarinazzo/HOI) and run [this function](https://github.com/danielemarinazzo/HOI/blob/main/hoi_exhaustive_loop_zerolag_fdr.m). For hardcore python users, [this repo](https://github.com/PranavMahajan25/HOI_toolbox) is a functioning version, but slower and still without the important check of [carryover significance](https://github.com/danielemarinazzo/HOI/blob/main/find_carryover_significance.m)\
3. Plot the output using [this repo](https://github.com/renzocom/hyperplot)


Simulation:
---------------------------
Use the R files to reproduce data with structures according to the figure below
![fourmodels](https://user-images.githubusercontent.com/5311102/164714069-c69e9676-949d-42aa-851c-49726847649b.png)
