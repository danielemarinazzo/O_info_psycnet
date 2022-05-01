# O_info_psycnet
description of code and tools to compute higher order psychological networks

How to:
---------------------------
1. Input: 2D data (observations x variables). Real data ([here](https://eiko-fried.com/data/)) you find plenty, or simulations (see below)
2. Clone [this repo](https://github.com/danielemarinazzo/HOI) and run [this function](https://github.com/danielemarinazzo/HOI/blob/main/hoi_exhaustive_loop_zerolag_fdr.m). For hardcore python users, [this repo](https://github.com/PranavMahajan25/HOI_toolbox) is a functioning version, but slower and still without the important check of [carryover significance](https://github.com/danielemarinazzo/HOI/blob/main/find_carryover_significance.m)
3. Plot the output using [this repo](https://github.com/renzocom/hyperplot) and the notebook `High order network psychometrics.ipynb`


Simulation:
---------------------------
Use the R files to reproduce data with structures according to the figure below

<img width="834" alt="threemodels" src="https://user-images.githubusercontent.com/5311102/166160831-a81f55c3-c131-4e12-ab1f-e3f46593c9e5.png">
