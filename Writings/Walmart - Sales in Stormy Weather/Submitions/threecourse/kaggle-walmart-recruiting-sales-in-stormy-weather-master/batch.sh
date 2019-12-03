mkdir model
mkdir submission
python r0_itemstore.py
R --vanilla < r1_baseline.r
python r2_preprocess.py
python r3a_rollingmean.py
python r3b_zeros.py
python r3c_features.py
python r4_vwtxt_creator.py
source  r5_vwrun.sh 
python r6_submission.py
#sort submission/p.csv > submission/sortp.csv
#diff submission/sortp.csv answer/sortp.csv > temp.txt