#/!bin/bash

from=${1}
for i in outputs/*_${from}_prepared.csv;do sed -n '2p' $i >> outputs/${from}_Stacked.csv;done
