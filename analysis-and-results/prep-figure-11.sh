# convert Figures/Figure-11-A-Small.png -trim Figures/Figure-11-A-Small.png
# convert Figures/Figure-11-B-Small.png -trim Figures/Figure-11-B-Small.png
# convert Figures/Figure-11-C-Small.png -trim Figures/Figure-11-C-Small.png
# convert Figures/Figure-11-B-Small.png Figures/Figure-11-C-Small.png +append Figures/Figure-11-B-C.png
# convert Figures/Figure-11-A-Small.png Figures/Figure-11-C-Small.png -append Figures/Figure-11-Small-Left-Panel.png
# convert Figures/Figure-11-Small-Left-Panel.png Figures/Figure-11-B-Small.png +append Figures/Figure-11-Small-Appended.png

convert Figures/Figure-11-B-Small_No_QRP.png -trim Figures/Figure-11-B-Small_No_QRP.png
convert Figures/Figure-11-B-Small_Bakker.png -trim Figures/Figure-11-B-Small_Bakker.png
convert Figures/Figure-11-C-Small_No_QRP.png -trim Figures/Figure-11-C-Small_No_QRP.png
convert Figures/Figure-11-C-Small_Bakker.png -trim Figures/Figure-11-C-Small_Bakker.png
convert Figures/Figure-11-B-Small_No_QRP.png Figures/Figure-11-B-Small_Bakker.png +append Figures/Figure-11-B-Double-Color.png
convert Figures/Figure-11-C-Small_No_QRP.png Figures/Figure-11-C-Small_Bakker.png +append Figures/Figure-11-C-Double-Color.png
convert Figures/Figure-11-B-Double-Color.png -trim Figures/Figure-11-B-Double-Color.png
convert Figures/Figure-11-C-Double-Color.png -trim Figures/Figure-11-C-Double-Color.png

cp Figures/Figure-11-B-Double-Color.png Figures/Figure-11-B-Double-Color-Editted.png
cp Figures/Figure-11-C-Double-Color.png Figures/Figure-11-C-Double-Color-Editted.png  

# convert Figures/Figure-11-Small.png -trim Figures/Figure-11-Small.png