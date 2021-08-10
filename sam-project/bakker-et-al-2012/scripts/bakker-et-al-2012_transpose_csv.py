import sys
import glob
import pandas as pd

from_ = sys.argv[1]

filenames = glob.glob("outputs/*_%s.csv" % from_)

for fname in filenames:
	stats = pd.read_csv(fname)
	stats = stats.drop(columns = ['type'])
	stats = stats.melt(id_vars = ['field'])
	stats['col'] = stats.variable + "_" + stats.field
	stats = stats.drop(columns = ['variable', 'field'])
	stats = stats[['col', 'value']]
	stats.T.to_csv(fname, header=None, index=None)

	# pd.read_csv(fname, header=None, index_col=0).T.to_csv(fname, index=False)
