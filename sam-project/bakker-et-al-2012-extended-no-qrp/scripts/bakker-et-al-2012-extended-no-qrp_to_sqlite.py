import os
import sys
import glob
import json
from tqdm import tqdm
import pandas as pd
from sqlalchemy import create_engine
from flatten_json import flatten
from string import digits

remove_digits = str.maketrans('', '', digits)

def replace_key(old_dict, old, new):
	new_dict = {}
	for (key, value) in old_dict.items():
		new_key = key.replace(old, new)
		new_dict[new_key] = old_dict[key]
	return new_dict


def extract_params(configfile, param_names):
	"""
	\brief 		Extracting and setting parameters according 
	to a given JSON file

	\param 	configfile	SAM's JSON config file
	\param 	param_names	Unique list of parameter names

	\return     A flatten dictionary with 
	"""
	config = json.load(open(configfile, 'r'))

	flatten_config = flatten(config)

	flatten_config = replace_key(flatten_config, "-", "_")

	# Getting the key value if available, if not it'll be set as None
	new_filtered_dict = {}
	
	for k in param_names:
		new_filtered_dict[k] = flatten_config.get(k, None)

	return new_filtered_dict

def extract_keys(fnames):
	"""
	\brief      Collecting all unique keys from all configuration filess
	
	\param      fnames  list of all configurations files
	
	\return     A unique set of keyss
	"""
	params = set()
	for fn in fnames:
		j = json.load(open(fn, 'r'))
		params.update(list(flatten(j).keys()))

	params = [p.replace("-", "_") for p in params]

	return params

def main():

	from_ = sys.argv[1]

	filenames = glob.glob("outputs/*_%s.csv" % from_)

	param_names = extract_keys(glob.glob("configs/*.json"))

	print("Creating the database...")
	engine = create_engine("sqlite:///dbs/bakker-et-al-2012-extended-no-qrp.db")

	for i in tqdm(range(len(filenames))):
		fbase = os.path.basename(filenames[i])
		fprefix = fbase.split("_")[0]

		params = extract_params("configs/" + fprefix + ".json", param_names)

		df = pd.read_csv("outputs/" + fprefix + ("_%s.csv" % from_))
		df.assign(**params).to_sql(from_, con = engine, if_exists='append', index=False)

if __name__ == '__main__':
	main()
