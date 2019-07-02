"""
This script reads [arg1], and pairs the words in column "target" with all their POS-equivalent WordNet synonyms. It outputs two files: one with the target-synset matches, one with the target-synonym matches.

"""

import sys
import random
#from scipy import 
from nltk.corpus import wordnet as wn
from nltk.tag import UnigramTagger
import pandas as pd
import nltk
nltk.data.path.append("nltk_data")
import csv
import pdb
import collections
import re
import codecs
import os



inputdata = sys.argv[1]
#inputdata = inputfolder + "targets.csv"
outputfile_synset = sys.argv[2]
#os.path.join(inputfolder, "evaluationset_synset.csv")
outputfile_wordpair = sys.argv[3]
#os.path.join(inputfolder, "evaluationset_wordpair.csv") 

posmapping_inputdata_to_wn = {"n": "n", "adj": "s", "vi": "v", "vt": "v", "v":"v", "adv":"r"}#unmapped: phr, prep, int
posmapping_alt = {"adj":"a"}

def write_headers(headers_to_copy):
	headers = ['ref', 'syn_def', 'synset', 'synset_size', 'synset_words', 'wn_pos']
	#headers.append(headers_to_copy)
  	writer = csv.DictWriter(open(outputfile_wordpair, 'w'), fieldnames = headers_to_copy + headers)	
	writer.writeheader()
	writer = csv.DictWriter(open(outputfile_synset, 'w'), fieldnames = headers_to_copy + headers)	
	writer.writeheader()
	#f = ['word', 'pos', 'nr_of_synsets']
	#writer = csv.DictWriter(open(syn_pos_count_file, 'w'), fieldnames = f)
	#writer.writeheader()
 


#[syn.pos() for syn in self.synsets] 
 
def get_synsets(word): #returns list of lists
	synsets = wn.synsets(word)
 	return [[str(lemma.name()) for lemma in synset.lemmas()] for synset in synsets]

	
			

def write_to_file(row, ref, synset, writefile): 
	syn_def_original = synset.definition()
	syn_def_stripped = re.sub(r'[^\w\s]','',syn_def_original)
	row_dict = {'ref':ref, 'wn_pos': synset.pos(), 'synset':synset.name(), 'syn_def':syn_def_stripped, 'synset_words': [str(lemma.name()) for lemma in synset.lemmas()], 'synset_size': len(synset.lemmas())}
	row = row.append(pd.Series(row_dict))
	row_as_df = pd.DataFrame(row).T
	#row_in_order = row_as_df[['term', 'match', 'syn_def','definition', 'syn_words',  'syn', 'pos', 'id']]
	row_as_df.to_csv(writefile, mode='a', header=False, index=False)



def do_row(row): #relevant cols: target, pos, t
	word = row["target"]
 	synsets = wn.synsets(word) #get_synsets(word)
	for synset in synsets: #synset is a list of words 
		if synset.pos() == posmapping_inputdata_to_wn.get(row["pos"], "NA") or  synset.pos() == posmapping_alt.get(row["pos"], "NA"):
                	write_to_file(row, None, synset, writefile = outputfile_synset)	
			for synonym_lemma in synset.lemmas():
				refword = synonym_lemma.name()
				if refword != word:
  					write_to_file(row, refword, synset, writefile = outputfile_wordpair)
					#log_ambiguity(refword)

def overwrite_and_remove_duplicates(csvfile):
	df = pd.read_csv(csvfile)
	df = df.drop_duplicates(keep='first')
	df.to_csv(csvfile, mode='w', header=True, index=False)

if __name__ == "__main__":
	df = pd.read_csv(inputdata)
	#TODO: sort by target
	print("reading file " + inputdata + " for target and pos, to find WordNet matches.")
	headers_to_copy = list(df.columns.values)
	write_headers(headers_to_copy)
	for index, row in df.iterrows():
		do_row(row)
	print("two output files (word-synonym and word-synset cartesian products) written to " + outputfile_wordpair + " and " + outputfile_synset)
	#now remove duplicates
	overwrite_and_remove_duplicates(outputfile_synset)
	overwrite_and_remove_duplicates(outputfile_wordpair)


	
