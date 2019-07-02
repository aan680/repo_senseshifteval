"""
This script reads [arg1], and pairs the words in column "target" with all their POS-equivalent WordNet synonyms. It outputs two files: one with the target-synset matches, one with the target-synonym matches.

"""
TEST

import sys
import re
from nltk.corpus import wordnet as wn
from nltk.corpus import brown
from nltk.tag import UnigramTagger
import pandas as pd
import nltk
nltk.data.path.append("nltk_data")
import csv
from itertools import combinations
import os



inputdata = sys.argv[1]
#inputdata = inputfolder + "targets.csv"

outputfile_synset = sys.argv[2]
#os.path.join(inputfolder, "evaluationset_synset.csv")

outputfile_wordpair = sys.argv[3]
#os.path.join(inputfolder, "evaluationset_wordpair.csv") 

posmapping_inputdata_to_wn = {"n": "n", "adj": "s", "vi": "v", "vt": "v", "v":"v", "adv":"r"}#unmapped: phr, prep, int
posmapping_alt = {"adj":"a"}

def write_headers(headers_from_source): 
	headers = ['ref', 'synset', 'synset_size', 'synset_words', 'wn_pos'] #'syn_def'
	#headers.append(headers_to_copy)
  	writer = csv.DictWriter(open(outputfile_wordpair, 'w'), fieldnames = headers_from_source + headers)	
	writer.writeheader()
	headers = ['synset', 'synset_size', 'synset_words', 'wn_pos']# 'syn_def'
	writer = csv.DictWriter(open(outputfile_synset, 'w'), fieldnames = headers_from_source + headers)	
	writer.writeheader()
	#f = ['word', 'pos', 'nr_of_synsets']
	#writer = csv.DictWriter(open(syn_pos_count_file, 'w'), fieldnames = f)
	#writer.writeheader()
 


#[syn.pos() for syn in self.synsets] 
 
def get_synsets(word): #returns list of lists
	synsets = wn.synsets(word)
 	return [[str(lemma.name()) for lemma in synset.lemmas()] for synset in synsets]

	
			

def write_to_file(row, synset, writefile): 
	syn_def_stripped = re.sub(r'[^\w\s]','', synset.definition())
	syn_def_stripped = syn_def_stripped.replace(";", ".")
	syn_def_stripped = syn_def_stripped.replace(",", ".")
	wn_info = {'synset':synset.name(),  'synset_size': len(synset.lemmas()), 'synset_words': [str(lemma.name()) for lemma in synset.lemmas()], 'wn_pos': synset.pos()}# 'syn_def':syn_def_stripped
	row = row.append(pd.Series(wn_info))
	print row
	row_as_df = pd.DataFrame(row).T
	row_as_df.to_csv(writefile, mode='a', header=False, index=False) 



def do_row(row): #relevant cols: target, pos, t
	word = row["target"]
 	synsets = wn.synsets(word) #get_synsets(word)
	for synset in synsets: #synset is a list of words 
		if synset.pos() == posmapping_inputdata_to_wn.get(row["pos"], "NA") or  synset.pos() == posmapping_alt.get(row["pos"], "NA"): #pos match
                	write_to_file(row, synset, outputfile_synset)	
			for synonym_lemma in synset.lemmas(): #this part is for the word-level file
				refword = synonym_lemma.name()
				if refword != word: #then include the word pair 
					row['ref'] = refword
  					write_to_file(row, synset, outputfile_wordpair)


 


if __name__ == "__main__":
	df = pd.read_csv(inputdata)
	df = df.drop_duplicates(keep='first')
	#TODO: sort by target
	print("reading file " + inputdata + " for target and pos, to find WordNet matches.")
	headers_from_source = list(df.columns.values)
	write_headers(headers_from_source)
	for index, row in df.iterrows():
		do_row(row)
	print("two output files (word-synonym and word-synset cartesian products) written to " + outputfile_wordpair + " and " + outputfile_synset)




	
