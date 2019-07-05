
#these ones you need for sure
import re
import sys
import csv
import os
import nltk
nltk.data.path.append("/export/scratch1/aggelen/nltk_data")
nltk.data.path.append("/Users/Astrid/nltk_data")
from nltk.corpus import wordnet as wn
import pandas as pd
import collections

gold = sys.argv[1] #are the positive instances (the matched words) moving away (-1) or towards (1) each other?
if gold == "1":
	gold=1
elif gold == "-1":
	gold=-1
ht_input = sys.argv[2] #"./thesaurus/thesaurus_combined_res_1950-1959.csv
outputfile_synset_all = sys.argv[3] #e.g. "./
outputfile_wordpair_match = sys.argv[4] #e.g. ".
outputfile_synset_match = sys.argv[5] 
outputfile_wordpair_nonmatch = sys.argv[6] #outputfile_wordpair.replace(".csv","_nonmatches.csv")

print(os.path.isdir("data/HT/oldsenses"))
print(os.path.isdir("data/logdata"))


posmapping_ht_to_wn = {"n": "n", "adj": "s", "vi": "v", "vt": "v", "v":"v", "adv":"r"}#unmapped: phr, prep, int
posmapping_alt = {"adj":"a"}
#unique POS values in HT: array(['n', 'adj', 'adv', 'vt', 'vi', 'v', 'phr', 'prep', 'int'])

def match(descriptors, synset, target):
	MatchInfo = collections.namedtuple('MatchInfo', ['pos_match', 'term_match'])
	row["pos"]=row["pos"].strip(".")
	if synset.pos() == posmapping_ht_to_wn.get(row["pos"].strip(), "NA") or  synset.pos() == posmapping_alt.get(row["pos"], "NA"):
		synonyms = [str(lemma.name()) for lemma in synset.lemmas()]
		if target in synonyms: synonyms.remove(target)
		matching_terms = list(set(descriptors) & set(synonyms))
		if not matching_terms: #i.e. POS match but no matching terms (empty list)
			return(MatchInfo(True, False))
		else: #POS match and matching terms found
			return(MatchInfo(True, True))
	else:  #no POS match or thesaurus POS we do not use
		return(MatchInfo(False, None))

def write_headers(): #and overwrite old file
	f = ['term', 'wn_match', 't', 'syn_def','ht_def', 'syn_words',  'syn', 'pos', 'id'] #synset
	f2 = ['target', 'ref', 't', 'match', 'gold', 'syn_def','definition', 'syn_words',  'syn', 'pos', 'id'] #wordpair
  	writer = csv.DictWriter(open(outputfile_synset_all, 'w'), fieldnames = f)	
	writer.writeheader()
	writer = csv.DictWriter(open(outputfile_synset_match, 'w'), fieldnames = f)	
	writer.writeheader()
	writer = csv.DictWriter(open(outputfile_wordpair_match, 'w'), fieldnames = f2)	
	writer.writeheader()
	writer = csv.DictWriter(open(outputfile_wordpair_nonmatch, 'w'), fieldnames = f2)	
	writer.writeheader()

def write_row_synset(row, outputfile):
	row_as_df = pd.DataFrame(row).T
	df_select = row_as_df[['target', 'match', 't', 'syn_def','definition', 'syn_words',  'syn', 'pos', 'id']]
	df_select.to_csv(outputfile, mode='a', header=False, index=False)

def write_row_wordpair(row, outputfile):
	row_as_df = pd.DataFrame(row).T
	df_select = row_as_df[['target', 'ref', 't', 'match', 'gold', 'syn_def','definition', 'syn_words',  'syn', 'pos', 'id']]
	df_select.to_csv(outputfile, mode='a', header=False, index=False)


def match_row(row):

	row["target"] = row["term"].lower() #lowercase the target term first
	#row["t"] = (row["startyear"] + row["endyear"])/2 #average, usually the same
	descriptors = row["definition"].lower().split("/")
	for synset in wn.synsets(row["target"].decode('utf-8', 'ignore')):
		match_info = match(descriptors, synset, row["target"])
		
		if match_info.pos_match: #only write if synset is of the right POS. term_match is boolean
			print("pos match")
			row["syn"] = synset
			row["match"] = match_info.term_match #boolean. True if matching terms
			syn_def = synset.definition()
			row["syn_def"] = re.sub(r'[^\w\s]','',syn_def)
			row["syn_words"] = [str(lemma.name()) for lemma in synset.lemmas()]
			if row["match"]:
				row["gold"]=gold
			else:
				row["gold"]=0
			print(row)
			write_row_synset(row, outputfile_synset_all)
			if row["match"]: #if there is a synset match also write to the wordpair level file, synset match then match for all terms in synset
				write_row_synset(row, outputfile_synset_match)
				for lemma in synset.lemmas():
					row["ref"]= lemma.name()
					if row["term"] != lemma.name(): #do not include the entry if target == ref
						#row["gold"] = gold #by virtue of "match" being TRUE
						write_row_wordpair(row, outputfile_wordpair_match)
			else: #save these instances in a file with non-matches, for posthoc analysis
				for lemma in synset.lemmas():
					if row["term"] != lemma.name(): 
						row["ref"]= lemma.name()
						write_row_wordpair(row, outputfile = outputfile_wordpair_nonmatch)
				


def remove_targetwords_without_any_match(): #redundant
	df = pd.read_csv(outputfile_synset_all)
	targets_with_match = df.loc[df['wn_match'] == True,'term']
	df_subset= df[df.term.isin(targets_with_match)]
	df_subset.to_csv(outputfile_synset_match, mode='w', header=True, index=False)

if __name__ == "__main__":
	write_headers()
	df = pd.read_csv(ht_input)
	for index, row in df.iterrows():
		match_row(row)
	#remove_targetwords_without_any_match()
	#print("Collected the synsets for the HT target words and saved the result as "+ outputfile_synset)
	#print("Removed the target words without any synset match and saved the resulting dataset as "+ outputfile_matching_terms)
	#print("Wrote the target words with their WN-matched reference terms to "+ outputfile_wordpair)



