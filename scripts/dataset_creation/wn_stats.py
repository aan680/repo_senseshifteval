
import sys

from nltk.corpus import wordnet as wn
from nltk.corpus import brown
from nltk.tag import UnigramTagger
import nltk
nltk.data.path.append("nltk_data")
import collections #for counter
import re
import codecs
import os
import numpy as np
import pandas as pd

posmapping_wn_to_inputdata = {"n": "n", "s": "adj","r":"adv", "a":"adj", "v":"v"}#unmapped: phr, prep, int

def synset_name(synset):
	return synset.name()

def synsets_as_wn_objects(word):
	return wn.synsets(word)

def synsets_by_name(word):
	synsets = wn.synsets(word)
	#return [synset.definition() for synset in synsets]
	return [synset.name() for synset in synsets]

def no_of_synsets(word):
	return len(wn.synsets(word))

def synset_def(synset):
	syn_def_original = synset.definition().strip()
	syn_def_stripped = re.sub(r'[^\w\s]','/',syn_def_original)
	return syn_def_stripped
	#return syn_def_stripped

def synset_words(synset):
	return [lemma.name() for lemma in synset.lemmas()]

def synset_words_single_string(synset):
	return '/'.join(synset_words(synset))


def synset_size(synset):
	return len(synset.lemmas())

def pos_my_notation(synset): #translate the wn pos to the notation I use (across all source data. NA if WN pos does not correspond to any 
	 wn_pos = synset.pos() 
	 return posmapping_wn_to_inputdata.get(wn_pos, "NA")
	
def no_of_pos(word):
	synsets = wn.synsets(word)
	pos_counter = collections.Counter()
	pos_counter.update(synset.pos() for synset in synsets)
	return len(pos_counter.items()) #i.e. how many different POS are the synsets distributed across  

def polysemy(word): #define polysemy of word (VECTOR) as nr of synsets / nr of different pos
	return synsets(word) / pos(word)

def synset_rank(word, synset): #what is the number of a given synset for a word? I.e. how central is the word to this synset? 
	synsets = wn_synsets(word) #! we need the raw name otherwise it says Synset(blabla.n.01)
	return synsets.index(synset) + 1 #!!! python indexes from zero so add 1 

def allinfo_synset(synset):
	info = {'synset':synset_name(synset), 'synset_def':synset_def(synset), 'synset_words':synset_words_single_string(synset), 'ref':synset_words(synset), 'pos':pos_my_notation(synset), 'synset_size':synset_size(synset), 'wn_pos':synset.pos()} #'synset_words':[synset_words(synset)]
	return info

def allinfo(target, pos, synset):
	target_and_pos = pd.DataFrame({'target':target, 'pos':pos}, index=[0])
	synsetinfo = pd.DataFrame(data=allinfo_synset(synset))
	alldata = pd.merge(target_and_pos, synsetinfo) #only synsets are kept with the with the stated pos
	return alldata


def allinfo_word(target, pos):
	synsets = synsets_as_wn_objects(target)
	return map(lambda x: allinfo(target, pos, x), synsets)

