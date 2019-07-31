import sys 
import collections
import pandas as pd
import numpy as np
import scipy.stats
import os

print("start")


#sys.path.insert(0, "./histwords")

histwords_dir = os.path.join(os.getcwd(), "histwords")
print(histwords_dir)
sys.path.insert(0, histwords_dir)


from representations.sequentialembedding import SequentialEmbedding


#embeddingfile="embeddings/eng-all_sgns"

def evaluate_by_hw(target, ref, t, gold, embeddingfile):
    gold   = int(gold)
    offset =  int(t / 10) * 10 #round off to nearest smaller decade, like I did in my R script get_correlations
    #print(offset)
    embeddings = SequentialEmbedding.load(embeddingfile, range(1800, 2000, 10))
    time_sims = embeddings.get_time_sims(target, ref)
    t = collections.OrderedDict([])
    for y in range(offset, 2000, 10): t[y]=time_sims[y]
    rho, p = scipy.stats.spearmanr(t.keys(), t.values())
    if p <= 0.05: 
	sig = 1
    else:
        sig = 0
    #print(t)
    if np.isnan(rho):
	correct = float('nan')
	sig = float('nan')
    elif np.sign(rho) == np.sign(gold):
	correct = 1
    else:
	correct = 0
    return {"correct":correct, 'sig':sig, "p":p, "corr":rho}



