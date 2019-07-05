import sys 
import collections
import pandas as pd
import numpy as np
import scipy.stats

print("start")
sys.path.insert(0, "./histwords")
from representations.sequentialembedding import SequentialEmbedding


embeddingfile="embeddings/eng-all_sgns"

def rho_p_correct(target, ref, gold, t):
    gold   = int(gold)
    offset =  int(t / 10) * 10 #round off to nearest smaller decade, like I did in my R script get_correlations
    print(offset)
    embeddings = SequentialEmbedding.load(embeddingfile, range(1800, 2000, 10))
    time_sims = embeddings.get_time_sims(target, ref)
    t = collections.OrderedDict([])
    for y in range(offset, 2000, 10): t[y]=time_sims[y]
    rho, p = scipy.stats.spearmanr(t.keys(), t.values())
    print(t)
    if np.isnan(rho):
	correct = float('nan')
    elif np.sign(rho) == np.sign(gold):
	correct = 1
    else:
	correct = 0
    return rho, p, correct
