import pickle
import numpy as np
import pandas as pd
# import pyfastx
# import logging
#logging.basicConfig(filename="logging.onehotGenome.txt", level=logging.DEBUG)


onehot_nuc = {'A':[1,0,0,0],
            'C':[0,1,0,0],
            'G':[0,0,1,0],
            'T':[0,0,0,1],
            'N':[0,0,0,0]}
            

def _onehot_seq(seq):
    return np.array([onehot_nuc[nuc] for nuc in str(seq).upper()])


def _onehot_genome(gfname):
    genome_dict = {}
    with open(gfname, "r") as fh:
        for line in fh:
            if line.startswith(">"):
                name = line.split()[0].replace(">", '')
            else:
                seq = line.rstrip()
                if name not in genome_dict:        
                    genome_dict[name] = [seq, _onehot_seq(seq)]
    
    return genome_dict


if __name__ == '__main__':
    from sys import argv
    assert len(argv) == 3
    _, gfname, out_fname = argv
    genome_dict = _onehot_genome(gfname)
    pickle.dump(genome_dict, open(out_fname, 'wb'))