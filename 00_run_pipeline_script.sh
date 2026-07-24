
# Mouse GRCm38
#1. extract 10K upstream and downstream TSS sequences
python 0_proc_prom_region_seq.py GRCm38.fa annotaion.gtf 10000 10000 > mouse_gene_up_down_10K.fa 2> log

#2. one-hot encoding
python 0_onehot.py mouse_gene_up_down_10K.fa  mouse_gene_up_down_10K_onehot.pickle

# 3. Generate train, val, and test datasets to make it easier to continue batch training later;
python 2_propare_datasets.py leave_chrom $Onehot $Label $Annotation Dataset.Human_Chrom8_train_test.h5 $GTF 8
python 2_propare_datasets.py leave_chrom  mm_TSS_up_down_10K_onehot.pickle ./mESCs_8_scRNAtech/mESCs_20250703.p mESCs_20250703_annatation4432.csv Dataset_mESCs_chr8_train_test_20250703.h5 gtf_annotation.csv chr8

# batch run
run benchmark_mESC_scTeh.sh

