BASEDIR=./BenchmarksInManuscript

# baseline-model
N=128; L=15; P=15; PT=avgpool; AT=ReLU
mkdir ${BASEDIR}/Benchmark_cnn_scRNA/scRNA_N${N}L${L}P${PT:0:1}P${P}${AT}
cd ${BASEDIR}/Benchmark_cnn_scRNA/scRNA_N${N}L${L}P${PT:0:1}P${P}${AT} && echo `pwd`
python ${BASEDIR}/nvtk-benchmark.py \
    ${BASEDIR}/Dataset/Dataset.Mouse_sctechnologies_leave_chrom8_mESC_MAGIC.h5 \
    --subset_task SmartSeq2 --subset_task_by Cluster \
    --lr 1e-5 --patience 10 --batch_size 128 \
    --gpu-device 1


# Length
N=128; L=15; P=15; PT=avgpool; AT=ReLU
for L in 5 25
do
    mkdir ${BASEDIR}/Benchmark_cnn_scRNA/scRNA_N${N}L${L}P${PT:0:1}P${P}${AT}
    cd ${BASEDIR}/Benchmark_cnn_scRNA/scRNA_N${N}L${L}P${PT:0:1}P${P}${AT} && echo `pwd`
    python ${BASEDIR}/nvtk-benchmark.py \
        ${BASEDIR}/Dataset/Dataset.Mouse_sctechnologies_leave_chrom8_mESC_MAGIC.h5 \
        --subset_task SmartSeq2 --subset_task_by Cluster \
        --filterLenConv1 $L \
        --lr 1e-5 --patience 10 --batch_size 128 \
        --gpu-device 1
done


# Filter_Number
N=128; L=15; P=15; PT=avgpool; AT=ReLU
for N in 8 32 512
do
    mkdir ${BASEDIR}/Benchmark_cnn_scRNA/scRNA_N${N}L${L}P${PT:0:1}P${P}${AT}
    cd ${BASEDIR}/Benchmark_cnn_scRNA/scRNA_N${N}L${L}P${PT:0:1}P${P}${AT} && echo `pwd`
    python ${BASEDIR}/nvtk-benchmark.py \
        ${BASEDIR}/Dataset/Dataset.Mouse_sctechnologies_leave_chrom8_mESC_MAGIC.h5 \
        --subset_task SmartSeq2 --subset_task_by Cluster \
        --numFiltersConv1 $N \
        --lr 1e-5 --patience 10 --batch_size 128 \
        --gpu-device 1
done


# Pooling size
N=128; L=15; P=15; PT=avgpool; AT=ReLU
for P in 5 25
do
    mkdir ${BASEDIR}/Benchmark_cnn_scRNA/scRNA_N${N}L${L}P${PT:0:1}P${P}${AT}
    cd ${BASEDIR}/Benchmark_cnn_scRNA/scRNA_N${N}L${L}P${PT:0:1}P${P}${AT} && echo `pwd`
    python ${BASEDIR}/nvtk-benchmark.py \
        ${BASEDIR}/Dataset/Dataset.Mouse_sctechnologies_leave_chrom8_mESC_MAGIC.h5 \
        --subset_task SmartSeq2 --subset_task_by Cluster \
        --Pool1 $P \
        --lr 1e-5 --patience 10 --batch_size 128 \
        --gpu-device 1
done


# Pooling Type
N=128; L=15; P=15; PT=maxpool; AT=ReLU
mkdir ${BASEDIR}/Benchmark_cnn_scRNA/scRNA_N${N}L${L}P${PT:0:1}P${P}${AT}
cd ${BASEDIR}/Benchmark_cnn_scRNA/scRNA_N${N}L${L}P${PT:0:1}P${P}${AT} && echo `pwd`
python ${BASEDIR}/nvtk-benchmark.py \
    ${BASEDIR}/Dataset/Dataset.Mouse_sctechnologies_leave_chrom8_mESC_MAGIC.h5 \
    --subset_task SmartSeq2 --subset_task_by Cluster \
    --pooltype $PT \
    --lr 1e-5 --patience 10 --batch_size 128 \
    --gpu-device 1


# activation type
N=128; L=15; P=15; PT=avgpool; AT=ReLU
for AT in Sigmoid Tanh Exp LeakyReLU
do
    mkdir ${BASEDIR}/Benchmark_cnn_scRNA/scRNA_N${N}L${L}P${PT:0:1}P${P}${AT}
    cd ${BASEDIR}/Benchmark_cnn_scRNA/scRNA_N${N}L${L}P${PT:0:1}P${P}${AT} && echo `pwd`
    python ${BASEDIR}/nvtk-benchmark.py \
        ${BASEDIR}/Dataset/Dataset.Mouse_sctechnologies_leave_chrom8_mESC_MAGIC.h5 \
        --subset_task SmartSeq2 --subset_task_by Cluster \
        --activation $AT \
        --lr 1e-5 --patience 10 --batch_size 128 \
        --gpu-device 1
done


# BatchNorm
N=128; L=15; P=15; PT=avgpool; AT=ReLU
mkdir ${BASEDIR}/Benchmark_cnn_scRNA/scRNA_N${N}L${L}P${PT:0:1}P${P}${AT}BN
cd ${BASEDIR}/Benchmark_cnn_scRNA/scRNA_N${N}L${L}P${PT:0:1}P${P}${AT}BN && echo `pwd`
python ${BASEDIR}/nvtk-benchmark.py \
    ${BASEDIR}/Dataset/Dataset.Mouse_sctechnologies_leave_chrom8_mESC_MAGIC.h5 \
    --subset_task SmartSeq2 --subset_task_by Cluster \
    --use_BN True \
    --lr 1e-5 --patience 10 --batch_size 128 \
    --gpu-device 1


# activation type
N=128; L=15; P=15; PT=avgpool; AT=ReLU
for AT in Sigmoid Tanh Exp
do
    mkdir ${BASEDIR}/Benchmark_cnn_scRNA/scRNA_N${N}L${L}P${PT:0:1}P${P}${AT}BN
    cd ${BASEDIR}/Benchmark_cnn_scRNA/scRNA_N${N}L${L}P${PT:0:1}P${P}${AT}BN && echo `pwd`
    python ${BASEDIR}/nvtk-benchmark.py \
        ${BASEDIR}/Dataset/Dataset.Mouse_sctechnologies_leave_chrom8_mESC_MAGIC.h5 \
        --subset_task SmartSeq2 --subset_task_by Cluster \
        --activation $AT \
        --use_BN True \
        --lr 1e-5 --patience 10 --batch_size 128 \
        --gpu-device 1
done

