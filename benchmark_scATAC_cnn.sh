BASEDIR=./BenchmarksInManuscript

# baseline-model
N=128; L=15; P=15; PT=avgpool; AT=ReLU
mkdir ${BASEDIR}/Benchmark_cnn_scATAC/scATAC_N${N}L${L}P${PT:0:1}P${P}${AT}
cd ${BASEDIR}/Benchmark_cnn_scATAC/scATAC_N${N}L${L}P${PT:0:1}P${P}${AT} && echo `pwd`
python ${BASEDIR}/nvtk-benchmark.py \
    ${BASEDIR}/Dataset/Dataset.sciATAC1_train_test.h5 \
    --tasktype binary_classification \
    --lr 1e-3 --patience 10 \
    --batch_size 5000 \
    --gpu-device 3 


# Length
N=128; L=15; P=15; PT=avgpool; AT=ReLU
for L in 5 25
do
    mkdir ${BASEDIR}/Benchmark_cnn_scATAC/scATAC_N${N}L${L}P${PT:0:1}P${P}${AT}
    cd ${BASEDIR}/Benchmark_cnn_scATAC/scATAC_N${N}L${L}P${PT:0:1}P${P}${AT} && echo `pwd`
    python ${BASEDIR}/nvtk-benchmark.py \
        ${BASEDIR}/Dataset/Dataset.sciATAC1_train_test.h5 \
        --tasktype binary_classification \
        --filterLenConv1 $L \
        --lr 1e-3 --patience 10 \
        --batch_size 5000 \
        --gpu-device 2
done


# Filter_Number
N=128; L=15; P=15; PT=avgpool; AT=ReLU
for N in 8 32 512
do
    mkdir ${BASEDIR}/Benchmark_cnn_scATAC/scATAC_N${N}L${L}P${PT:0:1}P${P}${AT}
    cd ${BASEDIR}/Benchmark_cnn_scATAC/scATAC_N${N}L${L}P${PT:0:1}P${P}${AT} && echo `pwd`
    python ${BASEDIR}/nvtk-benchmark.py \
        ${BASEDIR}/Dataset/Dataset.sciATAC1_train_test.h5 \
        --tasktype binary_classification \
        --numFiltersConv1 $N \
        --lr 1e-3 --patience 10 \
        --batch_size 5000 \
        --gpu-device 2
done


# Pooling size
N=128; L=15; P=15; PT=avgpool; AT=ReLU
for P in 5 25
do
    mkdir ${BASEDIR}/Benchmark_cnn_scATAC/scATAC_N${N}L${L}P${PT:0:1}P${P}${AT}
    cd ${BASEDIR}/Benchmark_cnn_scATAC/scATAC_N${N}L${L}P${PT:0:1}P${P}${AT} && echo `pwd`
    python ${BASEDIR}/nvtk-benchmark.py \
        ${BASEDIR}/Dataset/Dataset.sciATAC1_train_test.h5 \
        --tasktype binary_classification \
        --Pool1 $P \
        --lr 1e-3 --patience 10 \
        --batch_size 5000 \
        --gpu-device 2
done


# Pooling Type
N=128; L=15; P=15; PT=maxpool; AT=ReLU
mkdir ${BASEDIR}/Benchmark_cnn_scATAC/scATAC_N${N}L${L}P${PT:0:1}P${P}${AT}
cd ${BASEDIR}/Benchmark_cnn_scATAC/scATAC_N${N}L${L}P${PT:0:1}P${P}${AT} && echo `pwd`
python ${BASEDIR}/nvtk-benchmark.py \
    ${BASEDIR}/Dataset/Dataset.sciATAC1_train_test.h5 \
    --tasktype binary_classification \
    --pooltype $PT \
    --lr 1e-3 --patience 10 \
    --batch_size 5000 \
    --gpu-device 2


# activation type
N=128; L=15; P=15; PT=avgpool; AT=ReLU
for AT in Sigmoid Tanh Exp LeakyReLU
do
    mkdir ${BASEDIR}/Benchmark_cnn_scATAC/scATAC_N${N}L${L}P${PT:0:1}P${P}${AT}
    cd ${BASEDIR}/Benchmark_cnn_scATAC/scATAC_N${N}L${L}P${PT:0:1}P${P}${AT} && echo `pwd`
    python ${BASEDIR}/nvtk-benchmark.py \
        ${BASEDIR}/Dataset/Dataset.sciATAC1_train_test.h5 \
        --tasktype binary_classification \
        --activation $AT \
        --lr 1e-3 --patience 10 \
        --batch_size 5000 \
        --gpu-device 2
done


# BatchNorm
N=128; L=15; P=15; PT=avgpool; AT=ReLU
mkdir ${BASEDIR}/Benchmark_cnn_scATAC/scATAC_N${N}L${L}P${PT:0:1}P${P}${AT}BN
cd ${BASEDIR}/Benchmark_cnn_scATAC/scATAC_N${N}L${L}P${PT:0:1}P${P}${AT}BN && echo `pwd`
python ${BASEDIR}/nvtk-benchmark.py \
    ${BASEDIR}/Dataset/Dataset.sciATAC1_train_test.h5 \
    --tasktype binary_classification \
    --use_BN True \
    --lr 1e-3 --patience 10 \
    --batch_size 5000 \
    --gpu-device 2


# activation type
N=128; L=15; P=15; PT=avgpool; AT=ReLU
for AT in Sigmoid Tanh Exp
do
    mkdir ${BASEDIR}/Benchmark_cnn_scATAC/scATAC_N${N}L${L}P${PT:0:1}P${P}${AT}BN
    cd ${BASEDIR}/Benchmark_cnn_scATAC/scATAC_N${N}L${L}P${PT:0:1}P${P}${AT}BN && echo `pwd`
    python ${BASEDIR}/nvtk-benchmark.py \
        ${BASEDIR}/Dataset/Dataset.sciATAC1_train_test.h5 \
        --tasktype binary_classification \
        --activation $AT \
        --use_BN True \
        --lr 1e-3 --patience 10 \
        --batch_size 5000 \
        --gpu-device 2
done

