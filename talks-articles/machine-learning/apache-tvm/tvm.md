
## Apache TVM

> [doc](https://tvm.apache.org/docs/tutorial/index.html), [github](https://github.com/apache/tvm)
> A Deep Learning compiler that enables access to high-perf ML. Framework for CPUs, GPUs & ML accelerators.
> Allows ML engineers to optimize & run computations efficiently on any hardware backend.

### Overview of TVM & Model Optimization

```
   (1)       [2. Relay   _\3. TE        _\4.Auto TVM/Sched_\5.TE+Schedule_\6. TIR     _\]   (7)
[TF/PyTorch] [(high-level)/(computation) /(auto-tuning)    /(Optimization)/(Low-level) /] [Machine]
[/ONNX     ] [( IR       ) (definition )  ( module    )     (Spec        ) (IR       )  ] [ Code  ]
```

* (1) Import model from f/w like Tensorflow/PyTorch/ONNX (level of support is different, on issues try converting model to ONNX first). TVM ingest models at importer layer.

* (2) Imported model is represented in Relay (TVM's functional language `IR` i.e. Intermediate Representation for NNs). It applies graph-level optimization passes to model.

* (3) Post-optimizations Relay runs FuseOps pass to partition model into several sub-graphs, transformed to lower level representation of Tensor Expression (TE). TE is DSL for tensor computations. TE also provides schedule primitives to specify low-level loop optimizations like tiling, vectorization, parallelization, unrolling & fusion. TVM also includes a TOPI (Tensor Operator Inventory) that pre-defined templates of common tensor operators (e.g. conv2d, transpose).

* (4) Auto-tuning modules search for best schedule & compare them with cost models & on-device measurements. Two modules:

> * AutoTVM, template based. Runs search algorithms to find best values for tunable knobs. Common operators available via TOPI.
> * AutoScheduler (a.k.a. Ansor), template-free. Generates search space automatically by analyzing computation definition.

* (5) Choose optimal config for model compilations. Auto-tuning model generates JSON tuning records. Here best schedule for each subgraph gets picked.

* (6) Lower to TIR i.e. Tensor IR w/ low-level optimization. Then lowered to target compiler of h/w platform. It's final phase producing optimized model deployable to production. TVM support several compiler backends including:

> * LLVM, can arbitrary microprocessor arch including x86 & ARM, AMDGPU & NVPTX code gen; and any platform supported by LLVM.
> * Specialized compilers as NVCC, NVIDIA's compiler.
> * Embedded & specialized targets implemented through TVM's BringYourOwnCodegen framework.

* (7) Compile down to machine code.

TVM can compile modules down to linkable object module, can then be run with a lightweight TVM runtime that provides C APIs (to dynamically load the model, & entry points for other languages as Python & Rust).

It can also build a bundled deployment with runtime & model combined in a single package.


### Installing TVM

* From Source: Build shared library & setup for language package. [Details.](https://tvm.apache.org/docs/install/from_source.html#install-from-source)

* From Package: `TLCPack` is 3rd-party community package


### Compiling and Optimizing a Model with TVMC; w/ example

* `TVMC`, the TVM tool exposing features auto-tuning, compiling, profiling & model execution. Available as `tvmc <options>` and `python -m tvm.driver.tvmc <options>`. Main subcommands being `compile`, `run` & `tune`. Supports models created with ONNX, TensorFlow, TFLite & Torch. Use option `--model-format` if need to notify.

* E.g. will use ResNet-50 v2. A 50 layer CNN to classify images, pre-trained on more than million images with 1K+ classifications. Input image size of `224x224`. [Netron](https://github.com/lutzroeder/netron) is a model visualizer.

```
wget https://github.com/onnx/models/raw/b9a54e89508f101a1611cd64f4ef56b9cb62c7cf/vision/classification/resnet/model/resnet50-v2-7.onnx
```

> TVM relies on ONNX lib. Can install ONNX with `pip3 install --user onnx onnxoptimizer`.

* Then compile model to a dynamic library for target platform. Can run model on target device using TVM runtime. Output is a tar package.

```
tvmc compile --target 'llvm' --input-shapes 'data:[1,3,224,224]' \
    --output resnet50-v2-7-tvm.tar resnet50-v2-7.onnx

mkdir model && tar -xvf resnet50-v2-7-tvm.tar -C model && ls model
```

> * `mod.so` is model as C++ lib usable via TVM runtime; `mod.json` representation of TVM Relay computation graph; `mod.params` a file containing params for pre-trained model
> * Correct `--target` option can optimize well for h/w features. [Ref: x86](https://tvm.apache.org/docs/how_to/tune_with_autotvm/tune_relay_x86.html#tune-relay-x86).

* TVMC has TVM runtime built-in, can run produced model & make predictions. Models have specific tensor shapes, formats & data types. TVMC uses NumPy's `.npz` for both input & output data.

> [preprocess.py](./preprocess.py) resize image & output as NumPy array

* With input data pre-processed, can make prediction with TVMC as below; outputs `predictions.npz`.

```
tvmc run --inputs imagenet_cat.npz --output predictions.npz resnet50-v2-7-tvm.tar
```

* Need post-processing to render outputs from ResNet-50 v2 into more human-readable form, using lookup-table provided for model. Script below extract labels from output of compiled module.

> [postprocess.py](./postprocess.py) downloads label list & outputs prediction NumPy array


#### Automatically Tuning the ResNet Model

* Previous model had no platform optimization. Auto-tuner differs from fine-tuning as doesn't affect model accuracy. Results of runs are stored in a tuning records file.

* Tuning requires 3 things: target spec of device, path to tuning records file and path to model to be tuned.

```
## default search algo requires 'xgboost'
pip install xgboost
tvm tune --target 'llvm' --output resnet50-v2-7-autotuner_records.json resnet50-v2-7.onnx
```

> * Providing more specific options would tune better. On an Intel i7 processor could use `--target llvm -mcpu=skylake`. Can take several hours.
> * By default search is guided using `XGBoost Grid` algo. Based on model's complexity & time, might wanna choose another algo. For list use `tvmc tine --help`.
> * `--repeat` & `--number` to provide number of repititions.
> * Can tune specific tasks, to not waste time simple workloads. `-task` to list available.


#### Compiling an Optimized Model with Tuning Data

* Obtained tuning records `resnet50-v2-7-autotuner_records.json` can be used to further tune or direct input to compiler. To generate high-perf target-specific code for model `tvmc compile --tuning-records`.

```
# re-compile model using optimized operators
tvmc compile --target 'llvm' --tuning-records resnet50-v2-7-autotuner_records.json \
    --output resnet50-v2-7-tvm_autotuned.tar resnet50-v2-7.onnx

# verify model runs
tvmc run --inputs imagenet_cat.npz --output predictions.npz resnet50-v2-7-tvm_autotuned.tar

python postprocess.py   # check predictions to be same
```


#### Comparing Tuned & Untuned Models

* TVMC allows basic perf benchmarking between models. Execution time summary from below shows faster perf by tuned model.

```
tvmc run --inputs imagenet_cat.npz --output predictions.npz --print-time --repeat 100 resnet50-v2-7-tvm_autotuned.tar

tvmc run --inputs imagenet_cat.npz --output predictions.npz --print-time --repeat 100 resnet50-v2-7-tvm.tar
```


### Getting Started using TVMC Python: a high-level API for TVM

* Download ResNet model as above; and `mv resnet50-v2-7.onnx my_model.onnx`.

> [tvmcpythonintro.py](./tvmcpythonintro.py) loads model, compile, run & tune

* All frameworks support shape overwrite with `shape_dict` arg optional for most but mandatory for PyTorch.

> *Neutron* to see model's input/`shape_dict`.

* To compile a model `tvm.target` string is required. Some e.g. `cuda` (Nvidia), `llvm` (CPU), `llvm -mcpu=cascadelake` (Intel CPU).

* TVMC can run compiled package on h/w target (CPU, Cude, CL, Metal, Vulkan).

* Run speed can improve with tuning. Optional tuning tries looking for optimal way for each operation within model (done via Cost Model & Benchamrking possible schedule). Can take hours.

> * There are 2 ways to save tuning results

```
## method.1
def tune_1(model, log_file):
    # run tuning
    tvmc.tune(model, target='llvm', tuning_records=log_file)
    # ...
    # later, run tuning & reuse results
    tvmc.tune(model, target='llvm', prior_records=log_file)

## method.2
def tune_2(model):
    # run tuning
    tuning_records = tvmc.tune(model, target='llvm')
    # ...
    # later, run tuning & reuse results
    tvmc.tune(model, target='llvm', prior_records=tuning_records)
```

> * For tuning complex models might see `..T..` in log for need to increase searching time frame with `tvmc.tune(model,trials=10000,timeout=10,)`.

> * Tuning results to be provided in `tvm.compile(..., tuning_records='records.log')`, to apply them.


* Search space of schedules is autogen with `tvm.tune(..., enable_autoscheduler=True)`

* To make things faster for later, can save Relay version (from step1) with `model.save(new_model_path)`.

* To save compile package (from step2) use as below

```
tvmc.compile(model, target='llvm', package_path='some_value')

new_package = tvmc.TVMCPackage(package_path='some_value')
result = tvmc.run(new_package, device='cpu')
```

* Can use TVM RPC setup to compile model for a remote device, need target device with [RPC Server setup](https://tvm.apache.org/docs/tutorials/get_started/cross_compilation_and_rpc.html).

```
tvmc.tune(model, target='llvm', target_host=TARGET_HOST_PROCESSOR, hostname=HOST_IP, port=9090_OR_CUSTOM, rpc_key=YOUR_KEY)
```


---

