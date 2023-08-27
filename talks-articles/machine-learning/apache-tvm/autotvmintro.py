#!python3

import onnx
from tvm.contrib.download import download_testdata
from PIL import Image
import numpy as np
import tvm.relay as relay
import tvm
from tvm.contrib import graph_executor
import tvm.auto_scheduler as auto_scheduler
from tvm.autotvm.tuner import XGBTuner, GATuner, RandomTuner, GridSearchTuner
from tvm import autotvm

from scipy.special import softmax


def load_onnx_model(url, model_name):
    model_path = download_testdata(url, model_name, module='onnx')
    return onnx.load(model_path)


def load_image(url, img_file):
    img_path = download_testdata(img_url, img_file, module="data")
    # Resize it to 224x224
    resized_image = Image.open(img_path).resize((224, 224))
    img_data = np.asarray(resized_image).astype("float32")
    # Our input image is in HWC layout while ONNX expects CHW input, so convert the array
    img_data = np.transpose(img_data, (2, 0, 1))
    # Normalize according to the ImageNet input specification
    imagenet_mean = np.array([0.485, 0.456, 0.406]).reshape((3, 1, 1))
    imagenet_stddev = np.array([0.229, 0.224, 0.225]).reshape((3, 1, 1))
    norm_img_data = (img_data / 255 - imagenet_mean) / imagenet_stddev
    # Add the batch dimension, as we are expecting 4-dimensional input: NCHW.
    return np.expand_dims(norm_img_data, axis=0)


def compile_model(model, shape_dict, target):
    mod, params = relay.frontend.from_onnx(model, shape_dict)
    with tvm.transform.PassContext(opt_level=3):
        lib = relay.build(mod, target=target, params=params)
    dev = tvm.device(str(target), 0)
    return (
        mod,
        params,
        graph_executor.GraphModule(lib['default'](dev))
    )


def execute(module, input_name, img_data):
    dtype = "float32"
    module.set_input(input_name, img_data)
    module.run()
    output_shape = (1, 1000)
    return module.get_output(0, tvm.nd.empty(output_shape)).numpy()


def perf_data(module):
    import timeit
    timing_number = 10
    timing_repeat = 10
    unoptimized = (
        np.array(
            timeit.Timer(lambda: module.run()).repeat(repeat=timing_repeat, number=timing_number)
        ) * 1000 / timing_number
    )
    print({
        'mean': np.mean(unoptimized),
        'median': np.median(unoptimized),
        'std': np.std(unoptimized),
    })


def post_process(tvm_output, labels_url, labels_name):
    # Download a list of labels
    labels_path = download_testdata(labels_url, labels_name, module="data")

    with open(labels_path, "r") as f:
        labels = [l.rstrip() for l in f]

    # Open the output and read the output tensor
    scores = softmax(tvm_output)
    scores = np.squeeze(scores)
    ranks = np.argsort(scores)[::-1]
    for rank in ranks[0:5]:
        print("class='%s' with probability=%f" % (labels[rank], scores[rank]))


def autotune_model(mod, target, params, tuner):
    # create a TVM runner
    runner = autotvm.LocalRunner(
        number=10,
        repeat=1,
        timeout=10,  # in seconds
        min_repeat_ms=0,  # since we're tuning on a CPU, can be set to 0
        enable_cpu_cache_flush=True,
    )
    tuning_option = {
        "tuner": tuner,
        "trials": 20,
        "early_stopping": 100,
        "measure_option": autotvm.measure_option(
            builder=autotvm.LocalBuilder(build_func="default"),
            runner=runner,
        ),
        "tuning_records": "resnet-50-v2-autotuning.json",
    }
    # begin by extracting the tasks from the onnx model
    tasks = autotvm.task.extract_from_program(mod["main"], target=target, params=params)
    # Tune the extracted tasks sequentially.
    for i, task in enumerate(tasks):
        prefix = "[Task %2d/%2d] " % (i + 1, len(tasks))
        # create tuner
        if tuner == "xgb":
            tuner_obj = XGBTuner(task, loss_type="reg")
        elif tuner == "xgb_knob":
            tuner_obj = XGBTuner(task, loss_type="reg", feature_type="knob")
        elif tuner == "xgb_itervar":
            tuner_obj = XGBTuner(task, loss_type="reg", feature_type="itervar")
        elif tuner == "xgb_curve":
            tuner_obj = XGBTuner(task, loss_type="reg", feature_type="curve")
        elif tuner == "xgb_rank":
            tuner_obj = XGBTuner(task, loss_type="rank")
        elif tuner == "xgb_rank_knob":
            tuner_obj = XGBTuner(task, loss_type="rank", feature_type="knob")
        elif tuner == "xgb_rank_itervar":
            tuner_obj = XGBTuner(task, loss_type="rank", feature_type="itervar")
        elif tuner == "xgb_rank_curve":
            tuner_obj = XGBTuner(task, loss_type="rank", feature_type="curve")
        elif tuner == "xgb_rank_binary":
            tuner_obj = XGBTuner(task, loss_type="rank-binary")
        elif tuner == "xgb_rank_binary_knob":
            tuner_obj = XGBTuner(task, loss_type="rank-binary", feature_type="knob")
        elif tuner == "xgb_rank_binary_itervar":
            tuner_obj = XGBTuner(task, loss_type="rank-binary", feature_type="itervar")
        elif tuner == "xgb_rank_binary_curve":
            tuner_obj = XGBTuner(task, loss_type="rank-binary", feature_type="curve")
        elif tuner == "ga":
            tuner_obj = GATuner(task, pop_size=50)
        elif tuner == "random":
            tuner_obj = RandomTuner(task)
        elif tuner == "gridsearch":
            tuner_obj = GridSearchTuner(task)
        else:
            raise ValueError("Invalid tuner: " + tuner)

        tuner_obj.tune(
            n_trial=min(tuning_option["trials"], len(task.config_space)),
            early_stopping=tuning_option["early_stopping"],
            measure_option=tuning_option["measure_option"],
            callbacks=[
                autotvm.callback.progress_bar(tuning_option["trials"], prefix=prefix),
                autotvm.callback.log_to_file(tuning_option["tuning_records"]),
            ],
        )

    return tuning_option['tuning_records']




if __name__ == '__main__':
    # # download & load onnx model
    model_url = (
        "https://github.com/onnx/models/raw/main/"
        "vision/classification/resnet/model/"
        "resnet50-v2-7.onnx"
    )
    onnx_model = load_onnx_model(model_url, "resnet50-v2-7.onnx")

    # * Seed numpy's RNG to get consistent results
    np.random.seed(0)

    # # download, preprocess and load test image
    img_url = "https://s3.amazonaws.com/model-server/inputs/kitten.jpg"
    img_file = "imagenet_cat.png"
    img_data = load_image(img_url, img_file)

    # * input_name may vary across model types
    #   you can use a tool like Netron to check input names
    input_name = 'data'
    shape_dict = {input_name: img_data.shape}
    target = 'llvm'

    (mod, params, module) = compile_model(onnx_model, shape_dict, target)

    tvm_output = execute(module, input_name, img_data)

    perf_data(module)

    labels_url = "https://s3.amazonaws.com/onnx-model-zoo/synset.txt"
    post_process(tvm_output, labels_url, "synset.txt")

    # choose tuner
    tuner = "xgb"

    tuning_records = autotune_model(mod, target, params, tuner)

    ## compile & optimize model with tuning data
    ## using resnet-50-v2-autotuning.json
    with autotvm.apply_history_best(tuning_option["tuning_records"]):
        with tvm.transform.PassContext(opt_level=3, config={}):
            lib = relay.build(mod, target=target, params=params)

    dev = tvm.device(str(target), 0)
    module = graph_executor.GraphModule(lib["default"](dev))

    ## verify optimized model & results
    tvm_output = execute(module, input_name, img_data)
    post_process(tvm_output, labels_url, "synset.txt")
    perf_data(module)
