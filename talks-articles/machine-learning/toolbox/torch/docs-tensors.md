
## torch.tensor

> [source](https://pytorch.org/docs/stable/tensors.html); [class reference, methods](https://pytorch.org/docs/stable/tensors.html#tensor-class-reference)

> a multi-dimensional matrix containing elements of a single data type

* defines 10 tensor types with CPU & GPU variants

> * 64bit float: torch.{float32, float} ; cpu: torch.FloatTensor; cuda: torch.cuda.FloatTensor
> * 32bit float: .{float64, double} ; cpu: .DoubleTensor; cuda: .cuda.DoubleTensor
> * 16bit float: .{float16, half} ; cpu: .HalfTensor ; cuda: .cuda.HalfTensor
> * 16bit float: .{bfloat16} ; cpu: .BFloat16Tensor ; cuda: .cuda.BFloat16Tensor

> * 32bit complex: toch.complex32 or torch.chalf
> * 64bit complex: toch.complex64 or torch.cfloat
> * 128bit complex: toch.complex128 or torch.cdouble

> * torch.bool

> * quantized 8-bit int; unsigned: torch.quint8 w/ cpu: torch.ByteTensor; signed: torch.qint8 w/ cpu: torch.CharTensor
> * quantized 32-bit int; singed: torch.qint32 w/ cpu: torch.IntTensor
> * quantized 4-bit int; unsigned: torch.quint4x2 w/ cpu: torch.ByteTensor

> `torch.Tensor` alias for default type `torch.FloatTensor`

* constructed from a Python list/sequence with `torch.tensor`, it always copies the data

```
lst_of_vectors = [[10, 640, 480], [12, 1024, 768]]
tensorX = tensor.tensor(lst_of_vectors)
tensorY = tensor.tensor(np.array(lst_of_vectors))
```

> * with NumPy array to avoid a copy use `torch.as_tensor`; preserves [autograd history](https://pytorch.org/docs/stable/notes/autograd.html) if possible
> * specific type tensor can be constructed by passing `torch.dtype` &/or `torch.device` as

```
tensorZ = torch.zeros([3, 10], dtype=torch.int32)
tensorO = torch.ones([3, 10], dtype=torch.half, device=torch.device('cuda:0'))
```

> * when created with `requires_grad=True`, records op for automatic differentiation
> * access values via `torch.Tensor.item()`
> * each tensor has attached `tensor.Storage`

* methods on tensor compute result in a new tensor like `torch.FloatTensor.abs`; those suffixed with underscore mutate in-place like `torch.FloatTensor.abs_`

---
