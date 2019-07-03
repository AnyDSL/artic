# Proposal for new syntax

This document is a proposal for the new syntax of Impala.
Comments are welcome and the discussion should focus on a solution that balances language expressiveness and power, with implementation difficulty.

## Closure conversion

In the case where Impala cannot or should not perform specialization, some functions will escape with their environment, as in:

```rust
fn f() -> fn () -> () {
    let mut x = 0;
    || { x = 5 }
}
```

When `f` is inlined, the lifetime of `x` gets extended to the lifetime of the enclosing scope.
For the behaviour of the program to be identical with or without inlining, we need garbage collection.
When f is not inlined, the garbage collector needs to allocate a cell for `x` and allocate a closure for the returned function.
For this to work, we need a reliable closure conversion phase, along with a garbage collection system.

## Device/Host Interfaces

There is the problem of device/host interfaces:

```rust
fn main(ptr: &mut [i32]) -> () {
    let grid  = /* ... */;
    let block = /* ... */;
    with work_item in nvvm(grid, block) {
        let id = work_item.gidx();
        ptr(id) = 5;
    }
}
```

In this example compiled with the _existing_ version of impala, if the pointer does not reside in device memory, the program will crash with an obscure error message.
Indeed, there is no check to ensure that `ptr` is inside device memory.
Therefore, the runtime system should have a way to determine where a pointer is, and _automatically_ transfer data from device to host.
Relying on the user specifying when to transfer data is a bad idea. Consider the following code:

Module 1:
```rust
// Device-independent sort function
fn sort_data(data: &mut [Data], len: i32) -> () {
    // ...
}
```

Module 2:
```rust
fn sort_data_on_gpu(data: &mut [Data], len: i32) -> () {
    // Is 'data' on the GPU? No idea.
    // Will just alloc a copy and work on it.
    let data_gpu = alloc_gpu[Data](i32);
    copy(data, data_gpu); // copy host->device
    with work_item in nvvm(/* ... */) {
        sort_data(data_gpu, len);
    }
    copy(data_gpu, data); // copy device->host
}
```

Module 3:
```rust
fn use_sort_on_gpu_twice(len: i32) {
    let data = alloc_gpu[Data](len);
    fill_data_gpu(data);
    sort_data_on_gpu(data, len); // used once
    /* ... */
    sort_data_on_gpu(data, len); // used once more (copying is then wasteful)
}
```

For a program to be modular, functionality should be separated, but here, we do not know where pointers reside.
Therefore, in `sort_data_on_gpu` we have to copy data to another buffer on the GPU, which can be wasteful if sorting is not the only operation that we do on the GPU with this data.
Consequently, automatic data transfer is essential to modularity.
It has the additional benefit of making programs simpler, and more importantly, letting the user choose the device on which data resides at run time!

In contrast, encoding the device on which a pointer resides (imagine something like `&[i32] for nvvm` for a pointer to an array of integer for the `nvvm` backend) in the type system is severely flawed.
The example above would be rewritten in the following (hypothetical) style:

Module 1:
```rust
// Device-independent sort function, needs the device as type argument
fn sort_data[dev: device](data: &mut [Data] for dev, len: i32) -> () {
    // ...
}
```

Module 2:
```rust
fn sort_data_on_gpu[dev: device](data: &mut [Data] for dev, len: i32) -> () {
    // Is 'data' on the GPU? No idea.
    // Will just alloc a copy and work on it.
    if dev != nvvm {
        let data_gpu = alloc_gpu[nvvm, Data](i32);
        copy(data, data_gpu); // copy host->device
        with work_item in nvvm(/* ... */) {
            sort_data(data_gpu, len);
        }
        copy(data_gpu, data); // copy device->host
    } else {
        with work_item in nvvm(/* ... */) {
            // Explicit cast required to say that dev == nvvm to the type system
            sort_data(data as &mut [Data] for nvvm, len);
        }
    }
}
```

There are two major problems: First, this does not solve the wasteful copying mentioned above. Second, this introduces code duplication (`with work_item in ...` is present twice).
To fix this code duplication, we would have to rely on explicit type casts:

Module 2:
```rust
fn sort_data_on_gpu[dev: device](data: &mut [Data] for dev, len: i32) -> () {
    let mut data_on_nvvm = data; // types as &mut [Data] for dev
    if dev != nvvm {
        let data_gpu = alloc_gpu[nvvm, Data](i32); // types as &mut [Data] for nvvm
        copy(data, data_gpu); // copy host->device
        // This assignment would fail
        // data_on_nvvm_or_not = data_gpu;
        // Because:
        // &mut [Data] for nvvm != &mut [Data] for dev
        // We need an explicit cast:
        data_on_nvvm_or_not = data_gpu as &mut [Data] for dev;
    }

    with work_item in nvvm(/* ... */) {
        sort_data(data_on_nvvm as &mut [Data] for nvvm, len);
    }

    if dev != nvvm {
        copy(data_on_nvvm, data); // copy device->host
    }
}
```

Looking at this code, it appears clearly that this solution does not scale with more complex kernels and several input/output buffers.
Additionally, a type-system based approach requires an effect system too, so as to prevent the program from being correct:

```rust
fn main(ptr: &mut [i32] for nvvm) -> () {
    // 'ptr' is on a nvvm device, not on the host: error!
    ptr(0) = 5;
}
```

This means that operations with side effects now have an additional _effect_ type: The assignment `x(index) = 5` should have type `() for nvvm` if `x` is of type `&[i32] for nvvm`.
The additional complexity and the fact that not all problems are fixed by this approach makes this solution impractical at best.

Instead of relying on types, we suggest to implement a Garbage Collection (GC) system in the runtime. This GC is anyway necessary for closures (see point above).
In practice, this means that:

1. Memory is allocated with a `alloc()`, a function that allocates memory on the *host*,
2. When a device region is encountered, such as with `nvvm` or `cuda`, the compiler should call the GC/runtime to translate pointers from the host to the device,
3. The GC should be informed of the type of accesses done by the kernel on the device, so that it knows if the memory is dirty and needs to be copied back to the device or not.
4. The garbage collector should be controllable from Impala, so that the programmer can decide to only transfer part of an array, for instance

With this approach, the following code will work out the box and have the expected behaviour.

```
fn main() -> () {
    let mut a = 5;
    with work_item in nvvm(grid, block) {
        a = 3;
    }
}
```

To improve performance, _escape analysis_ should be run on mutable variables to determine when they can live on the stack, and when they cannot.

> In my opinion, point 4 (manual control from Impala) is mandatory for many application scenarios like double buffering, data doesn't fit on device, or when only a part of the data is required. Which makes this is a show stopper if we don't have it.

> I feel this point is important too, but I also see that we do not have the need for this _right now_. Additionally, double buffering should work without it, unless you want something much more complicated than:

```rust
fn main(n: i32) -> () {
    // allocate two buffers
    let bufs = [alloc[i32](n), alloc[i32](n)];
    for j in range(0, 10) {
        // choose buffers depending on iteration
        let buf1 = bufs[j & 1];
        let buf2 = bufs[(j + 1) & 1];
        with work_item in nvvm(/* ... */) {
            let id = work_item.gidx();
            buf1(id) = buf2(id);
        }
    }
}
```

> There are also other important issues like scheduling copies early (e.g. while another kernel is running), or manual triggering of a garbage collection, that will require annotations from the programmer. I believe we can somehow implement a `gc` module that does this.

```rust
import gc;

fn main(n: i32) -> () {
    let buf1 = alloc[i32](n);
    let buf2 = alloc[i32](n);
    gc::collect(); // manual GC collection, frees buf1 as it is not used anymore, but not buf2
    for i in range(0, n) {
        buf2(i) = i;
    }
    gc::update(buf2, nvvm); // update the NVVM version of the buffer right now
    
    // do some other work that *does not* involve buf2
    // ...

    // at this point, the buffer is probably copied already and
    // the runtime only has to translate the pointers, no copy is necessary
    with work_item in nvvm(/* ... */) {
        let id = work_item.gidx();
        buf2(id) = id;
    }
    
    // for a partial copy, we need to do the following:
    gc::mark_as_clean(buf2, nvvm); // tell the GC the NVVM buffer is up-to-date (effectively clear any dirty range)
    gc::mark_as_dirty(buf2, from, to, nvvm); // tell the gc the range [from, to] is dirty on the NVVM device
    gc::update(buf2, nvvm); // perform the copy only on [from, to]
}
```

## Pointers and Garbage Collection

The language should be constrained so that the garbage collection system can work. In particular, we need to prevent taking the address of the field of a structure or the element of an array:

```rust
let mut s = S { a: 1, b: 2 };
f(&s.a) // rejected, because s.a is a field, not the structure itself
f(&s)   // allowed
let mut array = [1, 2, 3, 4];
f(&array(5)) // rejected, cannot take the address of an element
f(&array)    // allowed
```

## Polymorphism

Polymorphism should be added using square brackets to simplify the syntax and disambiguate the expression `x<y>(z)`: If `<` and `>` denote type application, this expression can be taken either as a call to `x` applied with type argument `y` and argument `z`, or as two comparisons `x < y` and `(x < y) > (z)`.
For type inference, we should rely on bidirectional type inference. This method gives better error messages, mixes well with subtyping (required to make mutable pointers compatible with immutable ones), and is relatively easier to implement than algorithm W.
This change will however mean that some programs that could have their types inferred will now no longer type-check:

```rust
fn main() -> () {
    let mut x;
    x = 5;
}
```

In this example, the type of `x` is inferred from its use after the declaration. Since bidirectional type inference is a local type inference algorithm, it will only use the declaration site of `x` to determine its type, and will thus reject this program, since the declaration does not give a type.
We believe this does not happen too often in Impala programs, and the added benefits of getting better error messages and having a more robust type checking algorithm are more important than this simple limitation.
