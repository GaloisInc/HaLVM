Halfs Examples
===

## Library
This is intended to be used when Halfs's HaLVM compatible branch is built and installed to HaLVM.

Checkout the [main branch of Halfs]()

Checkout the [HaLVM version of Halfs]()

After installation, you can `make` and `make run` here like other examples.

## Examples
Due to the lack of file system, so although the `File.hs` is reserved in `System/Device`, it will not be used. Instead, we will use the memory backed block device model as well as the ST-backed. They are all arrays of memory in fact, to simulate the FS concepts such as "sectors" etc.