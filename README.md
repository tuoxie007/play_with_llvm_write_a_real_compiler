# play_with_llvm_write_a_real_compiler

A demo about how to build a real compiler with LLVM libraries, write for the book https://github.com/tuoxie007/play_with_llvm .

# How to build and run the compiler

```sh
$ git clone https://github.com/tuoxie007/play_with_llvm_write_a_real_compiler
$ cd play_with_llvm_write_a_real_compiler
$ cd play
$ ./build.sh
$ ./play
```

Then a `.o` file will be wrote in `tests/`.

# How to run the tests

1. Open the startup file `play/cli.cpp`.
2. Change the macro `TEST`.
3. Rebuild and run.

```cpp
#define TEST "int_indexer"
//#define TEST "int_pointer_arg"
//#define TEST "delete_ptr"
```

# How to write your test case

1. Write a test file in directory `play/tests`.
2. Save it with extension `.play`.
3. Change the macro `TEST` in `play/cli.cpp`.
4. Rebuild and run.

Have fun!