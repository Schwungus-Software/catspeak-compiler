# catspeak-compiler

A WIP compiler for the [Catspeak](https://github.com/katsaii/catspeak-lang) modding language.

## Usage

```bash
catspeak-compiler file1.txt file2.txt
```

The command above will produce files `file1.ast.json`, `file2.ast.json`, `file1.ir.json`, and `file2.ir.json`. The latter two are currently stubs. The AST and the IR files reside in the same directory as the original scripts.
