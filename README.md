# catspeak-compiler

A WIP compiler for the [Catspeak](https://github.com/katsaii/catspeak-lang) modding language.

The compiler is currently able to parse a limited subset of the language and convert it to Catspeak IR in JSON format.

This project is in its infancy; expect breaking bugs and breaking changes. Catspeak will not run all code compiled by this program, and not all Catspeak code can be compiled either.

The project is not tied to a specific release of Catspeak yet since its API is changing constantly. As such, it may become outdated overnight.

## Usage

```bash
catspeak-compiler file1.txt file2.txt
```

The command above will produce files `file1.ast.json`, `file2.ast.json`, `file1.ir.json`, and `file2.ir.json`. The AST and the IR files reside in the same directory as the original scripts.

**WARNING**: the IR files cannot be read properly with the built-in `json_parse` function since it converts `null` fields to null pointers rather than the `undefined` Catspeak expects. As of now, you should use [SNAP](https://github.com/JujuAdams/SNAP) by JujuAdams for parsing JSON.
