# HGDB RTL
This is an experimental tool to generate symbol table from SystemVerilog designs.
It uses [slang](https://github.com/MikePopoloski/slang) as a frontend to parse any SystemVerilog files.

## How to install
TO install, simply run the following commands

```
pip install hgdb-rtl
```

`hgdb-rtl` will show up in your path.

## Usage
`hgdb-rtl` shares the same command line options as `slang`. Here is an example command to generate a symbol table
called `debug.db`.

```
hgdb-trl design1.sv design2.sv tb.sv -o debug.db
```

You can see more options using the `-h` flag.

## Limitations
Since SystemVerilog is a complex language, there are several limitation in the current implementation.
Notice that none of them are systematic - given enough engineering effort, they can be overcome.

1. No SSA transformation for `always_comb` logic. This means that if you reassign the same variable
   in the same `always_comb` block, the intermediate result will not show in during debugging.
2. Local variables declared in the procedural block will not be detected.

## Use it with hgdb
The flow is identical to hardware generators. You need to use the symbol table and turn on proper simulator flags,
as described [here](https://hgdb.dev).
