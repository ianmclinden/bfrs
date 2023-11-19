# bfrs

A [brainfuck](https://esolangs.org/wiki/Brainfuck) interpreter, written in rust.

---

## Usage

To run in the interpreter in interactive mode, run `bfrs`. This will open a python-like prompt loop, where every new line entered will be executed as its own brainfuck program. Input/output will be read from/printed to the prompt. For example
```
$ bfrs
bfrs 0.1.0
Ctrl+C, or type 'q', 'quit' to quit.
>>> [echo input characters],[.,]
hi
hi
```

To execute a brainfuck file, pass it as an argument to the interpreter.
```
# Run a simple brainfuck program
bfrs my_file.bf

# Run a program, with pre-populated input
bfrs my_file.bf <<< "hi, program"
echo "hi, program" | bfrs my_file.bf
```

## License

Software is provided under the MIT License.