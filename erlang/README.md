# Brainfuck Erlang

## Compile and Run
```bash
erl -compile ./*.erl && erl -noshell -run main main "../samples/hello-world.bf" ""
```

## Check the `erl` version
```bash
erl -version
```

## Test
```bash
erl -compile ./*.erl && erl -noshell -run tests main  
```

## Notes
- [Get single character from input](https://stackoverflow.com/questions/42750491/read-a-character-input-from-erlang-without-requiring-the-return-key-pressed-from)