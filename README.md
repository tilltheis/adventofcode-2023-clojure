# Advent of Code 2023 in Clojure

Developed using JVM 21 and Clojure 1.11.

## Running

```shell
$ clj -M:solve
Usage: clj -M:solve <day> <part> <puzzle-input-path>
Example: clj -M:solve 1 2 resources/input_01.txt
```

You have to supply the puzzle input files yourself.

## Testing

```shell
$ clj -X:test
```

## Profiling

```shell
$ clj -M:profile
Usage: clj -M:profile <day> <part> <puzzle-input-path>
Example: clj -M:profile 1 2 resources/input_01.txt
```

Same interface as `clj -M:solve`.
Writes performance profiles to `/tmp/clj-async-profiler/results/` in HTML format.

On Linux you have to adjust some kernel parameters to make profiling work:

```shell
sudo sysctl -w kernel.perf_event_paranoid=1
sudo sysctl -w kernel.kptr_restrict=0
```
