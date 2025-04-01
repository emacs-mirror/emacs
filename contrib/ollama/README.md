# lem-ollama

A client for interacting with LLMs

## Setup

in ``~/.config/lem/init.lisp``

If you aren't running Ollama on localhost, specify a server:

```lisp
(setf lem-ollama:*host* "your.machine:1234")
```

You can also specify the model used:

```lisp
(setf lem-ollama:*model* "llama2")
```

## run-ollama

Start an listener-mode buffer talking to the LLM.

## ollama-prompt

Send a prompt and stream the response into a temp buffer.

## ollama-close

Close the current response stream, bound to C-c C-c by default.