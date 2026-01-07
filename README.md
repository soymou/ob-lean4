# ob-lean4

Org-babel execution for [Lean 4](https://leanprover.github.io/).

## Features

- **Lean 4 Execution**: Run Lean 4 code blocks directly from Org-mode.
- **Session Management**: Maintain state across multiple code blocks using the `:session` header.
- **Output Isolation**: Each block in a session only displays its own output, even though it has access to definitions from previous blocks.
- **Import Handling**: Automatically extracts and merges `import` statements at the top of the generated file to ensure they work correctly across sessions.
- **Tangling**: Support for tangling code blocks into `.lean` files.
- **Session Control**: Commands to restart specific or all active sessions.

## Installation

1. Clone this repository:
   ```bash
   git clone https://github.com/youruser/ob-lean4.git
   ```

2. Add the following to your Emacs configuration:
   ```elisp
   (add-to-list 'load-path "/path/to/ob-lean4")
   (require 'ob-lean4)

   ;; Optional: Add to org-babel-do-load-languages
   (org-babel-do-load-languages
    'org-babel-load-languages
    '((lean4 . t)))
   ```

## Usage

### Simple Block
```org
#+BEGIN_SRC lean4
def hello := "Hello, Lean 4!"
#eval hello
#+END_SRC
```

### Sessions
Using sessions allows you to define functions or variables in one block and use them in another.

```org
#+BEGIN_SRC lean4 :session mysess
def x := 42
#+END_SRC

#+BEGIN_SRC lean4 :session mysess
#eval x + 8
#+END_SRC
```

### Commands

- `M-x ob-lean4-restart-session-command`: Restarts the session associated with the current code block.
- `M-x ob-lean4-restart-all-sessions-command`: Clears all active Lean 4 sessions.

## Configuration

- `ob-lean4-command`: The command used to run Lean (defaults to `"lean"`).

## License

MIT
