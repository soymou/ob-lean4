# ob-lean4

Org-Babel support for Lean 4.

## Features

- **Standalone Execution**: Run Lean 4 code blocks directly using `lean --run`.
- **Project Awareness**: Automatically detects `lakefile.lean` or `lakefile.toml` and uses `lake env lean --run` to ensure dependencies are available.
- **Imports Helper**: Use `:imports` header argument to easily import modules without cluttering the code block.

## Installation

### Manual

1. Clone this repository.
2. Add the directory to your `load-path`.
3. Require `ob-lean4`.

```elisp
(add-to-list 'load-path "/path/to/ob-lean4")
(require 'ob-lean4)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((lean4 . t)))
```

### use-package

```elisp
(use-package ob-lean4
  :load-path "/path/to/ob-lean4"
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((lean4 . t))))
```

## Configuration

- `ob-lean4-command`: Path to the `lean` executable (default: "lean").
- `ob-lean4-lake-command`: Path to the `lake` executable (default: "lake").

## Usage

### Standalone Snippet

```org
#+begin_src lean4 :results output
def main : IO Unit :=
  IO.println "Hello, World!"
#+end_src
```

### Project Context

If your Org file is inside a Lean 4 project (contains `lakefile.lean`), `ob-lean4` automatically detects it.

```org
#+begin_src lean4 :results output :imports "MyProjectLib"
def main : IO Unit :=
  IO.println MyProjectLib.someFunction
#+end_src
```

### Remote Execution

You can execute code in the context of a project from anywhere using `:dir`.

```org
#+begin_src lean4 :dir ~/projects/my-lean-project :imports "MyProjectLib"
def main : IO Unit :=
  IO.println "Running inside the project!"
#+end_src
```

### Imports

Use the `:imports` header argument to specify modules to import.

```org
#+begin_src lean4 :imports "Init Data.List"
def main : IO Unit :=
  IO.println [1, 2, 3]
#+end_src

### Multi-file Projects (Literate Programming)

If you are defining a multi-file project within Org (using `:tangle`), you need to ensure dependencies are compiled before running code that imports them.

1.  Use `:tangle` to save your files.
2.  Define a `lakefile.lean` with `@[default_target]` on your library so `lake build` picks it up.
3.  Use `:build yes` on the block you want to execute to trigger a build first.

```org
* Configuration
#+begin_src lean4 :tangle lakefile.lean
import Lake
open Lake DSL

package myproj

@[default_target]
lean_lib MyLib {
  srcDir := "."
}
#+end_src

* Module
#+begin_src lean4 :tangle MyLib.lean
def msg := "Hello"
#+end_src

* Run
#+begin_src lean4 :imports "MyLib" :build yes
def main : IO Unit :=
  IO.println msg
#+end_src
```

## Configuration

- `ob-lean4-command`: Path to the `lean` executable (default: "lean").
```
