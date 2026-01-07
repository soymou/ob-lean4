(defconst ob-lean4-def-regex
  (concat "^[ 	]*"
          "\\(?:"
            (regexp-opt '("unsafe" "partial" "noncomputable") t) "[ 	]+\\)?"
          (regexp-opt '("def" "theorem" "lemma" "example" "structure" "inductive" "class" "abbrev" "axiom" "opaque") t)
          "[ 	]+\\([^ 	
({]+\\)"))

(defun ob-lean4--extract-def-name (block)
  "Extract the name of the definition in BLOCK, if any."
  (if (string-match ob-lean4-def-regex block)
      ;; Group 1: Modifiers (optional)
      ;; Group 2: Keyword
      ;; Group 3: Name
      (match-string 3 block)
    nil))

(defun test-extract ()
  (dolist (code '("def foo := 1" 
                  "  partial def bar (x : Nat) := x" 
                  "structure MyStruct where"
                  "#eval 1"
                  "def joinStringsWith (s1 s2 : String) := ..""))
    (message "Code: '%s' -> Name: '%s'" code (ob-lean4--extract-def-name code))))

(test-extract)