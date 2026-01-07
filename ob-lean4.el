;;; ob-lean4.el --- Org-babel support for Lean 4

;;; Code:

(require 'org)
(require 'ob)

(defgroup ob-lean4 nil
  "Org-babel support for Lean 4."
  :group 'org-babel)

(defcustom ob-lean4-command "lean"
  "Name of the lean executable."
  :group 'ob-lean4
  :type 'string)

(defcustom ob-lean4-lake-command "lake"
  "Name of the lake executable."
  :group 'ob-lean4
  :type 'string)

(defvar org-babel-default-header-args:lean4 '())

(defconst ob-lean4-command-keywords
  '("def" "theorem" "lemma" "example" "structure" "inductive" "class" "instance" 
    "abbrev" "axiom" "section" "namespace" "open" "variable" "universe" 
    "set_option" "attribute" "macro" "syntax" "elab" "export" "derive" 
    "declare_syntax_cat" "import" "opaque")
  "List of Lean 4 keywords that start a command.")

(defvar ob-lean4-sessions (make-hash-table :test 'equal)
  "Hash table mapping session names to their code history (cons imports body).")

(defun ob-lean4--find-project-root (dir)
  "Find the root of the Lean project containing DIR (looking for lakefile.lean or lakefile.toml)."
  (let ((default-directory dir))
    (if-let ((root (locate-dominating-file dir (lambda (d)
                                                 (or (file-exists-p (expand-file-name "lakefile.lean" d))
                                                     (file-exists-p (expand-file-name "lakefile.toml" d)))))))
        (expand-file-name root)
      nil)))

(defun ob-lean4--has-main (body)
  "Check if BODY contains a main function definition."
  (or (string-match-p "^[ 	]*def[ 	]+main" body)
      (string-match-p "^[ 	]*unsafe[ 	]+def[ 	]+main" body)
      (string-match-p "^[ 	]*partial[ 	]+def[ 	]+main" body)))

(defun ob-lean4--should-eval (body)
  "Check if BODY appears to be an expression that should be wrapped in #eval."
  (let* ((clean-body (replace-regexp-in-string "/-.*?-/" "" 
                                               (replace-regexp-in-string "^[ 	]*--.*$" "" body)))
         (first-word (car (split-string clean-body))))
    (and first-word
         (not (string-prefix-p "#" first-word))
         (not (member first-word ob-lean4-command-keywords)))))

(defun ob-lean4--get-session-data (session)
  "Get the (imports . body) data for SESSION."
  (gethash session ob-lean4-sessions '(() . "")))

(defun ob-lean4--clean-body-for-history (body)
  "Remove transient commands (starting with #) from BODY for session history.
Also attempts to normalize newlines."
  (with-temp-buffer
    (insert body)
    (goto-char (point-min))
    ;; Remove lines starting with # (eval, check, print, reduce, etc.)
    ;; We use ^[ 	]*# to match indentation.
    (flush-lines "^[ 	]*#")
    ;; Remove leading/trailing whitespace
    (let ((clean (string-trim (buffer-string))))
      clean)))

(defun ob-lean4--update-session-data (session imports body)
  "Update the SESSION with new IMPORTS and BODY."
  (let* ((old-data (ob-lean4--get-session-data session))
         (old-body (cdr old-data))
         (clean-body (ob-lean4--clean-body-for-history body)))
    
    (unless (string-empty-p clean-body)
      ;; Avoid appending if the exact same block is already at the end of history.
      ;; This is a simple heuristic to prevent accidental re-runs of the same definition block.
      (let ((should-append 
             (or (string-empty-p old-body)
                 (not (string-suffix-p clean-body (string-trim old-body))))))
        
        (if should-append
            (puthash session 
                     (cons imports (if (string-empty-p old-body)
                                       clean-body
                                     (concat old-body "\n\n" clean-body)))
                     ob-lean4-sessions)
          ;; If we didn't append body, we still might need to update imports
          (puthash session (cons imports old-body) ob-lean4-sessions))))))

(defun ob-lean4-reset-session (&optional session)
  "Reset the Lean 4 session history.
If SESSION is not provided, resets the 'default' session."
  (interactive "sSession name to reset (default): ")
  (let ((sess (if (or (null session) (string-empty-p session)) "default" session)))
    (remhash sess ob-lean4-sessions)
    (message "Lean 4 session '%s' cleared." sess)))

(defun org-babel-lean4-initiate-session (&optional session _params)
  "Initiate a Lean 4 session.
Since Lean 4 does not support a persistent REPL easily, this just returns nil
or the session name to satisfy Org Babel, while the state is managed internally."
  session)

(defun org-babel-execute:lean4 (body params)
  "Execute a block of Lean 4 code with Babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((dir (expand-file-name (or (cdr (assq :dir params)) default-directory)))
         (project-root (ob-lean4--find-project-root dir))
         (session (cdr (assq :session params)))
         (is-session (and session (not (string= session "none"))))
         (current-imports (if (cdr (assq :imports params))
                              (split-string (cdr (assq :imports params)) "[ ,]+" t)
                            nil))
         (build (cdr (assq :build params)))
         
         ;; Handle Code Preparation (Eval wrapping)
         (has-main (ob-lean4--has-main body))
         (should-eval (and (not has-main) (ob-lean4--should-eval body)))
         (processed-body (if should-eval (format "#eval %s" body) body))
         
         ;; Handle Session State
         (session-data (if is-session (ob-lean4--get-session-data session) '(() . "")))
         (old-imports (car session-data))
         (old-body (cdr session-data))
         
         (all-imports (delete-dups (append old-imports current-imports)))
         (full-body (if is-session
                        (if (string-empty-p old-body)
                            processed-body
                          (concat old-body "\n\n" processed-body))
                      processed-body))
         
         ;; Determine if we need --run (Main function check on FULL body)
         (final-has-main (ob-lean4--has-main full-body))
         
         (temp-file (org-babel-temp-file "lean-"))
         (cmd-line (if project-root
                       (format "%s env %s %s %s" 
                               ob-lean4-lake-command 
                               ob-lean4-command 
                               (if final-has-main "--run" "")
                               (org-babel-process-file-name temp-file))
                     (format "%s %s %s" 
                             ob-lean4-command 
                             (if final-has-main "--run" "")
                             (org-babel-process-file-name temp-file)))))
    
    (when (and project-root (member build '("yes" "t" "on" "true")))
      (let ((default-directory project-root))
        (org-babel-eval (format "%s build" ob-lean4-lake-command) "")))

    (with-temp-file temp-file
      (dolist (imp all-imports)
        (insert (format "import %s\n" imp)))
      (insert full-body))
    
    (let ((default-directory (or project-root dir))
          (result (org-babel-eval cmd-line "")))
      
      ;; Update session
      ;; Note: We pass the ORIGINAL 'body' (not processed-body with #eval) to update,
      ;; but the update function now cleans it itself.
      ;; Actually, wait. If 'should-eval' is true, 'body' was just an expression.
      ;; ob-lean4--clean-body-for-history will see "String.append ...".
      ;; It does NOT start with #. So it will keep it.
      ;; But an expression by itself in the history is useless (it does nothing) or might cause syntax error
      ;; if it's not a command.
      ;; Lean: "1+1" -> Error: unexpected token
      ;; So we should NOT save expressions that were meant for #eval.
      
      (when is-session
        (if should-eval
            ;; If it was a pure expression, we DO NOT add it to history.
            (ob-lean4--update-session-data session all-imports "")
          ;; Otherwise, add the body (and let the cleaner remove #check etc.)
          (ob-lean4--update-session-data session all-imports body)))
      
      result)))

(provide 'ob-lean4)
;;; ob-lean4.el ends here