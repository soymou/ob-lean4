;;; ob-lean4.el --- Org-babel support for Lean 4

;;; Code:

(require 'org)
(require 'ob)
(require 'cl-lib)
(require 'rx)

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

(defconst ob-lean4-def-regex
  (rx line-start
      (zero-or-more space)
      (optional (or "unsafe" "partial" "noncomputable") (one-or-more space))
      (or "def" "theorem" "lemma" "example" "structure" "inductive" "class" "abbrev" "axiom" "opaque")
      (one-or-more space)
      (group-n 3 (one-or-more (not (any " \t\n({")))))
  "Regex to match Lean definitions.")

(defvar ob-lean4-sessions (make-hash-table :test 'equal)
  "Hash table mapping session names to their code history.
Value is (cons imports persistent-blocks-list).")

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
  (or (string-match-p "^[ \t]*def[ \t]+main" body)
      (string-match-p "^[ \t]*unsafe[ \t]+def[ \t]+main" body)
      (string-match-p "^[ \t]*partial[ \t]+def[ \t]+main" body)))

(defun ob-lean4--extract-def-name (block)
  "Extract the name of the definition in BLOCK, if any.
Returns the defined name or nil."
  (if (string-match ob-lean4-def-regex block)
      (match-string 3 block)
    nil))

(defun ob-lean4--is-command-start (str)
  "Check if STR starts with a Lean command keyword."
  (let ((first-word (car (split-string str))))
    (member first-word ob-lean4-command-keywords)))

(defun ob-lean4--is-transient-start (str)
  "Check if STR starts with # (transient command)."
  (string-prefix-p "#" (string-trim-left str)))

(defun ob-lean4--process-block (text)
  "Process a text block (chunk). Returns (type . processed-text).
Type is :persistent or :transient."
  (let ((clean (string-trim text)))
    (unless (string-empty-p clean)
      (cond
       ((ob-lean4--is-transient-start clean)
        (cons :transient clean))
       ((ob-lean4--is-command-start clean)
        (cons :persistent clean))
       (t
        ;; Fallback: Treat as transient (bare expression)
        (cons :transient clean))))))

(defun ob-lean4--parse-body (body)
  "Split BODY into blocks and process them. Returns list of (type . text)."
  (let* ((chunks (split-string body "\n[ \t]*\n" t)) ;; Split by blank lines
         (processed (mapcar #'ob-lean4--process-block chunks)))
    (cl-remove-if-not #'identity processed)))

(defun ob-lean4--get-session-data (session)
  "Get the (imports . persistent-blocks) data for SESSION."
  (gethash session ob-lean4-sessions '(() . nil)))

(defun ob-lean4--update-session-data (session imports new-persistents)
  "Update the SESSION with new IMPORTS and NEW-PERSISTENTS.
Overwrites existing definitions with same name."
  (let* ((old-data (ob-lean4--get-session-data session))
         (old-persistents (cdr old-data))
         (current-persistents old-persistents))
    
    (dolist (new-block new-persistents)
      (let ((def-name (ob-lean4--extract-def-name new-block)))
        (if def-name
            ;; If defined name found, remove OLD blocks that define the same name
            (setq current-persistents 
                  (cl-remove-if (lambda (old-block)
                                  (equal (ob-lean4--extract-def-name old-block) def-name))
                                current-persistents))
          ;; If no name found, fallback to exact content removal (avoid duplicates)
          (setq current-persistents (remove new-block current-persistents))))
      
      ;; Append the new block
      (setq current-persistents (append current-persistents (list new-block))))
    
    (puthash session (cons imports current-persistents) ob-lean4-sessions)))

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
         
         ;; Parsing
         (parsed-blocks (ob-lean4--parse-body body))
         (current-persistents (mapcar #'cdr 
                                     (cl-remove-if-not (lambda (x) (eq (car x) :persistent)) parsed-blocks)))
         (current-transients (mapcar #'cdr 
                                    (cl-remove-if-not (lambda (x) (eq (car x) :transient)) parsed-blocks)))
         
         ;; Session Handling
         (session-data (if is-session (ob-lean4--get-session-data session) '(() . nil)))
         (history-imports (car session-data))
         (history-persistents (cdr session-data))
         
         ;; Update Session (calculate NEW history)
         (updated-persistents history-persistents))
    
    (when is-session
      (ob-lean4--update-session-data session 
                                     (delete-dups (append history-imports current-imports))
                                     current-persistents)
      ;; Reload updated data
      (setq session-data (ob-lean4--get-session-data session))
      (setq history-imports (car session-data))
      (setq updated-persistents (cdr session-data)))

    (let* ((all-imports (if is-session 
                            (delete-dups (append history-imports current-imports))
                          current-imports))
           (blocks-to-run (if is-session
                              (append updated-persistents current-transients)
                            (append current-persistents current-transients)))
           (full-body (mapconcat #'identity blocks-to-run "\n\n"))
           ;; Only use --run if the CURRENT block defines main.
           ;; This prevents main from history being re-executed on every eval.
           (current-has-main (ob-lean4--has-main body))
           
           (temp-file (org-babel-temp-file "lean-"))
           (cmd-line (if project-root
                         (format "%s env %s %s %s" 
                                 ob-lean4-lake-command 
                                 ob-lean4-command 
                                 (if current-has-main "--run" "")
                                 (org-babel-process-file-name temp-file))
                       (format "%s %s %s" 
                               ob-lean4-command 
                               (if current-has-main "--run" "")
                               (org-babel-process-file-name temp-file)))))
      
      (when (and project-root (member build '("yes" "t" "on" "true")))
        (let ((default-directory project-root))
          (org-babel-eval (format "%s build" ob-lean4-lake-command) "")))

      (with-temp-file temp-file
        (dolist (imp all-imports)
          (insert (format "import %s\n" imp)))
        (insert full-body))
      
      (let ((default-directory (or project-root dir)))
        (org-babel-eval cmd-line "")))))

(provide 'ob-lean4)
;;; ob-lean4.el ends here
