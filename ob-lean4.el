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

(defun ob-lean4--find-project-root (dir)
  "Find the root of the Lean project containing DIR (looking for lakefile.lean or lakefile.toml)."
  (let ((default-directory dir))
    (if-let ((root (locate-dominating-file dir (lambda (d)
                                                 (or (file-exists-p (expand-file-name "lakefile.lean" d))
                                                     (file-exists-p (expand-file-name "lakefile.toml" d)))))))
        (expand-file-name root)
      nil)))

(defun org-babel-execute:lean4 (body params)
  "Execute a block of Lean 4 code with Babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((dir (expand-file-name (or (cdr (assq :dir params)) default-directory)))
         (project-root (ob-lean4--find-project-root dir))
         (imports (cdr (assq :imports params)))
         (build (cdr (assq :build params)))
         (temp-file (org-babel-temp-file "lean-"))
         (cmd-line (if project-root
                       (format "%s env %s --run %s" ob-lean4-lake-command ob-lean4-command (org-babel-process-file-name temp-file))
                     (format "%s --run %s" ob-lean4-command (org-babel-process-file-name temp-file)))))
    
    (when (and project-root (member build '("yes" "t" "on" "true")))
      (let ((default-directory project-root))
        (org-babel-eval (format "%s build" ob-lean4-lake-command) "")))

    (with-temp-file temp-file
      (when imports
        (dolist (imp (split-string imports "[ ,]+" t))
          (insert (format "import %s\n" imp))))
      (insert body))
    
    (let ((default-directory (or project-root dir)))
      (org-babel-eval cmd-line ""))))

(provide 'ob-lean4)
;;; ob-lean4.el ends here
