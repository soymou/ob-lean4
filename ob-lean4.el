;;; ob-lean4.el --- Org-babel execution for Lean4

;; Copyright (C) 2026

;; Author: Gemini
;; Version: 0.1
;; Package-Requires: ((emacs "26.1") (org "9.0"))

;;; Commentary:

;; Execute Lean4 code blocks in Org-mode.
;; Supports sessions via history concatenation.
;; Supports output filtering per block.

;;; Code:

(require 'ob)
(require 'org)
;; Require ob-tangle to ensure org-babel-tangle-lang-exts is defined
(require 'ob-tangle)
(require 'cl-lib)
(require 'subr-x)

(defgroup ob-lean4 nil
  "org-babel-execute:lean4"
  :group 'org-babel)

(defcustom ob-lean4-command "lean"
  "Command to execute Lean4 code."
  :type 'string
  :group 'ob-lean4)

(defvar ob-lean4-sessions (make-hash-table :test 'equal)
  "Hash table mapping session names to their state (imports . body).")

(defvar org-babel-default-header-args:lean4
  '((:results . "output") (:exports . "both"))
  "Default arguments for evaluating a lean4 source block.")

(defun ob-lean4--extract-imports (code)
  "Separate CODE into imports and body.
Returns a cons cell (imports-list . body-string)."
  (let ((imports '())
        (body-lines '()))
    (dolist (line (split-string code "\n"))
      (if (string-match-p "^[ 	]*import " line)
          (push line imports)
        (push line body-lines)))
    (cons (nreverse imports) (mapconcat #'identity (nreverse body-lines) "\n"))))

(defun ob-lean4--session-name (params)
  "Get session name from PARAMS."
  (cdr (assq :session params)))

(defun ob-lean4--run-process (code)
  "Run CODE with `ob-lean4-command`.
Return (exit-code . output)."
  (let ((temp-file (make-temp-file "lean4-" nil ".lean")))
    (with-temp-file temp-file (insert code))
    (let* ((buff (get-buffer-create " *ob-lean4-output*"))
           (exit-code 
            (progn 
              (with-current-buffer buff (erase-buffer))
              (call-process ob-lean4-command nil buff nil temp-file))))
      (let ((output (with-current-buffer buff (buffer-string))))
        (kill-buffer buff)
        (delete-file temp-file)
        (cons exit-code output)))))

(defun org-babel-execute:lean4 (body params)
  "Execute a block of Lean4 code with Babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((session (ob-lean4--session-name params))
         (is-session (and session (not (string= session "none"))))
         (full-code "")
         ;; Use a random separator
         (separator (format "---OB-LEAN4-SEP-%04x---" (random 65535))))

    (if is-session
        (let* ((state (gethash session ob-lean4-sessions '(() . "")))
               (prev-imports (car state))
               (prev-body (cdr state))
               (curr-split (ob-lean4--extract-imports body))
               (curr-imports (car curr-split))
               (curr-body (cdr curr-split))
               ;; Merge imports
               (all-imports (cl-remove-duplicates (append prev-imports curr-imports) :test #'string=))
               (imports-str (mapconcat #'identity all-imports "\n")))
          
          ;; Construct code to try
          (setq full-code 
                (concat imports-str "\n"
                        prev-body "\n"
                        (format "#eval IO.println \"%s\"\n" separator)
                        curr-body))
          
          (let* ((proc-res (ob-lean4--run-process full-code))
                 (exit-code (car proc-res))
                 (output (cdr proc-res)))
            
            (if (= exit-code 0)
                (progn
                  ;; Update state: Add current body to history
                  (puthash session (cons all-imports (concat prev-body "\n" curr-body)) ob-lean4-sessions)
                  ;; Parse output
                  (ob-lean4--filter-output output separator))
              ;; On failure, return error but don't update state
              output)))
      
      ;; No session
      (let* ((split (ob-lean4--extract-imports body))
             (imports (car split))
             (curr-body (cdr split)))
        (setq full-code (concat (mapconcat #'identity imports "\n") "\n" curr-body))
        (let ((proc-res (ob-lean4--run-process full-code)))
          (cdr proc-res))))))

(defun ob-lean4--filter-output (output separator)
  "Return string in OUTPUT after SEPARATOR."
  (let ((parts (split-string output separator)))
    (if (> (length parts) 1)
        (string-trim (car (last parts)))
      (string-trim output))))

;; Shortcuts
(defun ob-lean4-restart-session-command ()
  "Restart the session named in the current block or ask user."
  (interactive)
  (let* ((info (ignore-errors (org-babel-get-src-block-info)))
         (session (if info (cdr (assq :session (nth 2 info))) nil)))
    (when (or (not session) (string= session "none"))
      (setq session (read-string "Session to restart: ")))
    (remhash session ob-lean4-sessions)
    (message "Session '%s' restarted." session)))

(defun ob-lean4-restart-all-sessions-command ()
  "Restart all lean4 sessions."
  (interactive)
  (clrhash ob-lean4-sessions)
  (message "All Lean4 sessions restarted."))

;; Tangling
(add-to-list 'org-babel-tangle-lang-exts '("lean4" . "lean"))

(provide 'ob-lean4)
;;; ob-lean4.el ends here