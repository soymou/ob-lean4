(add-to-list 'load-path ".")
(require 'ob-lean4)

(defun test-tangle-error ()
  (message "Testing block with tangle...")
  ;; Simulating the params passed by org-babel when :tangle is present
  ;; Note: org-babel-execute is called with computed params.
  ;; The error 'org-babel-merge-params' happens BEFORE execute.
  ;; So simply calling execute might not trigger it if the error is in Org infrastructure.
  ;; But if I call org-babel-execute-src-block, it triggers the full chain.
  
  (with-temp-buffer
    (insert "* Test\n")
    (insert "#+begin_src lean4 :tangle Greeting/Basic.lean\ndef hello := \"world\"\n#+end_src\n")
    (goto-char (point-min))
    (re-search-forward "#+begin_src")
    (org-babel-execute-src-block)))

(test-tangle-error)