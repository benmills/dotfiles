;;; go-alternate --- Alternate implementation and test in go.
;;; Commentary:

;;; Code:

(defun go-alternate-visit-at-point ()
  "Visit alternate file to currently visited go buffer."
  (interactive)
  (switch-to-buffer (find-file (go-alternate-alternate-file-name))))

(defun go-alternate-alternate-file-name ()
  "Find alternate file name of open buffer."
  (interactive)
  (let ((dir (file-name-directory buffer-file-name))
        (extension (file-name-extension buffer-file-name 't)))
    
    (unless (string= extension ".go")
      (error "Not in a go file"))

    (if (string-match "_test.go" buffer-file-name)
        (go-alternate-implementation-file-name buffer-file-name)
      (go-alternate-test-file-name buffer-file-name))))

(defun go-alternate-implementation-file-name (file-name)
  "Get name of implementation file corresponding to FILE-NAME."
  (replace-regexp-in-string "_test.go" ".go" file-name))

(defun go-alternate-test-file-name (file-name)
  "Get name of test file corresponding to FILE-NAME."
  (concat (file-name-sans-extension file-name) "_test.go"))

(provide 'go-alternate)
;;; go-alternate.el ends here
