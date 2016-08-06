(require 'json)
(require 'thingatpt)
(require 'emacs-tertestrial)

(defun tertestrial-buttercup-get-test-name ()
  (interactive)
  (save-excursion
    (end-of-defun)
    (beginning-of-defun)
    (sp-down-sexp)
    (sp-forward-sexp)
    (sp-forward-sexp)
    (sp-backward-sexp)
    (let ((thing (thing-at-point 'sexp t)))
      (substring thing 1 -1))))

(defun tertestrial-buttercup-get-test-suite-operation (&optional suite)
  (let ((suite-name (if suite suite (tertestrial-buttercup-get-test-name))))
    (json-encode `(:pattern ,suite-name))))

(defun tertestrial-test-suite ()
  (interactive)
  (tertestrial-write-command (tertestrial-buttercup-get-test-suite-operation)))

(provide 'emacs-tertestrial-buttercup)
