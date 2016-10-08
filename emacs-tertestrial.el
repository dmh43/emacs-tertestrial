;;; emacs-tertestrial.el --- Interface to tertestrial
;; Author: dmh
;; Version: 0.3
;; Changelog:
;; | Version | Contributor | Descrption                              |
;; |---------+-------------+-----------------------------------------|
;; |     0.3 | dmh43       | Tertestrial 0.0.5 encorporated breaking |
;; |         |             | changes to the mapping format. Mappings |
;; |         |             | are now referred to as "actionsets"     |
;; |---------+-------------+-----------------------------------------|
;; |     0.5 | dmh43       | Tertestrial 0.3.1 supports cycling      |
;; |         |             | action sets                             |
;; |---------+-------------+-----------------------------------------|


(require 'compile)
(require 'json)
(require 'json)
(require 'thingatpt)

(defgroup tertestrial nil
  "Emacs interface to Tertestrial"
  :group 'tools)

(defvar tertestrial-project-err-regexp-alist)
(defvar tertestrial-command "tertestrial")
(defvar node-err-regexp
  "^[  ]+at \\(?:[^\(\n]+ \(\\)?\\([a-zA-Z\.0-9_/-]+\\):\\([0-9]+\\):\\([0-9]+\\)\)?$"
  "Regular expression to match NodeJS errors.
From http://benhollis.net/blog/2015/12/20/nodejs-stack-traces-in-emacs-compilation-mode/")
(defvar tertestrial-lang-err-regexp-alist
  `(("node" . ((,node-err-regexp 1 2 3)))))

(defun tertestrial-get-project-name ()
  (file-name-base (directory-file-name (tertestrial-get-root-dir))))

(defun tertestrial-get-buffer-name (&optional project-name)
  (if project-name
      (concat "*tertestrial-" project-name "*")
    (concat "*tertestrial-" (tertestrial-get-project-name) "*")))

(defun tertestrial-get-root-dir ()
  (if (boundp 'tertestrial-root-dir)
      tertestrial-root-dir
    (let ((dir-name (read-directory-name "Select project root directory")))
      (add-dir-local-variable nil 'tertestrial-root-dir dir-name)
      (previous-buffer)
      dir-name)))

(defun tertestrial-tmp-path (&optional dir-path)
  "Path to .tertestrial.tmp."
  (concat (if dir-path dir-path tertestrial-root-dir) ".tertestrial.tmp"))

(defun tertestrial-start ()
  "Start the tertestrial server in a comint buffer."
  (interactive)
  (let* ((lang (when (boundp 'tertestrial-project-lang) tertestrial-project-lang))
         (project-path (tertestrial-get-root-dir))
         (tertestrial-buff-name (tertestrial-get-buffer-name)))
    (with-current-buffer (get-buffer-create tertestrial-buff-name)
      (setq default-directory project-path)
      (hack-dir-local-variables-non-file-buffer)
      (message default-directory)
      (ansi-color-for-comint-mode-on)
      (make-comint-in-buffer "tertestrial" tertestrial-buff-name tertestrial-command)
      (when lang
        (message lang)
        (compilation-minor-mode 1)
        (setq tertestrial-project-err-regexp-alist
              (cdr (assoc lang tertestrial-lang-err-regexp-alist)))
        (set (make-local-variable 'compilation-error-regexp-alist)
             tertestrial-project-err-regexp-alist))
      (pop-to-buffer tertestrial-buff-name))))

(defun tertestrial-get-test-file-operation (&optional filename)
  (let ((buffer-name (if filename filename (buffer-file-name))))
    (json-encode `(:filename ,buffer-name))))

(defun tertestrial-get-test-line-operation (&optional filename line)
  (let ((buffer-name (if filename filename (buffer-file-name)))
        (line-num (if line line (line-number-at-pos))))
    (json-encode `(:filename ,buffer-name :line ,line-num))))

(defun tertestrial-get-last-test-operation ()
  (json-encode '(:repeatLastTest t)))

(defun tertestrial-get-set-actionset-operation (&optional actionset)
  (let ((actionset-num
         (if actionset
             actionset
           (read-number "Please enter the number associated with the actionset to activate: "))))
    (json-encode `(:actionSet ,actionset-num))))

(defun tertestrial-get-cycle-actionset-operation ()
  (json-encode `(:cycleActionSet next)))

(defun tertestrial-write-command (tert-command-str)
  (let ((tmp-path (tertestrial-tmp-path)))
    (with-temp-buffer
      (insert tert-command-str)
      (write-file tmp-path))))

(defun tertestrial-clear-term-write-command (tert-command-str)
  (let ((comint-buf-name (tertestrial-get-buffer-name)))
    (with-current-buffer (get-buffer-create comint-buf-name)
      (comint-clear-buffer))
    (tertestrial-write-command tert-command-str)))

(defun tertestrial-test-file ()
  (interactive)
  (tertestrial-clear-term-write-command (tertestrial-get-test-file-operation)))

(defun tertestrial-test-line ()
  (interactive)
  (tertestrial-clear-term-write-command (tertestrial-get-test-line-operation)))

(defun tertestrial-last-test ()
  (interactive)
  (tertestrial-clear-term-write-command (tertestrial-get-last-test-operation)))

(defun tertestrial-set-actionset ()
  (interactive)
  (tertestrial-clear-term-write-command (tertestrial-get-set-actionset-operation)))

(defun tertestrial-cycle-actionset ()
  (interactive)
  (tertestrial-clear-term-write-command (tertestrial-get-cycle-actionset-operation)))

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
    (json-encode `(:name "pattern" :pattern ,suite-name))))

(defun tertestrial-buttercup-get-test-dir-operation (&optional dir)
  (let ((dir-path (if dir dir (file-name-directory (buffer-file-name)))))
    (json-encode `(:name "directory" :dirpath ,dir-path))))

(defun tertestrial-test-suite ()
  (interactive)
  (tertestrial-clear-term-write-command (tertestrial-buttercup-get-test-suite-operation)))

(defun tertestrial-test-dir ()
  (interactive)
  (tertestrial-clear-term-write-command (tertestrial-buttercup-get-test-dir-operation)))

(defun tertestrial-toggle-autotest-hook ()
  (interactive)
  (if (advice-member-p 'tertestrial-last-test 'save-buffer)
      (advice-remove 'save-buffer 'tertestrial-last-test)
    (advice-add 'save-buffer :after 'tertestrial-last-test)))

(advice-add 'tertestrial-write-command :before
            (lambda ()
              (when (not (advice-member-p 'tertestrial-last-test 'save-buffer))
                'save-buffer)))

(define-minor-mode tertestrial-mode
  "Start tertestrial for the current project and enable keybindings."
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-t C-s") 'tertestrial-start)
            (define-key map (kbd "C-c C-t f") 'tertestrial-test-file)
            (define-key map (kbd "C-c C-t l") 'tertestrial-test-line)
            (define-key map (kbd "C-c C-t s") 'tertestrial-test-suite)
            (define-key map (kbd "C-c C-t d") 'tertestrial-test-dir)
            (define-key map (kbd "C-c C-t C-t") 'tertestrial-last-test)
            (define-key map (kbd "C-c C-t c") 'tertestrial-set-actionset)
            (define-key map (kbd "C-c C-t a") 'tertestrial-toggle-autotest-hook)
            map))


(provide 'emacs-tertestrial)

;;; emacs-tertestrial.el ends here
