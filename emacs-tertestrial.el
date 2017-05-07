;;; emacs-tertestrial.el --- Interface to tertestrial
;; Author: dmh
;; Version: 0.8
;; Changelog:
;; | Version | Contributor | Description                             |
;; |---------+-------------+-----------------------------------------|
;; |     0.3 | dmh43       | Tertestrial 0.0.5 encorporated breaking |
;; |         |             | changes to the mapping format. Mappings |
;; |         |             | are now referred to as "actionsets"     |
;; |---------+-------------+-----------------------------------------|
;; |     0.5 | dmh43       | Tertestrial 0.3.1 supports cycling      |
;; |         |             | action sets                             |
;; |---------+-------------+-----------------------------------------|
;; |     0.6 | dmh43       | emacs-tertestrial no longer needs dir   |
;; |         |             | locals                                  |
;; |---------+-------------+-----------------------------------------|
;; |     0.7 | dmh43       | set-actionset now supports completion   |
;; |         |             | with common emacs autocompletion mode   |
;; |---------+-------------+-----------------------------------------|
;; |     0.8 | dmh43       | tertstrial mode is global. Only save    |
;; |         |             | buffer if visiting file                 |
;; |---------+-------------+-----------------------------------------|


(require 'compile)
(require 'json)
(require 's)
(require 'thingatpt)

(defgroup tertestrial nil
  "Emacs interface to Tertestrial"
  :group 'tools)

(defvar tertestrial-auto-test-mode nil)
(defvar tertestrial-project-err-regexp-alist)
(defvar tertestrial-command "tertestrial")
(defvar node-err-regexp
  "^[  ]+at \\(?:[^\(\n]+ \(\\)?\\([a-zA-Z\.0-9_/-]+\\):\\([0-9]+\\):\\([0-9]+\\)\)?$"
  "Regular expression to match NodeJS errors.
From http://benhollis.net/blog/2015/12/20/nodejs-stack-traces-in-emacs-compilation-mode/")

(defvar tertestrial-lang-err-regexp-alist
  `(("js" . ((,node-err-regexp 1 2 3)))
    ("coffee" . ((,node-err-regexp 1 2 3)))))

(defun tertestrial-get-project-name ()
  (file-name-base (directory-file-name (tertestrial-get-root-dir))))

(defun tertestrial-get-buffer-name (&optional project-name)
  (if project-name
      (concat "*tertestrial-" project-name "*")
    (concat "*tertestrial-" (tertestrial-get-project-name) "*")))

(defun tertestrial-get-root-dir () (locate-dominating-file default-directory "tertestrial.yml"))

(defun tertestrial-tmp-path (&optional dir-path)
  "Path to .tertestrial.tmp."
  (concat (if dir-path dir-path (tertestrial-get-root-dir)) ".tertestrial.tmp"))

(defun tertestrial-get-lang-err-regexp-alist ()
  (interactive)
  (let ((file-extension (file-name-extension buffer-file-name)))
    (cdr (assoc file-extension tertestrial-lang-err-regexp-alist))))

(defun tertestrial-start ()
  "Start the tertestrial server in a comint buffer."
  (interactive)
  (let* ((lang (when (boundp 'tertestrial-project-lang) tertestrial-project-lang))
         (project-path (tertestrial-get-root-dir))
         (project-err-regexp (tertestrial-get-lang-err-regexp-alist))
         (comint-buff-name (tertestrial-get-buffer-name)))
    (with-current-buffer (get-buffer-create comint-buff-name)
      (setq default-directory project-path)
      (message project-path)
      (ansi-color-for-comint-mode-on)
      (make-comint-in-buffer "tertestrial" comint-buff-name tertestrial-command)
      (when project-err-regexp
        (compilation-minor-mode 1)
        (set (make-local-variable 'compilation-error-regexp-alist)
             project-err-regexp))
      (pop-to-buffer comint-buff-name))))

(defun tertestrial-get-test-file-operation (&optional filename)
  (let ((buffer-name (if filename filename (buffer-file-name))))
    (json-encode `(:filename ,buffer-name))))

(defun tertestrial-get-test-line-operation (&optional filename line)
  (let ((buffer-name (if filename filename (buffer-file-name)))
        (line-num (if line line (line-number-at-pos))))
    (json-encode `(:filename ,buffer-name :line ,line-num))))

(defun tertestrial-get-last-test-operation ()
  (json-encode '(:repeatLastTest t)))

(defcustom tertestrial-completion-system 'helm
  "The completion system to be used by Projectile."
  :group 'projectile
  :type '(radio
          (const :tag "Ido" ido)
          (const :tag "Grizzl" grizzl)
          (const :tag "Helm" helm)
          (const :tag "Ivy" ivy)
          (const :tag "Default" default)
          (function :tag "Custom function")))

;; from projectile.el
(defun tertestrial-completing-read (prompt choices &optional initial-input)
  "Present a project tailored PROMPT with CHOICES."
  (let ((prompt (projectile-prepend-project-name prompt)))
    (cond
     ((eq tertestrial-completion-system 'ido)
      (ido-completing-read prompt choices nil nil initial-input))
     ((eq tertestrial-completion-system 'default)
      (completing-read prompt choices nil nil initial-input))
     ((eq tertestrial-completion-system 'helm)
      (if (fboundp 'helm-comp-read)
          (helm-comp-read prompt choices
                          :initial-input initial-input
                          :candidates-in-buffer t
                          :must-match 'confirm)
        (user-error "Please install helm from \
https://github.com/emacs-helm/helm")))
     ((eq tertestrial-completion-system 'grizzl)
      (if (and (fboundp 'grizzl-completing-read)
               (fboundp 'grizzl-make-index))
          (grizzl-completing-read prompt (grizzl-make-index choices))
        (user-error "Please install grizzl from \
https://github.com/d11wtq/grizzl")))
     ((eq tertestrial-completion-system 'ivy)
      (if (fboundp 'ivy-read)
          (ivy-read prompt choices
                    :initial-input initial-input
                    :caller 'tertestrial-completing-read)
        (user-error "Please install ivy from \
https://github.com/abo-abo/swiper")))
     (t (funcall tertestrial-completion-system prompt choices)))))

(defun tertestrial-get-actionset-list ()
  (with-temp-buffer
    (insert-file-contents "tertestrial.yml")
    (mapcan 'cdr
            (s-match-strings-all "^\s+\\([a-zA-Z0-9]+\\):$"
                                 (buffer-string)))))

(defun tertestrial-select-actionset ()
  (interactive)
  (let (actionset-list (tertestrial-get-actionset-list))
    (when actionset-list
      (tertestrial-completing-read
       "Please select an actionset"
       actionset-list))))

(defun tertestrial-get-set-actionset-operation (&optional actionset)
  (let ((actionset (or actionset (tertestrial-select-actionset))))
    (if actionset
        (json-encode `(:actionSet ,actionset))
      (progn
        (message "No alternate actionsets specified in tertestrial.yml")
        nil))))

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
    (when tert-command-str
      (tertestrial-write-command tert-command-str))))

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
  (if tertestrial-auto-test-mode
      (progn
        (setf tertestrial-auto-test-mode nil)
        (message "Tertestrial autotest mode disabled"))
    (progn
      (setf tertestrial-auto-test-mode t)
      (message "Tertestrial autotest mode enabled"))))

(advice-add 'tertestrial-write-command :before
            (lambda (&rest args)
              (when (buffer-file-name)
                (save-buffer)
                (tertestrial-last-test))))

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
            map)
  :global t)

(setq ansi-color-drop-regexp
      "\033\\[\\([ABCDsuK]\\|[012][GJK]\\|=[0-9]+[hI]\\|[0-9;]*[Hf]\\|\\?[0-9]+[hl]\\)")


(provide 'emacs-tertestrial)

;;; emacs-tertestrial.el ends here
