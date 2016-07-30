;;; emacs-tertestrial.el --- Interface to tertestrial

(require 'compile)
(require 'json)

(defgroup tertestrial nil
  "Emacs interface to Tertestrial"
  :group 'tools)

(defvar tertestrial-root-dir)
(defvar tertestrial-project-lang)
(defvar tertestrial-project-err-regexp-alist)
(defvar tertestrial-buffer-name "*tertestrial*")
(defvar tertestrial-command "tertestrial")
(defvar node-err-regexp
  "^[  ]+at \\(?:[^\(\n]+ \(\\)?\\([a-zA-Z\.0-9_/-]+\\):\\([0-9]+\\):\\([0-9]+\\)\)?$"
  "Regular expression to match NodeJS errors.
From http://benhollis.net/blog/2015/12/20/nodejs-stack-traces-in-emacs-compilation-mode/")

(defvar tertestrial-lang-err-regexp-alist
  `(("node" . ((,node-err-regexp 1 2 3)))))

(defun kill-old-buffer (buffer-name)
  "If buffer exists, kill it."
  (when (get-buffer buffer-name)
    (kill-buffer buffer-name)))

(defun tertestrial-tmp-path (&optional dir-path)
  "Path to .tertestrial.tmp."
  (let ((path (if (boundp dir-path) dir-path tertestrial-root-dir)))
    (concat path ".tertestrial.tmp")))

(defun tertestrial-start ()
  "Start the tertestrial server in a comint buffer."
  (interactive)
  (kill-old-buffer tertestrial-buffer-name)
  (with-current-buffer (get-buffer-create tertestrial-buffer-name)
    (setq tertestrial-root-dir (read-directory-name "Select project root directory"))
    (setq default-directory tertestrial-root-dir)
    (ansi-color-for-comint-mode-on)
    (make-comint-in-buffer "tertestrial" tertestrial-buffer-name tertestrial-command)
    (compilation-minor-mode 1)
    (dir-locals-read-from-dir tertestrial-root-dir)
    (when tertestrial-project-lang
        (progn
          (setq tertestrial-project-err-regexp-alist
                (cdr (assoc "node" tertestrial-lang-err-regexp-alist)))
          (set (make-local-variable 'compilation-error-regexp-alist)
               tertestrial-project-err-regexp-alist)))
    (pop-to-buffer tertestrial-buffer-name)))

(defun tertestrial-get-test-file-operation (&optional filename)
  (interactive)
  (let ((buffer-name (if (boundp 'filename) filename (buffer-file-name))))
    (json-encode `(:operation "testFile" :fileName ,buffer-name))))

(defun tertestrial-get-test-line-operation (&optional filename line)
  (interactive)
  (let ((buffer-name (if (boundp 'filename) filename (buffer-file-name)))
        (line-num (if (boundp 'line) line (line-number-at-pos))))
    (json-encode `(:operation "testFile" :fileName ,buffer-name :line ,line))))

(defun tertestrial-get-last-test-operation ()
  (interactive)
  (json-encode '(:operation "repeatLastTest")))

(defun tertestrial-get-set-mapping-operation (&optional mapping)
  (interactive)
  (let ((mapping-num (if (boundp 'mapping) mapping (read-number "Please enter the number associated with the mapping to activate: "))))
    (json-encode `(:operation "setMapping" :mapping ,mapping-num))))

(defun tertestrial-write-command (tert-command-str)
  (interactive)
  (with-temp-buffer
    (insert tert-command-str)
    (write-file (tertestrial-tmp-path))))

(defun tertestrial-test-file ()
  (interactive)
  (tertestrial-write-command (tertestrial-get-test-file-operation)))

(defun tertestrial-test-line ()
  (interactive)
  (tertestrial-write-command (tertestrial-get-test-line-operation)))

(defun tertestrial-last-test-operation ()
  (interactive)
  (tertestrial-write-command (tertestrial-get-last-test-operation)))

(defun tertestrial-set-mapping ()
  (interactive)
  (tertestrial-write-command (tertestrial-get-set-mapping-operation)))
