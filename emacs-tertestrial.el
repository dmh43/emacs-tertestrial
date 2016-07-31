;;; emacs-tertestrial.el --- Interface to tertestrial
;; Author: dmh
;; Version: 0.2

(require 'compile)
(require 'json)
(require 'lang-err-regexps)

(defgroup tertestrial nil
  "Emacs interface to Tertestrial"
  :group 'tools)

(defvar tertestrial-root-dir)
(defvar tertestrial-project-lang)
(defvar tertestrial-project-err-regexp-alist)
(defvar tertestrial-buffer-name "*tertestrial*")
(defvar tertestrial-command "tertestrial")


(defun kill-old-buffer (buffer-name)
  "If buffer exists, kill it."
  (when (get-buffer buffer-name)
    (kill-buffer buffer-name)))

(defun tertestrial-tmp-path (&optional dir-path)
  "Path to .tertestrial.tmp."
  (let ((path (if dir-path dir-path tertestrial-root-dir)))
    (concat path ".tertestrial.tmp")))

(defun tertestrial-start ()
  "Start the tertestrial server in a comint buffer."
  (interactive)
  (kill-old-buffer tertestrial-buffer-name)
  (let ((lang (when (boundp 'tertestrial-project-lang) tertestrial-project-lang)))
    (with-current-buffer (get-buffer-create tertestrial-buffer-name)
      (setq tertestrial-root-dir (read-directory-name "Select project root directory"))
      (setq default-directory tertestrial-root-dir)
      (ansi-color-for-comint-mode-on)
      (make-comint-in-buffer "tertestrial" tertestrial-buffer-name tertestrial-command)
      (when lang
        (compilation-minor-mode 1)
        (dir-locals-read-from-dir tertestrial-root-dir)
        (setq tertestrial-project-err-regexp-alist
              (cdr (assoc "node" tertestrial-lang-err-regexp-alist)))
        (set (make-local-variable 'compilation-error-regexp-alist)
             tertestrial-project-err-regexp-alist))
      (pop-to-buffer tertestrial-buffer-name))))

(defun tertestrial-get-test-file-operation (&optional filename)
  (let ((buffer-name (if filename filename (buffer-file-name))))
    (json-encode `(:operation "testFile" :filename ,buffer-name))))

(defun tertestrial-get-test-line-operation (&optional filename line)
  (let ((buffer-name (if filename filename (buffer-file-name)))
        (line-num (if line line (line-number-at-pos))))
    (json-encode `(:operation "testLine" :filename ,buffer-name :line ,line-num))))

(defun tertestrial-get-last-test-operation ()
  (json-encode '(:operation "repeatLastTest")))

(defun tertestrial-get-set-mapping-operation (&optional mapping)
  (let ((mapping-num (if mapping mapping (read-number "Please enter the number associated with the mapping to activate: "))))
    (json-encode `(:operation "setMapping" :mapping ,mapping-num))))

(defun tertestrial-write-command (tert-command-str)
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

(provide 'emacs-tertestrial)

;;; emacs-tertestrial.el ends here
