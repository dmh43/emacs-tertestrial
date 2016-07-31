(defvar node-err-regexp
  "^[  ]+at \\(?:[^\(\n]+ \(\\)?\\([a-zA-Z\.0-9_/-]+\\):\\([0-9]+\\):\\([0-9]+\\)\)?$"
  "Regular expression to match NodeJS errors.
From http://benhollis.net/blog/2015/12/20/nodejs-stack-traces-in-emacs-compilation-mode/")


(defvar tertestrial-lang-err-regexp-alist
  `(("node" . ((,node-err-regexp 1 2 3)))))

(provide 'lang-err-regexps)
