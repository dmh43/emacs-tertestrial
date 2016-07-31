(require 'emacs-tertestrial)
(require 'emacs-tertestrial-buttercup)

(define-minor-mode tertestrial-mode
  "Start tertestrial for the current project and enable keybindings."
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-t C-s") 'tertestrial-start)
            (define-key map (kbd "C-c C-t f") 'tertestrial-test-file)
            (define-key map (kbd "C-c C-t l") 'tertestrial-test-line)
            (define-key map (kbd "C-c C-t s") 'tertestrial-test-suite)
            (define-key map (kbd "C-c C-t C-t") 'tertestrial-last-test)
            (define-key map (kbd "C-c C-t c") 'tertestrial-set-mapping)
            map))

(provide 'tertestrial-mode)
