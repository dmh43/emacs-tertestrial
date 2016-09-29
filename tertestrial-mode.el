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
