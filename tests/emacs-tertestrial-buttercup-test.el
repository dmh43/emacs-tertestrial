(add-to-list 'load-path
             (file-name-directory
              (locate-dominating-file load-file-name "emacs-tertestrial-buttercup.el")))

(require 'emacs-tertestrial-buttercup)

(describe "tertestrial-buttercup-get-test-name"
  (it "returns the name of the current"
    (expect (tertestrial-get-test-file-operation "file.txt")
            :to-equal
            "{\"operation\":\"testFile\",\"filename\":\"file.txt\"}")))
