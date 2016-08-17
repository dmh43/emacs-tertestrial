(add-to-list 'load-path
             (file-name-directory
              (locate-dominating-file load-file-name "emacs-tertestrial-buttercup.el")))

(require 'emacs-tertestrial-buttercup)

(describe "tertestrial-buttercup-get-test-suite-operation"
  (it "returns the command to run the test suite operation"
    (expect (tertestrial-buttercup-get-test-suite-operation "a test suite name")
            :to-equal
            "{\"pattern\":\"a test suite name\"}")))

(describe "tertestrial-buttercup-get-test-dir-operation"
  (it "returns the command to run the test directory operation"
    (expect (tertestrial-buttercup-get-test-dir-operation "/home/code/")
            :to-equal
            "{\"dirpath\":\"/home/code/\"}")))
