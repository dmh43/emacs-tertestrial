(add-to-list 'load-path
             (file-name-directory
              (locate-dominating-file load-file-name "emacs-tertestrial.el")))

(require 'emacs-tertestrial)

(describe "tertestrial-get-test-file-operation"
  (it "returns the string for the test file operation"
    (expect (tertestrial-get-test-file-operation "file.txt")
            :to-equal
            "{\"filename\":\"file.txt\"}")))

(describe "tertestrial-get-test-line-operation"
  (it "returns the string for the test line operation"
    (expect (tertestrial-get-test-line-operation "file.txt" 10)
            :to-equal
            "{\"filename\":\"file.txt\",\"line\":10}")))

(describe "tertestrial-get-last-test-operation"
  (it "returns the string for the run last test operation"
    (expect (tertestrial-get-last-test-operation)
            :to-equal
            "{\"repeatLastTest\":true}")))

(describe "tertestrial-get-set-actionset-operation"
  (it "returns the string for the set actionset operation"
    (expect (tertestrial-get-set-actionset-operation "headless")
            :to-equal
            "{\"actionSet\":\"headless\"}")))

(describe "tertestrial-get-cycle-actionset-operation"
          (it "returns the string for the cycle-actionset operation"
              (expect (tertestrial-get-cycle-actionset-operation)
                      :to-equal
                      "{\"cycleActionSet\":\"next\"}")))

(describe "tertestrial-buttercup-get-test-suite-operation"
          (it "returns the command to run the test suite operation"
              (expect (tertestrial-buttercup-get-test-suite-operation "a test suite name")
                      :to-equal
                      "{\"name\":\"pattern\",\"pattern\":\"a test suite name\"}")))

(describe "tertestrial-buttercup-get-test-dir-operation"
          (it "returns the command to run the test directory operation"
              (expect (tertestrial-buttercup-get-test-dir-operation "/home/code/")
                      :to-equal
                      "{\"name\":\"directory\",\"dirpath\":\"/home/code/\"}")))
