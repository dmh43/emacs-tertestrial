(require 'emacs-tertestrial-buttercup)

(describe "tertestrial-buttercup-get-test-suite-operation"
  (it "returns the command to run the test suite operation"
    (expect (tertestrial-buttercup-get-test-suite-operation "a test suite name")
            :to-equal
            "{\"pattern\":\"a test suite name\"}")))
