(add-to-list 'load-path
             (file-name-directory (locate-dominating-file load-file-name "emacs-tertestrial.el")))

(require 'emacs-tertestrial)

(describe "tertestrial-get-test-file-operation"
  (it "returns the string for the test file operation"
    (expect (tertestrial-get-test-file-operation "file.txt")
            :to-equal
            "{\"operation\":\"testFile\",\"filename\":\"file.txt\"}")))

(describe "tertestrial-get-test-line-operation"
  (it "returns the string for the test line operation"
    (expect (tertestrial-get-test-line-operation "file.txt" 10)
            :to-equal
            "{\"operation\":\"testLine\",\"filename\":\"file.txt\",\"line\":10}")))

(describe "tertestrial-get-last-test-operation"
  (it "returns the string for the run last test operation"
    (expect (tertestrial-get-last-test-operation)
            :to-equal
            "{\"operation\":\"repeatLastTest\"}")))

(describe "tertestrial-get-set-mapping-operation"
  (it "returns the string for the set mapping operation"
    (expect (tertestrial-get-set-mapping-operation 2)
            :to-equal
            "{\"operation\":\"setMapping\",\"mapping\":2}")))
