(ert-deftest tertestrial-get-test-file-operation ()
    (should (equal (tertestrial-get-test-file-operation "file.txt")
                   "{\"operation\":\"testFile\",\"filename\":\"file.txt\"}")))

(ert-deftest tertestrial-get-test-line-operation ()
    (should (equal (tertestrial-get-test-line-operation "file.txt" 10)
                   "{\"operation\":\"testFile\",\"filename\":\"file.txt\",\"line\":10}")))

(ert-deftest tertestrial-get-last-test-operation ()
    (should (equal (tertestrial-get-last-test-operation)
                   "{\"operation\":\"repeatLastTest\"}")))

(ert-deftest tertestrial-get-set-mapping-operation ()
    (should (equal (tertestrial-get-set-mapping-operation 2)
                   "{\"operation\":\"setMapping\",\"mapping\":2}")))
