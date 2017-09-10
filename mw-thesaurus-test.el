(require 'mw-thesaurus)

(defvar mw-thesaurus--word-not-exist-xml
  "
<?xml version=\"1.0\" encoding=\"utf-8\"?>
<entry_list version=\"1.0\">
  <suggestion>jab</suggestion>
  <suggestion>table</suggestion>
  <suggestion>ad-lib</suggestion>
</entry_list>")

(ert-deftest mw-thesaurus--parse-test ()
  (let* ((xml (with-temp-buffer
                (insert-file-contents "./assets/sample.xml")
                (xml-parse-region (point-min) (point-max))))
         (parsed-org-text (mw-thesaurus--parse xml))
         (expected-org-text (string-trim (with-temp-buffer
                                           (insert-file-contents "./assets/sample.org")
                                           (buffer-string)))))
    (string= expected-org-text parsed-org-text)))

(ert-deftest mw-thesaurus--parse-not-existing-word-test ()
  (let* ((xml (with-temp-buffer
                (insert mw-thesaurus--word-not-exist-xml)
                (xml-parse-region (point-min) (point-max))))
         (parsed-org-text (mw-thesaurus--parse xml)))
    (should (equal "" parsed-org-text))))

(ert-deftest mw-thesaurus--correct-number-of-entries-test ()
    (let* ((xml (with-temp-buffer
               (insert-file-contents "./assets/sample.xml")
               (xml-parse-region (point-min) (point-max))))
        (entry-list (assq 'entry_list xml))
        (entries (xml-get-children entry-list 'entry)))
      (should (equal 2 (length entries)))))
