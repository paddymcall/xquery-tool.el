;;; some tests for xquery-tool

(ert-deftest xquery-test-tool-query ()
  (let ((filename (make-temp-file "xquery-tool-test")))
    (with-current-buffer (find-file-noselect filename)
      (insert
       "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<bookstore>
<book>
  <title lang=\"en\">Harry Potter</title>
  <price>29.99</price>
</book>
<book>
  <title lang=\"en\">Learning XML</title>
  <price>39.95</price>
</book>
</bookstore>")
      (should
       (equal (progn (xquery-tool-query "//title") (buffer-substring-no-properties (point-min) (point-max)))
	      "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<title lang=\"en\">Harry Potter</title>
<title lang=\"en\">Learning XML</title>")))))

(ert-deftest xquery-tool-test-get-namespace-candidates ()
    (let ((cases '(("<TEI xml:id=\"pvv-SARIT\"  xmlns=\"http://www.tei-c.org/ns/1.0\">" . nil)
		   ("<TEI xmlns:tmplink=\"potemkin\" tmplink:start=\"pramanavarttikavrtti.xml#3064\" xml:id=\"pvv-SARIT\"  xmlns=\"http://www.tei-c.org/ns/1.0\">" . ([31 38 44 46 75 t nil] [6 11 19 21 29 t nil]))
		   ("</TEI>" . nil)
		   ("<empty xmlns:tmplink=\"potemkin\" tmplink:start=\"pramanavarttikavrtti.xml#3064\" xml:id=\"pvv-SARIT\"  xmlns=\"http://www.tei-c.org/ns/1.0\" />" . ([33 40 46 48 77 t nil] [8 13 21 23 31 t nil])))))
      (dolist (case cases)
	(with-temp-buffer
	  (insert (car case))
	  (goto-char (point-min))
	  (xmltok-forward)
	  (should (equal (xquery-tool-get-namespace-candidates "tmplink") (cdr case)))))))

