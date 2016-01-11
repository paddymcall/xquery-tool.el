;;; some tests for xquery-tool

;;; run from command line as `emacs -batch -l ert -l xquery-tool.el -l tests/xquery-tool-tests.el -f ert-run-tests-batch-and-exit'

(require 'format-spec)

(ert-deftest xquery-tool-test-query ()
  "Check general functionality of `xquery-tool-query'.
Does not check the links, though."
  (xquery-tool-wipe-temp-files 'force)
  (let* ((tmp (find-file-noselect (make-temp-file "xquery-tool-test-src")))
	(test-src (file-truename (expand-file-name "simple.xml" (file-name-directory (symbol-file 'xquery-tool-test-query)))))
	(cases
	 `(("//price" "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<price>$5.95</price>
<price>$7.95</price>
<price>$8.95</price>
<price>$4.50</price>
<price>$6.95</price>")
	   ("//price" nil 'wrap "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<xq-tool-results>

<price>$5.95</price>
<price>$7.95</price>
<price>$8.95</price>
<price>$4.50</price>
<price>$6.95</price>
</xq-tool-results>
")
	   ("//price" nil nil 'save-namespace ,(format-spec "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<price xmlns:tmplink=\"potemkin\" tmplink:start=\"%p#98\">$5.95</price>
<price xmlns:tmplink=\"potemkin\" tmplink:start=\"%p#302\">$7.95</price>
<price xmlns:tmplink=\"potemkin\" tmplink:start=\"%p#507\">$8.95</price>
<price xmlns:tmplink=\"potemkin\" tmplink:start=\"%p#715\">$4.50</price>
<price xmlns:tmplink=\"potemkin\" tmplink:start=\"%p#898\">$6.95</price>" (format-spec-make ?p (buffer-file-name tmp))))
	   ("/" nil nil nil "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<breakfast_menu>\n	  <food>\n		    <name>Belgian Waffles</name>\n		    <price>$5.95</price>\n		    <description>Two of our famous Belgian Waffles with plenty of real maple syrup</description>\n		    <calories>650</calories>\n	  </food>\n	  <food>\n		    <name>Strawberry Belgian Waffles</name>\n		    <price>$7.95</price>\n		    <description>Light Belgian waffles covered with strawberries and whipped cream</description>\n		    <calories>900</calories>\n	  </food>\n	  <food>\n		    <name>Berry-Berry Belgian Waffles</name>\n		    <price>$8.95</price>\n		    <description>Light Belgian waffles covered with an assortment of fresh berries and whipped cream</description>\n		    <calories>900</calories>\n	  </food>\n	  <food>\n		    <name>French Toast</name>\n		    <price>$4.50</price>\n		    <description>Thick slices made from our homemade sourdough bread</description>\n		    <calories>600</calories>\n	  </food>\n	  <food>\n		    <name>Homestyle Breakfast</name>\n		    <price>$6.95</price>\n		    <description>Two eggs, bacon or sausage, toast, and our ever-popular hash browns</description>\n		    <calories>950</calories>\n	  </food>\n</breakfast_menu>"))))
    (dolist (case cases)
      (with-current-buffer tmp
	(erase-buffer)
	(insert-file-contents test-src)
	(save-buffer)
	(should
	 (equal
	  (progn
	    (apply 'xquery-tool-query (butlast case))
	    (buffer-substring-no-properties (point-min) (point-max)))
	  (car (last case))))))
    (delete-file (buffer-file-name tmp))
    (kill-buffer tmp)))

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

(ert-deftest xquery-tool-test-setup-xquery-results ()
  (let
      ((cases '(("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<price xmlns:tmplink=\"potemkin\" tmplink:start=\"buf://%20*temp*-777077#98\">$5.95</price>\n<price xmlns:tmplink=\"potemkin\" tmplink:start=\"buf://%20*temp*-777077#302\">$7.95</price>\n<price xmlns:tmplink=\"potemkin\" tmplink:start=\"buf://%20*temp*-777077#507\">$8.95</price>\n<price xmlns:tmplink=\"potemkin\" tmplink:start=\"buf://%20*temp*-777077#715\">$4.50</price>\n<price xmlns:tmplink=\"potemkin\" tmplink:start=\"buf://%20*temp*-777077#898\">$6.95</price>" . "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<price>$5.95</price>\n<price>$7.95</price>\n<price>$8.95</price>\n<price>$4.50</price>\n<price>$6.95</price>"))))
    (dolist (case cases)
      (with-temp-buffer
	(insert (car case))
	(should
	 (equal
	  (progn
	    (xquery-tool-setup-xquery-results)
	    (buffer-substring-no-properties (point-min) (point-max)))
	  (cdr case)))))))


