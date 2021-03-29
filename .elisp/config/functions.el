;; fix buffer with ^M line endings
(defun eol-fix ()
"fix buffer with DOS ^M line endings"
(interactive)
(save-excursion
  (goto-char (point-min))
  (while (search-forward-regexp "$" nil t)
    (goto-char (- (point) 1))
    (delete-char 1))))


(defun explode-region (start end separator-regexp)
  "Splits region into separate lines based on regexp separator"
  (interactive "r\nssplit regexp: ")
  ; (message (format "params: %d %d %s" start end separator-regexp))
  (save-excursion
    (goto-char start)
    (while (search-forward-regexp separator-regexp nil t)
      (replace-match "\n"))))

(defun implode-region (start end separator)
  "Merge region into one line inserting separator between original lines."
  (interactive "r\nsseparator: ")
  (save-excursion
    (goto-char start)
    (while (search-forward "\n" nil t)
      (replace-match separator))))

;; taken from xsteve-insert-date, changed the name and format string.
(defun insert-date (dayincr)
  "Inserts a date-stamp at point - Format: \"DD-MM-YYYY (wd)\""
  (interactive "p")
  (if (null current-prefix-arg) (setq dayincr 0))
  (let* ((base 65536.0)
         (nowlist (current-time))
         (datenum (+ (*  (nth 0 nowlist) base) (nth 1 nowlist)
                     (* dayincr 60.0 60.0 24.0)))
         (s (current-time-string
             (list (truncate( / datenum base)) (truncate(mod datenum base)))))
         (date))
    (if (equal current-prefix-arg '(4))
        (progn
          (let ((bound (line-beginning-position))
                (datenum)
                (datestring))
            (goto-char (min (point-max) (+ (point) 10)))
            (re-search-backward "[0-9][0-9]\\.[0-9][0-9]\\.[0-9][0-9][0-9][0-9]" bound)
            (setq datestring (buffer-substring (point) (+ (point) 10)))
            (setq datenum (calendar-absolute-from-gregorian
                           (list
                            (string-to-number (substring datestring 3 5))
                            (string-to-number (substring datestring 0 2))
                            (string-to-number (substring datestring 6 10)))))
            (setq dayincr (string-to-number (read-string "Increment by days: " "7")))
            (delete-region (point) (+ 10 (point)))
            (setq date (calendar-gregorian-from-absolute (+ datenum dayincr)) datestring)))
      (setq date (list (length (member (substring s 4 7)
                                       '("Dec" "Nov" "Oct" "Sep" "Aug" "Jul"
                                         "Jun" "May" "Apr" "Mar" "Feb" "Jan")))
                       (string-to-number (substring s 8 10))
                       (string-to-number (substring s 20 24)))))
                       ;;(cdr (assoc (substring s 0 3)
                       ;;            '(("Son" . "So")("Mon" . "Mo")("Tue" . "Di")
                       ;;              ("Wed" . "Mi")("Thu" . "Do")("Fri" . "Fr")
                       ;;              ("Sat" ."Sa")))))))

    (insert (format "%02d-%02d-%04d" (nth 1 date) (nth 0 date) (nth 2 date)))))

(defmacro not! (var)
  `(setq ,var (not ,var)))

(defun clipboard-toggle ()
  (interactive)
  (not! x-select-enable-clipboard))

(defun gtd ()
  (interactive)
  (find-file "~/private/gtd/gtd.org"))

(defun push-named-macro-to-kmacro-ring (macro-name)
  "Push named macro to the top of keyboard macro ring, so it
becomes last-kbd-macro."
  (interactive "aPush named macro to kmacro ring: ")
  (when last-kbd-macro
    (push (list last-kbd-macro kmacro-counter kmacro-counter-format)
          kmacro-ring))
  (setq last-kbd-macro (symbol-function macro-name)
        kmacro-counter 0
        kmacro-counter-format 0))

(require 'rect)

;; not mine
(defun apply-on-rectangle-region-points (fun beg end &rest args)
  "Like `apply-on-rectangle', but pass points in the buffer instead of columns."
  (apply-on-rectangle
   (lambda (bcol ecol)
     (apply fun
            (progn
              (move-to-column bcol 'coerce)
              (point))
            (progn
              (move-to-column ecol 'coerce)
              (prog1
                  (point)
                (beginning-of-line)))
            args))
   beg end))

(defun downcase-rectangle (beg end)
  "Convert the marked rectangle to lower case."
  (interactive "r")
  (apply-on-rectangle-region-points 'downcase-region beg end))

;;;###autoload
(defun upcase-rectangle (beg end)
  "Convert the marked rectangle to upper case."
  (interactive "r")
  (apply-on-rectangle-region-points 'upcase-region beg end))

(defun flatten-lists (&rest list)
  "Return a new, flat list of composed all the remaining arguments and
their sublists.
Dotted pairs are handled as lists.

  Examples: (flatten-lists '(foo (bar baz) (quux (bletch (moby
. bignum)))))
             => (foo bar baz quux bletch moby bignum)

             (flatten-lists '(a b (c d (e)) . f) '(1 2 ((3) . 4) 5))
             => (a b c d e f 1 2 3 4 5)"
  (cond ((consp (cdr list))
         (apply 'nconc (mapcar 'flatten-lists list)))
        ((consp (car list))
         (append (flatten-lists (car (car list)))
                 (flatten-lists (cdr (car list)))))
        ((car list)
         (list (car list)))))

(defalias 'flatten 'flatten-lists)

(setq find-dired-simple-ignored-dirs '(".svn"))

(defun find-dired-simple ()
  "Simplified version of find-dired.
   Asks for directory and for file pattern list.
   Ignores files which are in one of directories specified in
find-dired-simple-ignored-dirs"
  (interactive)
  (let* ((directory (read-directory-name "base directory: "))
         (patterns-string (read-string "file pattens: " nil
'find-dired-simple-history))
         (patterns (split-string patterns-string))
         (find-arguments (flatten
          `(
           "-not"
           "("
           ,(split-string (mapconcat (lambda (p) (concat "-path */" p
"/*")) find-dired-simple-ignored-dirs " -o "))
           ")"
           "-a"
           "-type"
           "f"
           "-a"
           "("
           ,(split-string (mapconcat (lambda (p) (concat "-name " p ))
patterns " -o "))
           ")"
           )))
         (find-arguments-quoted (mapconcat 'shell-quote-argument
find-arguments " "))
         )
    (find-dired directory find-arguments-quoted)))

(defun file-cache-save-cache-to-file (file)
  "Save contents of `file-cache-alist' to FILE.
For later retrieval using `file-cache-read-cache-from-file'"
  (interactive "FFile: ")
  (with-temp-file (expand-file-name file)
    (prin1 file-cache-alist (current-buffer))))

(defun file-cache-read-cache-from-file (file)
  "Clear `file-cache-alist' and read cache from FILE.
The file cache can be saved to a file using
`file-cache-save-cache-to-file'."
  (interactive "fFile: ")
  (file-cache-clear-cache)
  (let ((buf (find-file-noselect file)))
    (setq file-cache-alist (read buf))
    (kill-buffer buf)))


(defun decrease-number-at-point ()
  (interactive)
  (save-excursion
    (when (search-forward-regexp "[0-9]*" nil t)
      (let ((m (match-string 0)))
        (unless (string= "" m)
          (replace-match
           (number-to-string
            (- (string-to-number (match-string 0))
               1))))))))
