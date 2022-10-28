;;; parasail-mode.el --- major-mode for editing ParaSail sources

;; Copyright (C) 2015 Derick Eddington
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:
;;
;; This is basic and imperfect.  It exists only to have something until a better
;; mode is created.  Improvements are welcome.


(defgroup parasail nil
  "Major mode for editing ParaSail source-code."
  :group 'languages
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces))

(defconst parasail-delimiter-face 'parasail-delimiter-face)
(defface parasail-delimiter-face
  '((t (:inherit default)))
  "Face for delimiters."
  :group 'parasail)

(defconst parasail-enumeration-face 'parasail-enumeration-face)
(defface parasail-enumeration-face
  '((t (:inherit font-lock-constant-face)))
  "Face for enumeration literals."
  :group 'parasail)

(defcustom parasail-indent-width 4
  "Indentation width."
  :type 'integer
  :group 'parasail)


(eval-when-compile
  (defmacro define-parasail-regexps (&rest body)
    "Internal. Do not use"
    (let ((names (mapcar #'car body)))
      `(progn
         (eval-when-compile
           (defconst parasail-regexps-alist
             (let* ,body
               (list . ,(mapcar #'(lambda (n) `(cons ',n (rx-to-string ,n)))
                                names)))))
         . ,(mapcar
             #'(lambda (n)
                 `(defconst ,(intern (concat "parasail-rx-" (symbol-name n)))
                    (eval-when-compile
                      (cdr (assq ',n parasail-regexps-alist)))))
             names)))))

(define-parasail-regexps
  (identifier
   '(seq letter (* (char ?_ letter digit))))
  (decimal_numeral
   '(seq digit (* (char ?_ digit))))
  (decimal_integer_literal
   decimal_numeral)
  (binary_digit
   '(char ?0 ?1))
  (binary_numeral
   `(seq ,binary_digit (* (or ?_ ,binary_digit))))
  (binary_integer_literal
   `(seq ?0 (char ?b ?B) ,binary_numeral))
  (hex_digit
   'hex-digit)
  (hex_numeral
   `(seq ,hex_digit (* (or ?_ ,hex_digit))))
  (hex_integer_literal
   `(seq ?0 (char ?x ?X) ,hex_numeral))
  (extended_digit
   '(char digit (?A . ?Z) (?a . ?z)))
  (extended_numeral
   `(seq ,extended_digit (* (or ?_ ,extended_digit))))
  (based_integer_literal
   `(seq ,decimal_numeral ?# ,extended_numeral ?#))
  (integer_literal
   `(or ,based_integer_literal
        ,binary_integer_literal
        ,hex_integer_literal
        ,decimal_integer_literal))
  (exponent
   `(seq (char ?e ?E) (? (char ?+ ?-)) ,decimal_numeral))
  (decimal_real_literal
   `(seq ,decimal_numeral ?. ,decimal_numeral (? ,exponent)))
  (based_real_literal
   `(seq ,decimal_numeral ?#
         ,extended_numeral ?. ,extended_numeral ?# (? ,exponent)))
  (real_literal
   `(or ,decimal_real_literal
        ,based_real_literal))
  (escapable_character
   '(char ?\\ ?' ?\" ?n ?r ?t ?f ?0))
  (escaped_character
   `(seq ?\\ (or ,escapable_character (seq ?# ,hex_numeral ?#))))
  (character_literal
   `(seq ?' (or (not (any ?' ?\\)) ,escaped_character) ?'))
  (string_literal
   `(seq ?\" (*? (or (not (any ?\" ?\\)) ,escaped_character)) ?\"))
  (enumeration_literal
   `(seq ?# ,identifier))

  (number_literal
   `(seq (or bol (not (any letter digit ?_)))
         (group (or ,real_literal ,integer_literal))))
  (keyword
   '(seq symbol-start
         (or "abs" "abstract" "all" "and" "block" "case" "class"
             "concurrent" "const" "continue" "each" "else" "elsif" "end"
             "exit" "extends" "exports" "for" "forward" "func" "global"
             "if" "implements" "import" "in" "interface" "is" "lambda"
             "locked" "loop" "mod" "new" "not" "null" "of" "op" "optional"
             "or" "private" "queued" "ref" "rem" "return" "reverse"
             "separate" "some" "then" "type" "until" "var" "while" "with"
             "xor")
         symbol-end))
  (delimiter
   '(or "(" ")" "{" "}" "[" "]" "," ";" "." ":" "|" "<" ">" "+" "-"
        "*" "/" "'" "?" "::" ";;" "||" "==" "!=" "=?" "<=" ">=" "==>"
        "->" "**" "=>" "[[" "]]" "<<" ">>" ":=" "<==" "<=>" "<|="
        "+=" "-=" "*=" "/=" "**=" "<<=" ">>=" "|=" ".." "<.." "..<"
        "<..<" "and=" "or=" "xor="))
  (sk-char-lit
   '(seq (group ?')
         (or (not (any ?' ?\\))
             (seq ?\\ (or nonl (seq ?# (* (not (any ?# ?' ?\\))) ?#))))
         (group ?')))
  (line-indent-under
   '(seq symbol-start
         (or (or "block" "else" "exports")
             (seq (or "is" "of" "then")
                  (* space) eol)
             (seq (or "if" "elsif") symbol-end (* nonl)
                  symbol-start "then" (* space) eol)
             (seq "case" symbol-end (* nonl)
                  symbol-start "of" (* space) eol)
             (seq (? (or "for" "until" "while")
                     symbol-end (* nonl) symbol-start)
                  "loop" (* space) eol)
             (seq (? "abstract" (+ space))
                  (? "concurrent" (+ space))
                  (or "class" "interface") symbol-end (* nonl)
                  symbol-start "is" (* space) eol)
             (seq (? (or "abstract" "optional") (+ space))
                  (? "queued" (+ space))
                  (or "func" "op") symbol-end (* nonl)
                  symbol-start "is" (* space) eol))
         symbol-end))
  (line-negative-indent
   '(seq symbol-start (or "else" "elsif" "end" "exports") symbol-end))
  )


(defun parasail-indent-line ()
  "ParaSail mode version of the `indent-line-function'."
  (let ((o (current-column)) n s e)
    (save-excursion
      (save-match-data
        ; What beginning of current line looks like.
        (beginning-of-line)
        (skip-chars-forward "[:space:]")
        (setq s (current-column))
        (setq e (looking-at parasail-rx-line-negative-indent))
        ; Find non-empty previous line. It'll determine how to indent.
        (while (and (not n) (> (point) (point-min)))
          (beginning-of-line 0) ; Previous line.
          (skip-chars-forward "[:space:]")
          (cond
           ; Empty line, keep going.
           ((looking-at "$"))
           ; A kind of line to indent under.
           ((looking-at parasail-rx-line-indent-under)
            (setq n (+ (current-column) (if e 0 parasail-indent-width))))
           ; Indent same as previous regular line.
           (t
            (setq n (- (current-column) (if e parasail-indent-width 0))))))))
    (if n
        (progn (indent-line-to (max n 0))
               (when (> o s)
                 (move-to-column (+ (current-column) (- o s))))
               t)
      'noindent)))

(defun parasail-change-indent (w)
  "Unconditionally change indentation of current line."
  (let ((o (current-column)) s)
    (save-excursion
      (save-match-data
        (beginning-of-line)
        (skip-chars-forward "[:space:]")
        (setq s (current-column))))
    (indent-line-to (max (+ s w) 0))
    (when (> o s)
      (move-to-column (+ (current-column) (- o s))))))

(defun parasail-add-indent ()
  "Unconditionally add indentation to current line."
  (interactive)
  (parasail-change-indent parasail-indent-width))

(defun parasail-sub-indent ()
  "Unconditionally subtract indentation from current line."
  (interactive)
  (parasail-change-indent (- parasail-indent-width)))


(define-generic-mode parasail-mode
  ; Comments
  '("//")

  ; Keywords. Not given here, but below, to avoid highlighting enumerations as.
  '()

  ; Font-lock.  Note: The order of these is important to prevent incorrect
  ; highlighting.
  `((,parasail-rx-number_literal 1 font-lock-constant-face)
    (,parasail-rx-enumeration_literal . parasail-enumeration-face)
    ,parasail-rx-keyword  ; as font-lock-keyword-face
    (,parasail-rx-delimiter . parasail-delimiter-face))

  ; Auto-mode filename extensions
  `(,(rx ".ps" (char ?i ?l) eos))

  ; Functions to do additional configuration
  `(,#'(lambda ()
         ; Only give apostrophes "\"'" syntax when they make a reasonable
         ; character literal, so that primed object_names can have different
         ; apostrophes.
         (let ((sk `((,parasail-rx-sk-char-lit (1 "\"'") (2 "\"'")))))
           (if (eval-when-compile (fboundp 'syntax-propertize-via-font-lock))
               (set (make-local-variable 'syntax-propertize-function)
                    (syntax-propertize-via-font-lock sk))
             (set (make-local-variable 'font-lock-syntactic-keywords)
                  sk)))
         ; Keymap. So user can customize it.
         (use-local-map (make-sparse-keymap))
         ; Indentation
         (set (make-local-variable 'indent-line-function)
              'parasail-indent-line)
         ))
  )

(provide 'parasail-mode)
