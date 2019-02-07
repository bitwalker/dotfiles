;;;###autoload
(add-to-list 'auto-mode-alist '("\\.pest\\'" . pest-mode))

(defvar pest-mode-hook nil)
(defvar pest-new-file-hook nil)
;; User Variables

(defgroup pest nil
  "Customization of Pest mode."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'languages)

(defcustom pest-indent 4
  "Amount of horizontal space to indent.
After, for instance, an opening brace"
  :type 'integer
  :group 'pest)

(defface font-lock-reserved-word-face
  '((((class color))))
  "Basic face for reserved words in the Pest grammar."
  :group 'basic-faces)

;;;###autoload
(define-derived-mode pest-mode prog-mode "Pest"
    "Major mode for editing Pest source files in Emacs"
    :syntax-table nil
    (pest-syntax-table-init)
    (set-face-foreground 'font-lock-reserved-word-face "red")
    (pest-font-lock-init)
    (run-hooks 'pest-mode-hook)

    ;; Align |
    (add-to-list 'align-rules-list
                 '(pest-alternations
                    (regexp  . "\\(\\s-*\\)\\(|\\)\\s-*")
                    (modes   . '(pest-mode))
                    (repeat  . t)))
    (if (zerop (buffer-size))
        (run-hooks 'pest-new-file-hook)))

(defun pest-syntax-table-init ()
  (pest-ensure-syntax-table-is-initialized)
  (set-syntax-table pest-mode-syntax-table))

(defvar pest-mode-syntax-table nil
  "Syntax table in use in pest-mode buffers.")

(defvar pest-font-lock-syntax-table nil
  "Syntax table used by font lock mode")

(defun pest-ensure-syntax-table-is-initialized ()
  (unless pest-mode-syntax-table
    (let ((table (make-syntax-table)))
      (modify-syntax-entry ?\/ ". 12b" table)
      (modify-syntax-entry ?\n "> b" table)
      (modify-syntax-entry ?\" "\"" table)
      (modify-syntax-entry ?\' "\"" table)
      (modify-syntax-entry ?_ "_" table)
      (modify-syntax-entry ?@ "_" table)
      (modify-syntax-entry ?$ "_" table)
      (modify-syntax-entry ?! "_" table)
      (modify-syntax-entry ?~ "_" table)
      (modify-syntax-entry ?| "_" table)
      (modify-syntax-entry ?* "_" table)
      (modify-syntax-entry ?+ "_" table)
      (modify-syntax-entry ?& "_" table)

      (setq pest-mode-syntax-table table))))

(defun pest-font-lock-init ()
  (or pest-font-lock-syntax-table
      (setq pest-font-lock-syntax-table
            (let ((table (copy-syntax-table pest-mode-syntax-table)))
              (modify-syntax-entry ?_ "w" table)
              table)))
  (set (make-local-variable 'font-lock-defaults)
       '(pest-font-lock-highlights)))

(setq pest-font-lock-highlights
      (let* (
             (pest-keywords '("PUSH" "POP_ALL" "POP" "PEEK_ALL" "PEEK" "DROP"))
             (pest-special '("WHITESPACE" 
                             "COMMENT" 
                             "ANY" 
                             "SOI" 
                             "EOI" 
                             "ASCII_DIGIT" 
                             "ASCII_NONZERO_DIGIT"
                             "ASCII_BIN_DIGIT"
                             "ASCII_OCT_DIGIT"
                             "ASCII_HEX_DIGIT"
                             "ASCII_ALPHA_LOWER"
                             "ASCII_ALPHA_UPPER"
                             "ASCII_ALPHA"
                             "ASCII_ALPHANUMERIC"
                             "ASCII"
                             "NEWLINE"
                             "LETTER"
                             "CASED_LETTER"
                             "UPPERCASE_LETTER"
                             "LOWERCASE_LETTER"
                             "TITLECASE_LETTER"
                             "MODIFIER_LETTER"
                             "OTHER_LETTER"
                             "MARK"
                             "NONSPACING_MARK"
                             "SPACING_MARK"
                             "ENCLOSING_MARK"
                             "NUMBER"
                             "DECIMAL_NUMBER"
                             "LETTER_NUMBER"
                             "OTHER_NUMBER"
                             "PUNCTUATION"
                             "CONNECTOR_PUNCTUATION"
                             "CONNECTOR_PUNCTUATION"
                             "DASH_PUNCTUATION"
                             "OPEN_PUNCTUATION"
                             "CLOSE_PUNCTUATION"
                             "INITIAL_PUNCTUATION"
                             "FINAL_PUNCTUATION"
                             "OTHER_PUNCTUATION"
                             "SYMBOL"
                             "MATH_SYMBOL"
                             "CURRENCY_SYMBOL"
                             "MODIFIER_SYMBOL"
                             "OTHER_SYMBOL"
                             "SEPARATOR"
                             "SPACE_SEPARATOR"
                             "LINE_SEPARATOR"
                             "PARAGRAPH_SEPARATOR"
                             "OTHER"
                             "CONTROL"
                             "FORMAT"
                             "SURROGATE"
                             "PRIVATE_USE"
                             "UNASSIGNED"
                             "ALPHABETIC"
                             "BIDI_CONTROL"
                             "CASE_IGNORABLE"
                             "CASED"
                             "CHANGES_WHEN_CASEFOLDED"
                             "CHANGES_WHEN_CASEMAPPED"
                             "CHANGES_WHEN_LOWERCASED"
                             "CHANGES_WHEN_TITLECASED"
                             "CHANGES_WHEN_UPPERCASED"
                             "DASH"
                             "DEFAULT_IGNORABLE_CODE_POINT"
                             "DEPRECATED"
                             "DIACRITIC"
                             "EXTENDER"
                             "GRAPHEME_BASE"
                             "GRAPHEME_EXTEND"
                             "GRAPHEME_LINK"
                             "HEX_DIGIT"
                             "HYPHEN"
                             "IDS_BINARY_OPERATOR"
                             "IDS_TRINARY_OPERATOR"
                             "ID_CONTINUE"
                             "ID_START"
                             "IDEOGRAPHIC"
                             "JOIN_CONTROL"
                             "LOGICAL_ORDER_EXCEPTION"
                             "LOWERCASE"
                             "MATH"
                             "NONCHARACTER_CODE_POINT"
                             "OTHER_ALPHABETIC"
                             "OTHER_DEFAULT_IGNORABLE_CODE_POINT"
                             "OTHER_GRAPHEME_EXTEND"
                             "OTHER_ID_CONTINUE"
                             "OTHER_ID_START"
                             "OTHER_LOWERCASE"
                             "OTHER_MATH"
                             "OTHER_UPPERCASE"
                             "PATTERN_SYNTAX"
                             "PATTERN_WHITE_SPACE"
                             "PREPENDED_CONCATENATION_MARK"
                             "QUOTATION_MARK"
                             "RADICAL"
                             "REGIONAL_INDICATOR"
                             "SENTENCE_TERMINAL"
                             "SOFT_DOTTED"
                             "TERMINAL_PUNCTUATION"
                             "UNIFIED_IDEOGRAPH"
                             "UPPERCASE"
                             "VARIATION_SELECTOR"
                             "WHITE_SPACE"
                             "XID_CONTINUE"
                             "XID_START"))
             (pest-forbidden '("abstract"
                               "alignof"
                               "as"
                               "become"
                               "box"
                               "break"
                               "const"
                               "continue"
                               "crate"
                               "do"
                               "else"
                               "enum"
                               "external"
                               "false"
                               "final"
                               "fn"
                               "for"
                               "if"
                               "impl"
                               "in"
                               "let"
                               "loop"
                               "macro"
                               "match"
                               "mod"
                               "move"
                               "mut"
                               "offsetof"
                               "override"
                               "priv"
                               "proc"
                               "pure"
                               "pub"
                               "ref"
                               "return"
                               "Self"
                               "self"
                               "sizeof"
                               "static"
                               "struct"
                               "super"
                               "trait"
                               "true"
                               "type"
                               "typeof"
                               "unsafe"
                               "unsized"
                               "use"
                               "virtual"
                               "where"
                               "while"
                               "yield"))
             (pest-operators '("@"
                               "$"
                               "!"
                               "~"
                               "|"
                               "*"
                               "+"
                               "?"
                               "&"))

             (pest-special-regexp (regexp-opt pest-special 'words))
             (pest-keywords-regexp (regexp-opt pest-keywords 'words))
             (pest-operators-regexp (regexp-opt pest-operators 'symbols))
             (pest-forbidden-regexp (regexp-opt pest-forbidden 'words)))

        `(
          ;; Keywords, etc.
          (,pest-special-regexp . font-lock-constant-face)
          (,pest-keywords-regexp . font-lock-keyword-face)
          (,pest-operators-regexp . font-lock-keyword-face)
          ;;Rule definitions
          ("\\_<[[:alpha:]]\\(?:\\sw\\|\\s_\\)*\\_>[[:space:]]+=" . font-lock-function-name-face)
          ;;Rule usages
          ("\\_<[[:alpha:]]\\(?:\\sw\\|\\s_\\)*\\_>" . font-lock-variable-name-face)
          ;; Invalid names
          (,pest-forbidden-regexp . font-lock-reserved-word-face))))

(provide 'pest)
