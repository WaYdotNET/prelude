
;;; rlzx-mode.el --- Major mode for editing Slim-Openlaslzo mode

;; Copyright (c) 2013 Carlo Bertini

;; Author: Carlo Bertini
;; URL: http://github.com/WaYdotNET/rlaszlo
;; Version: 20130715
;; X-Original-Version: 0.1
;; Keywords: markup, language

;;; Commentary:

;; Because Slim's indentation schema is similar
;; to that of YAML and Python, many indentation-related
;; functions are similar to those in yaml-mode and python-mode.

;; To install, save this on your load path and add the following to
;; your .emacs file:

;;
;; (require 'rlaszlo-mode)

;; TODO: fix keyword (see new , var)

;;; Code:
(require 'compile)

(eval-when-compile
  (defvar font-lock-beg)
  (defvar font-lock-end)
  (require 'cl))

;; command



(defun rslazlo-compile-file ()
  "Execute the current file.
if the file is emacs lisp, run the byte compiled version if exist."
  (interactive)
  (let* (
         (fName (buffer-file-name))
         (fSuffix (file-name-extension fName))
         (oName (file-name-base fName))
         (progName "~/.emacs.d/personal/vendor/rlaszlo.rb")
         (params "")
         (cmdStr (concat progName " "  fName  ))
         )

    (when (buffer-modified-p)
      (when (y-or-n-p "Buffer modified. Do you want to save first?")
          (save-buffer) ) )

    (if (string-equal fSuffix "el") ; special case for emacs lisp
        (load (file-name-sans-extension fName))
      (if progName
          (progn
            (message "Running…")
            (shell-command cmdStr "*run-current-file output*" )
            )
        (message "No recognized program file suffix for this file.")
        ) ) ))



(defgroup rlaszlo nil
  "Support for the rlaszlo template language."
  :group 'languages
  :prefix "rlaszlo-")

(defcustom rlaszlo-mode-hook nil
  "Hook run when entering Rlaszlo mode."
  :type 'hook
  :group 'rlaszlo)

(defcustom rlaszlo-indent-offset 2
  "Amount of offset per level of indentation."
  :type 'integer
  :group 'rlaszlo)

(defcustom rlaszlo-backspace-backdents-nesting t
  "Non-nil to have `rlaszlo-electric-backspace' re-indent all code
nested beneath the backspaced line be re-indented along with the
line itself."
  :type 'boolean
  :group 'rlaszlo)

(defvar rlaszlo-indent-function 'rlaszlo-indent-p
  "This function should look at the current line and return true
if the next line could be nested within this line.")

(defvar rlaszlo-block-openers
  `("^ *\\([\\.#a-z][^ \t]*\\)\\(\\[.*\\]\\)?"
    "^ *[-=].*do[ \t]*\\(|.*|[ \t]*\\)?$"
    ,(concat "^ *-[ \t]*\\("
             (regexp-opt '("if" "unless" "while" "until" "else"
                           "begin" "elsif" "rescue" "ensure" "when"))
             "\\)")
    "^ *|"
    "^ */"
    "^ *[a-z0-9_]:")
  "A list of regexps that match lines of Rlaszlo that could have
text nested beneath them.")

;; Font lock

;; Helper for nested block (comment, embedded, text)
(defun rlaszlo-nested-re (re)
  (concat "^\\( *\\)" re "\n\\(?:\\(?:\\1 .*\\)\n\\)*"))

(defvar laszlo-tags
  '(
    ;; basic
    "canvas" "window" "attribute" "button" "text" "class" "node" "handler"
    "resource" "dataset" "method" "view" "include" "library"
    "tabslider" "tabelement" "state"
    ;; animation
    "animatorgroup" "animator"
    "script"
    ;; test
    "TestSuite" "TestCase"
    )
  "A list of all valid LASZLO4.0 tag names.")

(defvar laszlo-attrs
  '(
    ;; basic
    "bgcolor" "event" "name" "src" "datapath" "resource" "srtetches" "clip"
    "text" "args" "layout" "extends" "value" "type" "opacity"
    "align" "valign" "title" "resizable" "href" "applied"
    ;; mouse
    "cursor"
    ;; font
    "fontsize" "fontstyle" "font" "fgcolor"
    ;; event
    "onclick" "onerror" "onload" "ontimeout" "onaddsubresource"
    "ondbclick" "onmousedown" "onmousedragin" "onmousedragout" "onmouseout"
    "onmouseover" "onmouseup" "onmouseupoutside"
    "onblur" "onkeydown" "onkeyup"
    "clickable" "clickregion" "focusable" "focustrap"
    ;; coordinate
    "width" "height" "xoffset" "yoffset" "pixellock" "rotation"
    ;; method
    "bringToFront" "sendBehind" "sendInFrontOf" "sendToBack"
    ;; media
    "resourceheight" "resourcewidth" "source" "stretches" "unstretchedheight"
    "unstretchedwidth" "play"
    "onframe" "onlastframe" "onplay" "onstop"
    ;; slide
    "spacing" "slideduration"
    ;; animator
    "target" "attribute" "duration" "to"
    )
  "A list of all basic attributes.")

(defvar laszlo-keys
  '(
    "new" "var" "LzView" "switch" "case" "default" "break" "if" "else" "return"
    )
  "A list of all keyword valid.")
(defvar laszlo-specials
  '(
    "this" "super" "classroot"
    )
  "A list of special keyword.")

(defvar laszlo-debug
  '(
    "Debug" "lz"
    "assertEquals" "assertFalse" "assertNotNull" "assertNotSame" "assertNotUndefined"
    "assertNull" "assertSame" "assertTrue" "assertUndefined" "assertWithin"
    )
  "List keywords use in debug and testcase mode.")


(defvar laszlo-tags-re (concat "^ *\\(" (regexp-opt laszlo-tags 'words) "\/?\\)"))
(defvar laszlo-attrs-re (concat " *\\(" (regexp-opt laszlo-attrs 'words) "\/?\\)"))
(defvar laszlo-keys-re (concat " *\\(" (regexp-opt laszlo-keys 'words) "\/?\\)"))
(defvar laszlo-specials-re (concat " *\\(" (regexp-opt laszlo-specials 'words) ".\\)"))
(defvar laszlo-debug-re (concat " *\\(" (regexp-opt laszlo-debug 'words) "\\)"))


(defconst rlaszlo-font-lock-keywords
  `(
    ;; comment block
    (,(rlaszlo-nested-re "/.*")
     0 font-lock-comment-face)
    ;; Debug
    (, laszlo-debug-re
     1 font-lock-keyword-face)

    ;; keys
    (,laszlo-keys-re
     1 font-lock-constant-face)
    ;; specials
    (,laszlo-specials-re
     1 font-lock-type-face)
    ;; ;; this.
    ;; ("this."
    ;;  0 font-lock-type-face)

    ("['|\"]on\\\w+['|\"]" . font-lock-type-face)

    ;; attributes
    (,laszlo-attrs-re
     1 font-lock-constant-face)

    ;; embedded block
    (,(rlaszlo-nested-re "\\([a-z0-9_]+:\\)")
     0 font-lock-preprocessor-face)
    ;; text block
    (,(rlaszlo-nested-re "[\|'`].*")
     1 font-lock-string-face)
    ;; directive
    ("^!.*"
     0 font-lock-constant-face)
    ;; Single quote string TODO
    ("[^=]\\('[^'\n]*'\\)"
     1 font-lock-string-face append)
    ;; Double quoted string TODO
    ("\\(\"[^\"]*\"\\)"
     1 font-lock-string-face append)
    ;; Class variable TODO
    ("@[a-z0-9_]+"
     0 font-lock-variable-name-face append)
    ;; @var.method
    ("@[a-z0-9_]+"
     (0 font-lock-variable-name-face)
     ("\\.[a-z0-9_-]+" nil nil
      (0 font-lock-variable-name-face)))
    ;; ruby symbol
    (":\\w+" . font-lock-constant-face)
    ;; ruby symbol (1.9)
    ("\\w+:" . font-lock-constant-face)
    ;; #id
    ("^ *[a-z0-9_.-]*\\(#[a-z0-9_-]+\/?\\)"
     1 font-lock-keyword-face)
    ;; .class
    ("^ *[a-z0-9_#-]*\\(\\(\\.[a-z0-9_-]+\/?\\)+\\)"
     1 font-lock-type-face)
    ;; tag
    (,laszlo-tags-re
     1 font-lock-function-name-face)
    ;; doctype
    ("^\\(doctype .*$\\)"
     1 font-lock-preprocessor-face)
    ;; ==', =', -
    ("^ *\\(==?'?\\|-\\)"
      (1 font-lock-preprocessor-face)
      (,(regexp-opt
         '("if" "else" "elsif" "for" "in" "do" "unless"
           "while" "yield" "not" "and" "or")
         'words) nil nil
           (0 font-lock-keyword-face)))
    ;; tag ==, tag =

    ("^ *[\\.#a-z0-9_-]+.*[^<>!]\\(==?'?\\) +"
     1 font-lock-preprocessor-face)))

(defconst rlaszlo-embedded-re "^ *[a-z0-9_-]+:")
(defconst rlaszlo-comment-re  "^ */")

(defun* rlaszlo-extend-region ()
  "Extend the font-lock region to encompass embedded engines and comments."
  (let ((old-beg font-lock-beg)
        (old-end font-lock-end))
    (save-excursion
      (goto-char font-lock-beg)
      (beginning-of-line)
      (unless (or (looking-at rlaszlo-embedded-re)
                  (looking-at rlaszlo-comment-re))
        (return-from rlaszlo-extend-region))
      (setq font-lock-beg (point))
      (rlaszlo-forward-sexp)
      (beginning-of-line)
      (setq font-lock-end (max font-lock-end (point))))
    (or (/= old-beg font-lock-beg)
        (/= old-end font-lock-end))))


;; Mode setup

(defvar rlaszlo-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?: "." table)
    (modify-syntax-entry ?_ "w" table)
    table)
  "Syntax table in use in rlaszlo-mode buffers.")

(defvar rlaszlo-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [backspace] 'rlaszlo-electric-backspace)
    (define-key map "\C-?" 'rlaszlo-electric-backspace)
    (define-key map "\C-c\C-f" 'rlaszlo-forward-sexp)
    (define-key map "\C-c\C-b" 'rlaszlo-backward-sexp)
    (define-key map "\C-c\C-u" 'rlaszlo-up-list)
    (define-key map "\C-c\C-d" 'rlaszlo-down-list)
    (define-key map "\C-c\C-k" 'rlaszlo-kill-line-and-indent)
    map))

;; For compatibility with Emacs < 24, derive conditionally
(defalias 'rlaszlo-parent-mode
  (if (fboundp 'prog-mode) 'prog-mode 'fundamental-mode))

;;;###autoload
(define-derived-mode rlaszlo-mode rlaszlo-parent-mode "Rlaszlo"
  "Major mode for editing Rlaszlo files.

\\{rlaszlo-mode-map}"
  (set-syntax-table rlaszlo-mode-syntax-table)
  (add-to-list 'font-lock-extend-region-functions 'rlaszlo-extend-region)
  (set (make-local-variable 'font-lock-multiline) t)
  (set (make-local-variable 'indent-line-function) 'rlaszlo-indent-line)
  (set (make-local-variable 'indent-region-function) 'rlaszlo-indent-region)
  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  (setq comment-start "/")
  (setq indent-tabs-mode nil)
  (setq font-lock-defaults '((rlaszlo-font-lock-keywords) nil t)))

;; Useful functions

(defun rlaszlo-comment-block ()
  "Comment the current block of Rlaszlo code."
  (interactive)
  (save-excursion
    (let ((indent (current-indentation)))
      (back-to-indentation)
      (insert "/")
      (newline)
      (indent-to indent)
      (beginning-of-line)
      (rlaszlo-mark-sexp)
      (rlaszlo-reindent-region-by rlaszlo-indent-offset))))

(defun rlaszlo-uncomment-block ()
  "Uncomment the current block of Rlaszlo code."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (while (not (looking-at rlaszlo-comment-re))
      (rlaszlo-up-list)
      (beginning-of-line))
    (rlaszlo-mark-sexp)
    (kill-line 1)
    (rlaszlo-reindent-region-by (- rlaszlo-indent-offset))))

;; Navigation

(defun rlaszlo-forward-through-whitespace (&optional backward)
  "Move the point forward at least one line, until it reaches
either the end of the buffer or a line with no whitespace.

If `backward' is non-nil, move the point backward instead."
  (let ((arg (if backward -1 1))
        (endp (if backward 'bobp 'eobp)))
    (loop do (forward-line arg)
          while (and (not (funcall endp))
                     (looking-at "^[ \t]*$")))))

(defun rlaszlo-at-indent-p ()
  "Returns whether or not the point is at the first
non-whitespace character in a line or whitespace preceding that
character."
  (let ((opoint (point)))
    (save-excursion
      (back-to-indentation)
      (>= (point) opoint))))

(defun rlaszlo-forward-sexp (&optional arg)
  "Move forward across one nested expression.
With `arg', do it that many times.  Negative arg -N means move
backward across N balanced expressions.

A sexp in Rlaszlo is defined as a line of Rlaszlo code as well as any
lines nested beneath it."
  (interactive "p")
  (or arg (setq arg 1))
  (if (and (< arg 0) (not (rlaszlo-at-indent-p)))
      (back-to-indentation)
    (while (/= arg 0)
      (let ((indent (current-indentation)))
        (loop do (rlaszlo-forward-through-whitespace (< arg 0))
              while (and (not (eobp))
                         (not (bobp))
                         (> (current-indentation) indent)))
        (back-to-indentation)
        (setq arg (+ arg (if (> arg 0) -1 1)))))))

(defun rlaszlo-backward-sexp (&optional arg)
  "Move backward across one nested expression.
With ARG, do it that many times.  Negative arg -N means move
forward across N balanced expressions.

A sexp in Rlaszlo is defined as a line of Rlaszlo code as well as any
lines nested beneath it."
  (interactive "p")
  (rlaszlo-forward-sexp (if arg (- arg) -1)))

(defun rlaszlo-up-list (&optional arg)
  "Move out of one level of nesting.
With ARG, do this that many times."
  (interactive "p")
  (or arg (setq arg 1))
  (while (> arg 0)
    (let ((indent (current-indentation)))
      (loop do (rlaszlo-forward-through-whitespace t)
            while (and (not (bobp))
                       (>= (current-indentation) indent)))
      (setq arg (- arg 1))))
  (back-to-indentation))

(defun rlaszlo-down-list (&optional arg)
  "Move down one level of nesting.
With ARG, do this that many times."
  (interactive "p")
  (or arg (setq arg 1))
  (while (> arg 0)
    (let ((indent (current-indentation)))
      (rlaszlo-forward-through-whitespace)
      (when (<= (current-indentation) indent)
        (rlaszlo-forward-through-whitespace t)
        (back-to-indentation)
        (error "Nothing is nested beneath this line"))
      (setq arg (- arg 1))))
  (back-to-indentation))

(defun rlaszlo-mark-sexp ()
  "Marks the next Rlaszlo block."
  (let ((forward-sexp-function 'rlaszlo-forward-sexp))
    (mark-sexp)))

(defun rlaszlo-mark-sexp-but-not-next-line ()
  "Marks the next Rlaszlo block, but puts the mark at the end of the
last line of the sexp rather than the first non-whitespace
character of the next line."
  (rlaszlo-mark-sexp)
  (set-mark
   (save-excursion
     (goto-char (mark))
     (forward-line -1)
     (end-of-line)
     (point))))

;; Indentation and electric keys

(defun rlaszlo-indent-p ()
  "Returns true if the current line can have lines nested beneath it."
  (loop for opener in rlaszlo-block-openers
        if (looking-at opener) return t
        finally return nil))

(defun rlaszlo-compute-indentation ()
  "Calculate the maximum sensible indentation for the current line."
  (save-excursion
    (beginning-of-line)
    (if (bobp) 0
      (rlaszlo-forward-through-whitespace t)
      (+ (current-indentation)
         (if (funcall rlaszlo-indent-function) rlaszlo-indent-offset
           0)))))

(defun rlaszlo-indent-region (start end)
  "Indent each nonblank line in the region.
This is done by indenting the first line based on
`rlaszlo-compute-indentation' and preserving the relative
indentation of the rest of the region.

If this command is used multiple times in a row, it will cycle
between possible indentations."
  (save-excursion
    (goto-char end)
    (setq end (point-marker))
    (goto-char start)
    (let (this-line-column current-column
          (next-line-column
           (if (and (equal last-command this-command) (/= (current-indentation) 0))
               (* (/ (- (current-indentation) 1) rlaszlo-indent-offset) rlaszlo-indent-offset)
             (rlaszlo-compute-indentation))))
      (while (< (point) end)
        (setq this-line-column next-line-column
              current-column (current-indentation))
        ;; Delete whitespace chars at beginning of line
        (delete-horizontal-space)
        (unless (eolp)
          (setq next-line-column (save-excursion
                                   (loop do (forward-line 1)
                                         while (and (not (eobp)) (looking-at "^[ \t]*$")))
                                   (+ this-line-column
                                      (- (current-indentation) current-column))))
          ;; Don't indent an empty line
          (unless (eolp) (indent-to this-line-column)))
        (forward-line 1)))
    (move-marker end nil)))

(defun rlaszlo-indent-line ()
  "Indent the current line.
The first time this command is used, the line will be indented to the
maximum sensible indentation.  Each immediately subsequent usage will
back-dent the line by `rlaszlo-indent-offset' spaces.  On reaching column
0, it will cycle back to the maximum sensible indentation."
  (interactive "*")
  (let ((ci (current-indentation))
        (cc (current-column))
        (need (rlaszlo-compute-indentation)))
    (save-excursion
      (beginning-of-line)
      (delete-horizontal-space)
      (if (and (equal last-command this-command) (/= ci 0))
          (indent-to (* (/ (- ci 1) rlaszlo-indent-offset) rlaszlo-indent-offset))
        (indent-to need)))
      (if (< (current-column) (current-indentation))
          (forward-to-indentation 0))))

(defun rlaszlo-reindent-region-by (n)
  "Add N spaces to the beginning of each line in the region.
If N is negative, will remove the spaces instead.  Assumes all
lines in the region have indentation >= that of the first line."
  (let ((ci (current-indentation)))
    (save-excursion
      (while (re-search-forward (concat "^" (make-string ci ? )) nil t)
        (replace-match (make-string (max 0 (+ ci n)) ? ) nil nil)))))

(defun rlaszlo-electric-backspace (arg)
  "Delete characters or back-dent the current line.
If invoked following only whitespace on a line, will back-dent
the line and all nested lines to the immediately previous
multiple of `rlaszlo-indent-offset' spaces.

Set `rlaszlo-backspace-backdents-nesting' to nil to just back-dent
the current line."
  (interactive "*p")
  (if (or (/= (current-indentation) (current-column))
          (bolp)
          (looking-at "^[ \t]+$"))
      (backward-delete-char arg)
    (save-excursion
      (let ((ci (current-column)))
        (beginning-of-line)
        (if rlaszlo-backspace-backdents-nesting
            (rlaszlo-mark-sexp-but-not-next-line)
          (set-mark (save-excursion (end-of-line) (point))))
        (rlaszlo-reindent-region-by (* (- arg) rlaszlo-indent-offset))
        (back-to-indentation)
        (pop-mark)))))

(defun rlaszlo-kill-line-and-indent ()
  "Kill the current line, and re-indent all lines nested beneath it."
  (interactive)
  (beginning-of-line)
  (rlaszlo-mark-sexp-but-not-next-line)
  (kill-line 1)
  (rlaszlo-reindent-region-by (* -1 rlaszlo-indent-offset)))

(defun rlaszlo-indent-string ()
  "Return the indentation string for `rlaszlo-indent-offset'."
  (mapconcat 'identity (make-list rlaszlo-indent-offset " ") ""))

;;;###autoload

;; (add-hook 'rlaszlo-mode-hook
;;    (lambda ()
;;      (add-hook 'after-save-hook 'rslazlo-compile-file)))

(add-to-list 'auto-mode-alist '("\\.rlzx" . rslazlo-mode))
(add-to-list 'auto-mode-alist '("\\.rlaszlo\\'" . rlaszlo-mode))
(add-to-list 'auto-mode-alist '("\\.rlzx$" . rslaszlo-mode))
;; Setup/Activation
(provide 'rlaszlo-mode)
;;; rlaszlo-mode.el ends here
