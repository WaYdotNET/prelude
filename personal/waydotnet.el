;;; pacakge -- waydotnet.el
;;; commentary:
;; bypassiamo la schermata di benvenuto

;; requirement

(prelude-require-package 'nyan-mode)
(prelude-require-package 'smex)
(prelude-require-package 'flx-ido)
(prelude-require-package 'highlight-indentation)
(prelude-require-package 'smart-mode-line)
(prelude-require-package 'find-temp-file)
(prelude-require-package 'diff-hl)
(prelude-require-package 'ido-vertical-mode)
(prelude-require-package 'workgroups2)
(prelude-require-package 'project-explorer)
(prelude-require-package 'visual-regexp)
(prelude-require-package 'indent-guide)
(prelude-require-package 'anaconda-mode)
(prelude-require-package 'company-anaconda)
(prelude-require-package 'pyenv-mode)
(prelude-require-package 'pony-mode)
(prelude-require-package 'web-mode)
(prelude-require-package 'python-django)
(prelude-require-package 'neotree)
(prelude-require-package 'flymake-jshint)
(prelude-require-package 'flymake-jslint)
(prelude-require-package 'flymake-json)
(prelude-require-package 'js2-mode)
(prelude-require-package 'js2-refactor)
(prelude-require-package 'ac-js2)
(prelude-require-package 'virtualenvwrapper)
(prelude-require-package 'auto-highlight-symbol)
(prelude-require-package 'tern)
(prelude-require-package 'company-tern)
(prelude-require-package 'helm-company)
(prelude-require-package 'parenface)

(global-auto-highlight-symbol-mode t)

(require 'virtualenvwrapper)
(venv-initialize-interactive-shells) ;; if you want interactive shell support
(venv-initialize-eshell) ;; if you want eshell support
(setq venv-location "/Volumes/home/works/gitlab/anfora/env/")

(require 'company)
;; js
(setq js2-highlight-level 3)
(prelude-require-package 'auto-highlight-symbol)
(add-to-list 'company-backends 'company-tern)



;;; Code:

;;;; MAC !!!! FIX
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)

(eval-after-load 'company
  '(progn
     (define-key company-mode-map (kbd "C-:") 'helm-company)
     (define-key company-active-map (kbd "C-:") 'helm-company)))

(add-hook 'after-init-hook 'global-company-mode)
(setq inhibit-startup-message t)
;; impostiamo il tab di default a 2 caratteri
(setq-default tab-width 2)
;; usiamo gli spazi al posto dei tab !!!
(setq-default indent-tabs-mode nil)
;; show column number in mode-line
(setq column-number-mode t)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


;; fun :D
(require 'nyan-mode)
(nyan-mode)

;; flat !
(set-face-attribute 'header-line nil :box nil)
(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)

(require 'workgroups2)
;; autoload/autosave:
;; if you start Emacs as "emacs --daemon" - turn off autoloading of workgroups:
(setq wg-use-default-session-file t)


;; save/restore opened files and windows config
(desktop-save-mode +1) ; 0 for off
;; (desktop-read)
;; salviamo la posizione del cursore quando chiudo un buffer
;; in modo da ripristinarla
;; (require 'saveplace)
;; (setq save-place-file (concat user-emacs-directory "saveplace"))

;; change color of bracket, curly
;; (require 'parenface)
;; (eval-after-load 'parenface
;;   (progn
;;     (set-face-foreground 'parenface-paren-face "dark gray")
;;     (set-face-foreground 'parenface-bracket-face "gray")
;;     (set-face-foreground 'parenface-curly-face "gray")))

;; (add-hook 'lisp-mode-hook 'paren-face-add-keyword)
;; ;; (add-hook 'js2-mode-hook 'paren-face-add-keyword)
;; (add-hook 'python-mode-hook 'paren-face-add-keyword)
;; (add-hook 'emacs-lisp-mode-hook 'paren-face-add-keyword)
;; (add-hook 'emacs-program-mode-hook 'paren-face-add-keyword)

;; ASPELL !!!
;; $ sudo port -v selfupdate
;; $ sudo port install aspell-dict-en
(setq ispell-program-name "/opt/local/bin/aspell")

;; smart-mode-line
(setq sml/theme 'dark)
(sml/setup)

;; python mode
(add-hook 'python-mode-hook 'anaconda-mode)
(add-to-list 'company-backends 'company-anaconda)
(pyenv-mode)

;; full screen toggle using command+[RET]
(defun toggle-fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen)
                                           nil
                                         'fullboth)))

;; How to copy from one dired dir to the next dired dir shown in a split window?
(setq dired-dwim-target t)


;; (set-frame-font "Source Code Pro-11")

;; Highlight Current Line
(global-hl-line-mode +1)
(require 'highlight-indentation)

(setq frame-char (frame-char-width (selected-frame)))
;; disabilito la acapo automatico
(global-visual-line-mode t)

;; use fuzzy style matching for commands aswell
(require 'smex)
(smex-initialize)
(require 'find-temp-file)

;; Show the current function name in the header line
(which-function-mode)
(setq which-func-unknown "n/a")
(setq-default header-line-format
              '((which-func-mode ("" which-func-format " "))))
(setq mode-line-misc-info
      ;; We remove Which Function Mode from the mode line, because it's mostly
      ;; invisible here anyway.
      (assq-delete-all 'which-func-mode mode-line-misc-info))

;; line numbering with linum (WTF it's not activated by defaults?!)
(global-linum-mode 1)
(setq linum-disabled-modes-list '(mu4e-headers-mode))
(defun linum-on ()
  (unless (or (minibufferp) (member major-mode linum-disabled-modes-list))
    (linum-mode 1)))
;; disable linum for speedbar
(add-hook 'speedbar-mode-hook  '(lambda () (linum-mode 0)))

(require 'diff-hl)
(global-diff-hl-mode 1)
;; ido mode per il completamento dei file e dir
(ido-mode 1)
(ido-everywhere 1)
;; flx mode .. like sublimeText cmd-T
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)
;; preferisco il vertical !
(ido-vertical-mode t)

;; (require 'workgroups2)
;; autoload/autosave:
;; if you start Emacs as "emacs --daemon" - turn off autoloading of workgroups:
(setq wg-use-default-session-file t)

;; utf-8 powaa!!
(set-language-environment 'utf-8)
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-input-method nil)



;; uniquify buffer names
;; (require 'uniquify) ; bundled with GNU emacs 23.2.1 or before. On in 24.4
;; (setq uniquify-buffer-name-style 'post-forward-angle-brackets) ; emacs 24.4 style ⁖ cat.png<dirName>
;; (setq uniquify-buffer-name-style 'reverse)
;; (setq uniquify-separator "/")
;; (setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
;; (setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; (require 'rainbow-delimiters)
;; (global-rainbow-delimiters-mode)

;; visual-regexp
(require 'visual-regexp)
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)

;; EDITOR
(global-auto-revert-mode 1)

;; linea di identazione
(require 'indent-guide)
(setq-default indent-guide-mode t)
(indent-guide-global-mode)

;; change color of bracket, curly
(require 'parenface)
(eval-after-load 'parenface
  (progn
    (set-face-foreground 'parenface-paren-face "dark gray")
    (set-face-foreground 'parenface-bracket-face "gray")
    (set-face-foreground 'parenface-curly-face "gray")))

;; org-mode
;; HACK: http://nickhigham.wordpress.com/2013/07/05/emacs-org-mode-version-8/
;;
;; fix org-mode 8:
;; start emacs without custom (emacs -Q)
;; delete org (rm -rf ~/.emacs.d/elpa/org-TAB)
;; M-x package-install org

;;;;;;; ;; Include the latex-exporter
;;;;;;; (require 'ox-latex)
;;;;;;; ;; Add minted to the defaults packages to include when exporting.
;;;;;;; (add-to-list 'org-latex-packages-alist '("" "minted"))
;;;;;;; ;; Tell the latex export to use the minted package for source
;;;;;;; ;; code coloration.
;;;;;;; (setq org-latex-listings 't)
;;;;;;; (setq org-latex-listings 'minted)

;;;;;;; ;; #+LATEX_HEADER: \input{ABSOLUTEPATH\lib.tex}

;;;;;;; ;; Let the exporter use the -shell-escape option to let latex
;;;;;;; ;; execute external programs.
;;;;;;; ;; This obviously and can be dangerous to activate!
;;;;;;; (setq org-latex-pdf-process
;;;;;;;       '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
;;;;;;; ;; org-mode syntax color for code block :D
;; (setq org-src-fontify-natively t)

;; EX zencode... C-j per attivare la preview del autocompletamento
;;(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'web-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
;; (add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2))) ;; indent 2 spaces.


(provide 'waydotnet)
;;; waydotnet.el ends here
