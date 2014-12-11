;;; lzx -- Personal keybinds
;;; Commentary:
;;; Enable js2-mode into lzx file
;;; Code:
;;


(require 'mmm-mode)
;;(require 'mmm-auto)
;; (require 'cl)
;;(require 'mmm-auto)
(require 'mmm-vars)

;;; Code:g
(setq mmm-global-mode 'maybe)

(setq mmm-submode-decoration-level 0)
;; (flyspell-mode 0)

(mmm-add-group
 'lzx
 '((js-script-cdata
    :submode js-mode
    :face mmm-code-submode-face
    :front "<!\\[CDATA\\[[ \t]*\n?"
    :back "[ \t]*\\(//\\)?]]>")

   )
 )
(add-to-list 'mmm-mode-ext-classes-alist '(nxml-mode nil lzx))
;; Setup/Activation
(provide 'lzx)
;;; lzx.el ends here
