;;; personal-bindings.el -- Personal keybinds
;;; Commentary:
;;; List of personal shortcuts
;;; Code:

;; Navigate between windows using Alt-1, Alt-2, Shift-left, shift-up, shift-right
(windmove-default-keybindings)

;; Enable copy and pasting from clipboard
(setq x-select-enable-clipboard t)

(defvar my-keys-minor-mode-map (make-keymap) "WaY-Key")

(define-minor-mode my-keys-minor-mode
  "A minor mod for my custom keys"
  t " my-keys" 'my-keys-minor-mode-map)
(my-keys-minor-mode t)

;; Non ci sta bisogno di commentare questo codice
;; (define-key my-keys-minor-mode-map (kbd "RET") 'newline-and-indent)


;; open current directory in iTerm2 from Emacs
;; http://dlvr.it/5ySmRy
(define-key my-keys-minor-mode-map (kbd "C-0") 'iterm-here)

;; move current line up or down
(define-key my-keys-minor-mode-map [(meta shift up)] 'move-line-up)
(define-key my-keys-minor-mode-map [(meta shift down)] 'move-line-down)

 ;; fullscreen
(define-key my-keys-minor-mode-map (kbd "M-RET") 'toggle-fullscreen)

 ;; find-temp-file
(define-key my-keys-minor-mode-map (kbd "C-x C-t") 'find-temp-file)

;; ;; project-explorer
;; (define-key my-keys-minor-mode-map (kbd "M-1") 'project-explorer-open)
;; neotree
(define-key my-keys-minor-mode-map (kbd "M-1") 'neotree-toggle)

;; cambio il mapping di default di taglio (C-w)
(define-key my-keys-minor-mode-map [remap kill-region] 'cut-line-or-region)

;; cambio il mapping di default di copia (M-w)
(define-key my-keys-minor-mode-map [remap kill-ring-save] 'copy-line-or-region)

(define-key my-keys-minor-mode-map (kbd "M-x") 'smex)
(define-key my-keys-minor-mode-map (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(define-key my-keys-minor-mode-map (kbd "C-c C-c M-x") 'execute-extended-command)


 (add-hook 'focus-out-hook (lambda () (save-some-buffers t)))

;; use regex searches by default.
(define-key my-keys-minor-mode-map (kbd "C-s") 'isearch-forward-regexp)
(define-key my-keys-minor-mode-map (kbd "\C-r") 'isearch-backward-regexp)
(define-key my-keys-minor-mode-map (kbd "C-M-s") 'isearch-forward)
(define-key my-keys-minor-mode-map (kbd "C-M-r") 'isearch-backward)

(define-key my-keys-minor-mode-map (kbd "C-x O") (lambda ()
                                (interactive)
                                (other-window -1)))
(define-key my-keys-minor-mode-map (kbd "C-x C-o") (lambda ()
                                  (interactive)
                                  (other-window 2)))

;; Create ad empty buffer
(define-key my-keys-minor-mode-map (kbd "C-c C-n") (generate-new-buffer "temp"))

;; Start a regular shell if you prefer that.
(define-key my-keys-minor-mode-map (kbd "C-x M-m") 'shell)

;; Fetch the contents at a URL, display it raw.
(define-key my-keys-minor-mode-map (kbd "C-x C-h") 'view-url)

;; auto-complete-mode
(define-key my-keys-minor-mode-map (kbd "M-q") 'auto-complete-m)

;; comment line
(define-key my-keys-minor-mode-map (kbd "M-7") 'comment-or-uncomment-current-line-or-region)

;; OCCUR
(define-key my-keys-minor-mode-map (kbd "C-c C-o") 'occur)
;; redo+
(define-key my-keys-minor-mode-map (kbd "C-?") 'redo)

;; helm imenu
(define-key my-keys-minor-mode-map (kbd "M-2") 'helm-semantic-or-imenu)

;; whitespace
(define-key my-keys-minor-mode-map (kbd "C-*") 'whitespace-mode)

;; django
(define-key my-keys-minor-mode-map (kbd "C-x j") 'python-django-open-project)

;; openlaszlo
(define-key my-keys-minor-mode-map (kbd "C-c C-c C-c") 'rslazlo-compile-file)

;; hide ^M in svn-output
(define-key my-keys-minor-mode-map (kbd "C-c C-.") 'remove-dos-eol)

;; Align your code in a pretty way.
(define-key my-keys-minor-mode-map (kbd "C-x \\") 'align-regexp)

;; Font size
(define-key my-keys-minor-mode-map (kbd "C-+") 'text-scale-increase)
(define-key my-keys-minor-mode-map (kbd "C--") 'text-scale-decrease)

;; Window switching. (C-x o goes to the next window)
(define-key my-keys-minor-mode-map (kbd "C-x O") (lambda ()
                                (interactive)
                                (other-window -1))) ;; back one
;; kill lines backward
(define-key my-keys-minor-mode-map (kbd "C-<backspace>") (lambda ()
                                        (interactive)
                                        (kill-line 0)
                                        (indent-according-to-mode)))

;; kill-other-buffers
(define-key my-keys-minor-mode-map (kbd "C-c C-c C-k") 'kill-other-buffers)

;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-c o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp
                 isearch-string
               (regexp-quote isearch-string))))))

;; use hippie-expand instead of dabbrev
(define-key my-keys-minor-mode-map (kbd "M-/") 'hippie-expand)

;; replace buffer-menu with ibuffer
(define-key my-keys-minor-mode-map (kbd "C-x C-b") 'ibuffer)

;; magiìt
(define-key my-keys-minor-mode-map (kbd "C-x g") 'magit-status)
(define-key my-keys-minor-mode-map (kbd "C-x p") 'magit-push)

;; expand region!
(define-key my-keys-minor-mode-map (kbd "C-o") 'er/expand-region)

;; jump
(define-key my-keys-minor-mode-map (kbd "C-c SPC") 'ace-jump-mode)
;; (define-key my-keys-minor-mode-map (kbd "C-c j") 'ace-jump-mode)
(define-key my-keys-minor-mode-map (kbd "s-.") 'ace-jump-mode)
(define-key my-keys-minor-mode-map (kbd "C-c J") 'ace-jump-buffer)
(define-key my-keys-minor-mode-map (kbd "s->") 'ace-jump-buffer)

;; muoviamoci tra le window
(define-key my-keys-minor-mode-map (kbd "M-<left>")  'windmove-left)
(define-key my-keys-minor-mode-map (kbd "M-<right>") 'windmove-right)
(define-key my-keys-minor-mode-map (kbd "M-<up>")    'windmove-up)
(define-key my-keys-minor-mode-map (kbd "M-<down>")  'windmove-down)


(add-hook 'js2-mode-hook
          #'(lambda ()
              (define-key my-keys-minor-mode-map (kbd "C-c i") 'js-doc-insert-function-doc)
              (define-key my-keys-minor-mode-map (kbd "@") 'js-doc-insert-tag)))

;;; personal-bindings.el ends here
