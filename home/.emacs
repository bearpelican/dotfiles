;disable scratch message on load
;(setq initial-scratch-message nil)

;disable backup
(setq backup-inhibited t)
;disable auto save
(setq auto-save-default nil)
;word wrap
(defadvice isearch-repeat (after isearch-no-fail activate)
  (unless isearch-success
    (ad-disable-advice 'isearch-repeat 'after 'isearch-no-fail)
    (ad-activate 'isearch-repeat)
    (isearch-repeat (if isearch-forward 'forward))
    (ad-enable-advice 'isearch-repeat 'after 'isearch-no-fail)
    (ad-activate 'isearch-repeat)))

;prompt to save directory if it doesn't exist
(add-hook 'before-save-hook
          (lambda ()
            (when buffer-file-name
              (let ((dir (file-name-directory buffer-file-name)))
                (when (and (not (file-exists-p dir))
                           (y-or-n-p (format "Directory %s does not exist. Create it?" dir)))
                  (make-directory dir t))))))

;key bindings
(global-set-key (kbd "C-M-h") 'backward-kill-word)

;ediff
(setq ediff-split-window-function 'split-window-horizontally)

; activate all the packages (in particular autoloads)
(package-initialize)

;autopair
(require 'autopair)
(autopair-global-mode) ;; enable auopair in all buffers

;vendor extensions
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;lisp
(setq inferior-lisp-program "sbcl")
(add-to-list 'load-path "~/quicklisp")
(require 'slime-autoloads)
(slime-setup)

;jade
(add-to-list 'load-path "~/.emacs.d/vendor/jade-mode")
(require 'sws-mode)
(require 'jade-mode)    
(add-to-list 'auto-mode-alist '("\\.styl$" . sws-mode))
(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))

;haskell
(custom-set-variables '(haskell-mode-hook '(turn-on-haskell-doc-mode)))
(custom-set-variables '(haskell-mode-hook '(turn-on-haskell-indent)))

;projectile project management
(projectile-global-mode)


