;loading default packages
; list the packages you want
(setq package-list '(autopair solarized-theme haskell-mode json-mode magit slime projectile))

; list the repositories containing them
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available 
(or (file-exists-p package-user-dir)
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))


; solarized
(load-theme 'solarized-dark t)

;; Enable mouse support
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (global-set-key [mouse-4] '(lambda ()
                              (interactive)
                              (scroll-down 1)))
  (global-set-key [mouse-5] '(lambda ()
                              (interactive)
                              (scroll-up 1)))
  (defun track-mouse (e))
  (setq mouse-sel-mode t)
)

; Disable menu bar
(menu-bar-mode 0)

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

;comment region
(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)))
(global-set-key "\C-c\C-c" 'comment-or-uncomment-region-or-line)

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
(global-set-key (kbd "C-x a r") 'align-regexp)

;tabs
(setq-default tab-width 1)
(setq-default indent-tabs-mode nil)
(setq indent-line-function 'insert-tab)
(setq tab-stop-list (number-sequence 2 400 2))

;ediff
(setq ediff-split-window-function 'split-window-horizontally)

;autopair
(require 'autopair)
(autopair-global-mode) ;; enable auopair in all buffers

;vendor extensions
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;https://www.seas.upenn.edu/~chaoliu/2017/09/01/python-programming-in-emacs/
(defun company-jedi-setup ()
  (add-to-list 'company-backends 'company-jedi))
(add-hook 'python-mode-hook 'company-jedi-setup)
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi-custom-file (expand-file-name "jedi-custom.el" user-emacs-directory))
(when (file-exists-p jedi-custom-file)
  (load jedi-custom-file))

(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args "-i")

;(add-hook 'after-init-hook 'global-flycheck-mode)
;(setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)

;(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)

;(require 'py-autopep8)
;(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)


;haskell
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(haskell-mode-hook (quote (turn-on-haskell-indent)))
 '(safe-local-variable-values (quote ((haskell-process-use-ghci . t) (haskell-indent-spaces . 4)))))

;projectile project management
(projectile-global-mode)


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
