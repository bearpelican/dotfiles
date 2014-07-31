;loading default packages
; list the packages you want
(setq package-list '(autopair haskell-mode json-mode magit slime projectile))

; list the repositories containing them
(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
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

;ediff
(setq ediff-split-window-function 'split-window-horizontally)

;autopair
(require 'autopair)
(autopair-global-mode) ;; enable auopair in all buffers

;vendor extensions
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;haskell
(custom-set-variables '(haskell-mode-hook '(turn-on-haskell-doc-mode)))
(custom-set-variables '(haskell-mode-hook '(turn-on-haskell-indent)))

;projectile project management
(projectile-global-mode)


