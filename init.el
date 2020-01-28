;; DESTROY THE STARTUP SCREEN/BASIC BAR AND STUFF CAUSE IT SUCKS!!!!
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(global-visual-line-mode t)

;; Frame transparency
(add-to-list 'default-frame-alist '(fullscreen . fullboth))
(set-frame-parameter (selected-frame) 'alpha '(95 . 50))
(add-to-list 'default-frame-alist '(alpha . (95 . 50)))

;;ibuffer
(defalias 'list-buffers 'ibuffer-other-window)

;;Change yes, no to y, n and revert buffer hotkey
(fset 'yes-or-no-p 'y-or-n-p)
(global-set-key (kbd "<f5>") 'revert-buffer)

;; Add melpa and initialize
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;;Force installation of use package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;package configurations via use-package

;; linum
(use-package linum
  :ensure t
  :config (global-linum-mode t))

;;lets you try packages
(use-package try
  :ensure t)

;;which key stuff
(use-package which-key
  :ensure t
  :config (which-key-mode))

;;Ace Window for navigation purposes
(use-package ace-window
  :ensure t
  :init
  (progn
    (global-set-key [remap other-window] 'ace-window)
    (custom-set-faces
     '(aw-leading-char-face
       ((t (:inherit ace-jump-face-foreground :height 2.0)))))
    ))

;; it looks like counsel is a requirement for swiper
(use-package counsel
  :ensure t
  )

(use-package swiper
  :ensure try
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (global-set-key "\C-s" 'swiper)
    (global-set-key (kbd "C-c C-r") 'ivy-resume)
    (global-set-key (kbd "<f6>") 'ivy-resume)
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)
    (global-set-key (kbd "<f1> f") 'counsel-describe-function)
    (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
    (global-set-key (kbd "<f1> l") 'counsel-load-library)
    (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
    (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
    (global-set-key (kbd "C-c g") 'counsel-git)
    (global-set-key (kbd "C-c j") 'counsel-git-grep)
    (global-set-key (kbd "C-c k") 'counsel-ag)
    (global-set-key (kbd "C-x l") 'counsel-locate)
    (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
    ))

;; ;;Outline Magic
;; (use-package outline
;;   :ensure t)

;;Rainbow delimiters
(use-package rainbow-delimiters
  :ensure t
  :config
  (progn
    (rainbow-delimiters-mode 1)
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)))


;;AutoComplete stuff
(use-package auto-complete
  :ensure t
  :init
  (progn
    (ac-config-default)
    (global-auto-complete-mode t)
    ))

;;Org-mode stuff
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;;Magit
(use-package magit
  :ensure t)

;; Flycheck
(use-package flycheck
  :ensure t
  :config
  (add-hook 'python-mode-hook 'flycheck-mode))

;;Python mode stuff
(use-package jedi
  :ensure t
  :init
  (add-hook 'python-mode-hook 'jedi:setup)
  (add-hook 'python-mode-hook 'jedi:ac-setup))

;;LaTeX mode stuff
(use-package latex
  :defer t
  :ensure auctex
  :mode ("//.tex//" . latex-mode)
  :config
  (progn
    (setq TeX-fold-mode t)
    (setq TeX-parse-self t)
    (setq TeX-save-query nil)
    (setq TeX-PDF-mode t)
    (add-hook 'LaTeX-mode-hook 'cdlatex-mode)
      ))
    

;;Theme stuff
(use-package darkokai-theme
  :ensure t
  :config (load-theme 'darkokai t))

;;Miscellaneous

;; undo tree
(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode))

;; expand the marked region in semantic increments (negative prefix to reduce region)
(use-package expand-region
:ensure t
:config
(global-set-key (kbd "C-=") 'er/expand-region))

;; Loading an org mode file as default.
(find-file "~/Documents/life/life.org")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (cdlatex auctex TeX AUCTeX LaTeX-mode LaTeX latex expand-region jedi flycheck outline-mode use-package undo-tree rainbow-delimiters org-bullets org magit htmlize diminish auto-complete which-key try outline-magic darkokai-theme counsel ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 2.0)))))


;;(org-babel-load-file (expand-file-name "~/.emacs.d/initfile.org"))
