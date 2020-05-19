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

;;Use org-babel to parse the init file. Something about tangling?? Must look into it.
(org-babel-load-file (expand-file-name "~/.emacs.d/initfile.org"))


;; '(fringe-mode 0 nil (fringe))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (ibuffer-git ibuffer-tramp ibuffer-vc pdf-tools which-key use-package undo-tree try restart-emacs rainbow-mode rainbow-delimiters projectile pretty-symbols pretty-mode powerline outline-magic org-bullets modus-vivendi-theme magit linum-relative kurecolor god-mode flycheck expand-region diminish dashboard darkokai-theme counsel company cdlatex autothemer auto-complete auctex all-the-icons-ivy-rich all-the-icons-ivy all-the-icons-ibuffer ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
