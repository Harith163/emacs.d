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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;; '(fringe-mode 0 nil (fringe))
 '(package-selected-packages
   (quote
    (kurecolor pretty-symbols modus-vivendi-theme restart-emacs projectile rainbow-mode god-mode all-the-icons-ibuffer all-the-icons-ivy-rich ivy-rich all-the-icons-ivy company linum-relative pretty-mode autothemer dashboard powerline-mode powerline cdlatex auctex TeX AUCTeX LaTeX-mode LaTeX latex expand-region flycheck outline-mode use-package undo-tree rainbow-delimiters org-bullets org magit htmlize diminish auto-complete which-key try outline-magic darkokai-theme counsel ace-window))))

(org-babel-load-file (expand-file-name "~/.emacs.d/initfile.org"))


