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
 '(custom-safe-themes
   (quote
    ("84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "855eb24c0ea67e3b64d5d07730b96908bac6f4cd1e5a5986493cbac45e9d9636" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(fringe-mode 0 nil (fringe))
 '(org-agenda-files (quote ("~/Documents/life/README.org")))
 '(package-selected-packages
   (quote
    (restart-emacs projectile rainbow-mode god-mode all-the-icons-ibuffer all-the-icons-ivy-rich ivy-rich all-the-icons-ivy company linum-relative doom-modeline pretty-mode autothemer dashboard smog seti-theme powerline-mode powerline cdlatex auctex TeX AUCTeX LaTeX-mode LaTeX latex expand-region jedi flycheck outline-mode use-package undo-tree rainbow-delimiters org-bullets org magit htmlize diminish auto-complete which-key try outline-magic darkokai-theme counsel ace-window))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :weight normal :height 167 :width normal :foundry "PfEd" :family "DejaVu Sans Mono"))))
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 2.0))))
 '(linum ((t (:inherit (shadow default) :height 0.85)))))

(org-babel-load-file (expand-file-name "~/.emacs.d/initfile.org"))
