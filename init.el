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
(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))


;; '(fringe-mode 0 nil (fringe))
