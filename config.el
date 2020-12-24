(setq user-full-name "Skye Srinivasan Harith")
(setq user-mail-address "kaushik.harith@gmail.com")

(setq inhibit-startup-message t) ;startup.el
(tool-bar-mode -1) ;tool-bar.el
(menu-bar-mode -1) ;menu-bar.el
(set-scroll-bar-mode nil) ;sroll-bar.el
(global-visual-line-mode t) ;simple.el
(set-fringe-mode 0) ;fringe.el
(tab-bar-mode 0) ;tab-bar.el
(global-tab-line-mode 0) ;tab-line.el

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'trans-side t)

(add-to-list 'default-frame-alist '(fullscreen . fullboth))

(use-package telephone-line
  :custom 
  (telephone-line-primary-left-separator 'telephone-line-cubed-left)
  (telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left)
  (telephone-line-primary-right-separator 'telephone-line-cubed-right)
  (telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right)
  (telephone-line-height 24)

  :config
  (defface pinkblue '((t (:foreground "#FF619D" :background "#000038"))) "")
  (defface greenblack '((t (:foreground "#00ff68" :background "#000028"))) "")

  (setq telephone-line-faces
	'((whiteblack . (pinkblue . pinkblue))
	  (greenblack . (greenblack . greenblack))
	  (accent . (telephone-line-accent-active . telephone-line-accent-inactive))
	  (nil . (mode-line . mode-line-inactive))))



  (setq telephone-line-lhs '((accent . (telephone-line-vc-segment
					telephone-line-erc-modified-channels-segment
					telephone-line-process-segment))
			     (nil . (telephone-line-airline-position-segment))
			     (greenblack .(telephone-line-buffer-name-segment))
			     (whiteblack . (telephone-line-major-mode-segment
					    telephone-line-minor-mode-segment))))

  (setq telephone-line-center-lhs nil)
  (setq telephone-line-center-rhs nil)
  (setq telephone-line-rhs nil)

  (telephone-line-mode t))

(fset 'yes-or-no-p 'y-or-n-p)

(setq backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist '(("." . "~/.emacs.d/saves/"))    ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)       ; use versioned backups

(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/saves/" t)))

(setq split-height-threshold nil) ;window.el
(setq split-width-threshold 0) ;window.el

(use-package window
  :init
  (setq display-buffer-alist
	'(;; top side window
	  ("\\*\\(Flycheck\\|Flymake\\|Package-Lint\\|vc-git :\\).*" ;; This bit is useless to me currently. Rethink it later. 
	   (display-buffer-in-side-window)
	   (window-height . 0.25)
	   (side . top)
	   (slot . 0)
	   (window-parameters . ((no-other-window . t))))
	  ("\\*Messages.*"
	   (display-buffer-in-side-window)
	   (window-height . 0.25)
	   (side . top)
	   (slot . 1)
	   (window-parameters . ((no-other-window . t))))
	  ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\)\\*"
	   (display-buffer-in-side-window)
	   (window-height . 0.25)
	   (side . top)
	   (slot . 2)
	   (window-parameters . ((no-other-window . t))))
	  ;; bottom side window
	  ("\\*\\(Output\\|Register Preview\\).*"
	   (display-buffer-in-side-window)
	   (window-width . 0.20)       ; See the :hook
	   (side . bottom)
	   (slot . -1)
	   (window-parameters . ((no-other-window . t))))
	  (".*\\*\\(Completions\\|Embark.*Occur\\).*"
	   (display-buffer-in-side-window)
	   (window-height . 0.25)
	   (side . bottom)
	   (slot . 0)
	   (window-parameters . ((no-other-window . t))))
	  ("^\\(\\*e?shell\\|vterm\\).*" ;; You don't use eshell. get rid of it
	   (display-buffer-in-side-window)
	   (window-width . 0.40)
	   (side . right)
	   (slot . 1))
	  ;; left side window
	  ("\\*Help.*"
	   (display-buffer-in-side-window)
	   (window-width . 0.25)       ; See the :hook
	   (side . left)
	   (slot . 0)
	   (window-parameters . ((no-other-window . t))))
	  ;; right side window
	  ("\\*Faces\\*"
	   (display-buffer-in-side-window)
	   (window-width . 0.25)
	   (side . right)
	   (slot . 0)
	   (window-parameters . ((no-other-window . t)
				 (mode-line-format . (" "
						      mode-line-buffer-identification)))))
	  ("\\*Custom.*"
	   (display-buffer-in-side-window)
	   (window-width . 0.25)
	   (side . right)
	   (slot . 1))
	  ;; bottom buffer (NOT side window)
	  ("\\*\\vc-\\(incoming\\|outgoing\\).*"
	   (display-buffer-at-bottom))))
  (setq window-combination-resize t)
  (setq even-window-sizes 'height-only)
  (setq window-sides-vertical nil)
  ;; Note that the the syntax for `use-package' hooks is controlled by
  ;; the `use-package-hook-name-suffix' variable.  The "-hook" suffix is
  ;; not an error of mine.
  :hook ((help-mode . visual-line-mode)
	 (custom-mode . visual-line-mode))
  :bind (("s-n" . next-buffer)
	 ("s-p" . previous-buffer)
	 ("s-o" . other-window)
	 ("s-3" . bufler-list)
	 ("s-0" . delete-window)
	 ("s-1" . delete-other-windows)
	 ("s-5" . delete-frame)
	 ("C-x +" . balance-windows-area)))

(setq auto-window-vscroll nil)

(setq custom-file (concat user-emacs-directory "custom.el"))

(load-file custom-file)

(defun contrib/toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	 (next-win-buffer (window-buffer (next-window)))
	 (this-win-edges (window-edges (selected-window)))
	 (next-win-edges (window-edges (next-window)))
	 (this-win-2nd (not (and (<= (car this-win-edges)
		     (car next-win-edges))
		     (<= (cadr this-win-edges)
		     (cadr next-win-edges)))))
	 (splitter
	  (if (= (car this-win-edges)
	     (car (window-edges (next-window))))
	  'split-window-horizontally
	'split-window-vertically)))
    (delete-other-windows)
    (let ((first-win (selected-window)))
      (funcall splitter)
      (if this-win-2nd (other-window 1))
      (set-window-buffer (selected-window) this-win-buffer)
      (set-window-buffer (next-window) next-win-buffer)
      (select-window first-win)
      (if this-win-2nd (other-window 1))))))

(defun contrib/keyboard-quit-context+ ()
  "Quit current context.

This function is a combination of `keyboard-quit' and
`keyboard-escape-quit' with some parts omitted and some custom
behavior added."
  (interactive)
  (cond ((region-active-p)
	 ;; Avoid adding the region to the window selection.
	 (setq saved-region-selection nil)
	 (let (select-active-regions)
	   (deactivate-mark)))
	((eq last-command 'mode-exited) nil)
	(current-prefix-arg
	 nil)
	(defining-kbd-macro
	  (message
	   (substitute-command-keys
	    "Quit is ignored during macro defintion, use \\[kmacro-end-macro] if you want to stop macro definition"))
	  (cancel-kbd-macro-events))
	((active-minibuffer-window)
	 (when (get-buffer-window "*Completions*")
	   ;; hide completions first so point stays in active window when
	   ;; outside the minibuffer
	   (minibuffer-hide-completions))
	 (abort-recursive-edit))
	(t
	 (when completion-in-region-mode
	   (completion-in-region-mode -1))
	 (let ((debug-on-quit nil))
	   (signal 'quit nil)))))

(global-set-key [remap keyboard-quit] #'contrib/keyboard-quit-context+)

(defun contrib/inhibit-global-linum-mode ()
  "Counter-act `global-linum-mode'."
  (add-hook 'after-change-major-mode-hook (lambda () (linum-mode 0)) :append :local))

(defun skye/kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(defun skye/small-text-scale ()
  "Make the buffer text zoom smaller"
  (interactive)
  (text-scale-set -1))

(defun skye/big-text-scale ()
  "Make the buffer text zoom bigger"
  (interactive)
  (text-scale-set 1))

(defun skye/quit-window ()
  "If more than one window is open, close window on quit"
  (interactive)
  (if (> (length (window-list)) 1) (delete-window) (quit-window)))

(use-package emacs
  :bind
  (:map global-map
   :prefix-map my-ctrl-z-prefix-map
   :prefix "C-z"
   ("C-<SPC>" . fixup-whitespace)
   ("C-e" . eval-defun)
   ("|" . contrib/toggle-window-split)
   (";" . comment-region)
   ("C-h f" . describe-face))

  (:map global-map
   :prefix-map my-meta-z-prefix-map
   :prefix "M-z"
   (";" . uncomment-region))

  (:map global-map
   ("<f5>" . revert-buffer)
   ([remap kill-buffer] . skye/kill-current-buffer)
   ("s-s" . save-buffer)
   ("s-m" . mu4e))
  )

(use-package which-key
  :diminish ""
  :ensure t
  :config (which-key-mode))

(use-package try
  :ensure t)

(use-package linum-relative
  :ensure t
  :init
  (global-linum-mode t)
  :config
  (linum-relative-mode)
  (add-hook 'doc-view-mode-hook 'contrib/inhibit-global-linum-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode)
  (Latex-mode . rainbow-delimiters-mode)
  :config
  (rainbow-delimiters-mode 1))

(use-package rainbow-mode
  :ensure t)

(use-package diminish
  :ensure t)

(use-package undo-tree
  :ensure t
  :diminish ""
  :init
  (global-undo-tree-mode))

(use-package ace-window
  :ensure t
  :bind
  ([remap other-window] . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  )

(use-package avy
  :ensure t
  :bind
  (:map global-map
  ("C-;" . avy-goto-char)
  ("C-'" . avy-goto-char-2)
  ("M-g l" . avy-goto-line)
  ("M-g r" . avy-resume)
  :map org-mode-map
  ("C-'" . avy-goto-char-2))
  )

(use-package all-the-icons
  :ensure t
  :diminish "")

(use-package all-the-icons-dired
  :ensure t
  :diminish "")

(all-the-icons-ivy-setup)
(all-the-icons-ivy-rich-mode 1)

(use-package expand-region
  :ensure t
  :after (org)
  :bind
  (:map global-map
	("C-=" . er/expand-region)))

(use-package counsel
  :ensure t)

(use-package swiper
  :ensure t)

(use-package ivy
  :demand
  :diminish ""
  :ensure t
  :bind
  (:map global-map
	("C-s" . swiper)
	("C-r" . swiper-backward)
	("C-c C-r" . ivy-resume)
	("<f6>" . ivy-resume)
	("M-x" . counsel-M-x)
	("C-x C-f" . counsel-find-file)
	("s-f" . counsel-find-file)	
	("<f1> f" . counsel-describe-function)
	("<f1> v" . counsel-describe-variable)
	("<f1> l" . counsel-load-library)
	("<f2> i" . counsel-info-lookup-symbol)
	("<f2> u" . counsel-unicode-char)
	("C-c g" . counsel-git)
	("C-c j" . counsel-git-grep)
	("C-c k" . counsel-ag)
	("C-x l" . counsel-locate)
	("M-y" . counsel-yank-pop))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq swiper-use-visual-line nil)
  (setq swiper-use-visual-line-p (lambda (a) nil)))

(ivy-rich-mode 1)
(setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)

(use-package magit
  :ensure t)

(use-package company
  :diminish ""
  :ensure t
  :init
  (setq company-require-match nil) ; Don't require match, so you can still move your cursor as expected.
  (setq company-tooltip-align-annotations t) ; Align annotation to the right side.
  (setq company-eclim-auto-save nil) ; Stop eclim auto save.
  (setq company-dabbrev-downcase nil) ; No downcase when completion.

  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 4)
  (add-hook 'prog-mode-hook 'company-mode)

  (defun jcs--company-complete-selection--advice-around (fn)
    "Advice execute around `company-complete-selection' command."
    (let ((company-dabbrev-downcase t)) (call-interactively fn))) (advice-add 'company-complete-selection :around #'jcs--company-complete-selection--advice-around))

(use-package company-fuzzy
  :ensure t
  :after (company)
  :diminish ""
  :config
  (global-company-fuzzy-mode 1)
  (setq company-fuzzy-prefix-ontop t)
  (setq company-fuzzy-sorting-backend 'alphabetic)
  (setq company-fuzzy-show-annotation t))

(use-package company-auctex
  :ensure t
  :init
  (company-auctex-init))

(use-package restart-emacs
  :ensure t
  :config
  (setq restart-emacs-restore-frames t))

(use-package bufler
  :ensure t
  :bind
  (("C-x C-b" . bufler)
   ("s-b" . bufler))
  :config
  (bufler-tabs-mode 0))

(use-package dired
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches "-al --group-directories-first --time-style=iso")
  (setq dired-dwim-target t)
  :hook
  ((dired-mode . dired-hide-details-mode)
   (dired-mode . all-the-icons-dired-mode))
  :bind
  (:map dired-mode-map
	("q" . skye/quit-window)))

(use-package lorem-ipsum
  :ensure t)

(use-package prescient
  :ensure
  :config
  (setq prescient-history-length 200)
  (setq prescient-save-file "~/.emacs.d/prescient-items")
  (setq prescient-filter-method '(literal regexp))
  (prescient-persist-mode 1))

(use-package ivy-prescient
  :ensure
  :after (prescient ivy)
  :config
  (setq ivy-prescient-sort-commands
	'(:not counsel-grep
	       counsel-rg
	       counsel-switch-buffer
	       ivy-switch-buffer
	       swiper
	       swiper-multi))
  (setq ivy-prescient-retain-classic-highlighting t)
  (setq ivy-prescient-enable-filtering nil)
  (setq ivy-prescient-enable-sorting t)
  (ivy-prescient-mode 1))

(use-package org
  :ensure org-superstar
  :hook
  (org-mode . org-cdlatex-mode)
  (org-mode . (lambda () (org-superstar-mode)))
  :init
  (setq org-highlight-latex-and-related '(native latex script))
  (setq org-export-backends '(ascii html icalendar latex odt org))
  :bind
  (:map org-mode-map
   ("C-c C-x C-e" . skye/org-mark-and-archive)
   ("C-c C-x <up>" . org-cycle-list-bullet)
   :map global-map
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture))
  :config
  (require 'org-tempo)

  (setq org-directory "~/Documents/life/")

  (defun skye/org-get-path (stringname)
    "Use concat to generate full path."
    (concat (file-name-as-directory org-directory) stringname))

  (setq skye/Readme (skye/org-get-path "README.org"))
  (setq skye/Ideas (skye/org-get-path "Ideas.org"))
  (setq skye/School (skye/org-get-path "SchoolWork.org"))
  (setq skye/archive (skye/org-get-path "archive.org"))
  (setq skye/calendar-personal (skye/org-get-path "calendar-personal.org"))
  (setq skye/calendar-stony (skye/org-get-path "calendar-stony.org"))

  (setq org-agenda-files (list skye/Readme skye/Ideas skye/School))
  (setq org-archive-location (concat skye/archive "::* From %s"))

  (setq org-ellipsis " ▼")
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)

  (setq org-todo-keywords '((sequence "☛TODO(t)" "|" "⚑WAITING(w!)") (sequence "|" "❌CANCELED(c)" "|" "✔DONE(d)")))

  (setq org-enforce-todo-dependencies t)
  (setq org-enforce-todo-checkbox-dependencies t)

  (setq skye/bullets-list '("◉" "●" "○" "⊙"))

  (setq org-src-window-setup 'current-window)

  (defun skye/org-mark-and-archive ()
    "Mark the state of the current subtree as either DONE or CANCELLED and export to my archive.org file"
    (interactive)
    (ivy-read "Choose a final TODO state:" '("✔DONE" "❌CANCELED")
	      :action '(1
			("o" org-todo "action 1")
			("j" org-todo "action 2")))
    (org-archive-subtree))

  (add-to-list 'org-modules 'org-habit)

  (setq org-habit-graph-column 80)
  (setq org-habit-show-habits-only-for-today t)
  (setq org-habit-show-all-today t)

  (setq org-default-notes-file skye/Readme)

  (setq org-capture-templates '(
				("e" "Email to be dealt with. Action item" entry
				 (file+headline skye/Readme "Emails to deal with")
				 "* ☛TODO %:from %? \n %a \n SCHEDULED: %^t DEADLINE: %^t \n :PROPERTIES: \n CREATED: %u \n :END:"
				 )

				("m" "Miscellaneous TODO. Refile" entry
				 (file+headline skye/Readme "Miscellaneous")
				 "* ☛TODO %^{PROMPT} %? \n SCHEDULED: %^t DEADLINE: %^u"
				 )

				("t" "Date-less TODO. Generic" entry
				 (file skye/Readme)
				 "* ☛TODO %^{PROMPT} \n  %?"
				 )
				))

  (setq org-refile-targets
	'((nil :maxlevel . 3)
	  (org-agenda-files :maxlevel . 2)))

  (setq org-pretty-entities nil)
  (setq org-preview-latex-default-process 'dvisvgm)
  )

(use-package org-superstar
  :ensure t
  :after
  (org)
  :config
  (setq org-superstar-leading-bullet ?\s)
  (setq org-superstar-cycle-headline-bullets t)
  (setq org-superstar-headline-bullets-list skye/bullets-list))

(use-package latex
  :defer t
  :ensure auctex
  :mode ("//.tex//" . latex-mode)
  :hook
  (LaTeX-mode . outline-minor-mode)
  :config
  (progn
    (setq TeX-fold-mode t)
    (setq TeX-parse-self t)
    (setq TeX-save-query nil)
    (setq TeX-PDF-mode t)
    (add-hook 'LaTeX-mode-hook 'cdlatex-mode)
    ))

(setenv "PATH" (concat "/opt/texlive/2020/bin/x86_64-linux:"
			 (getenv "PATH")))
(add-to-list 'exec-path "/opt/texlive/2020/bin/x86_64-linux")

;; (load "preview-latex.el" nil t t)

(load "~/.emacs.d/mu4e-init.el")

(use-package mu4e-alert
  :ensure t
  :config
  (mu4e-alert-set-default-style 'libnotify)
  (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
  (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display))

(require 'org-mu4e)

(use-package org-msg
  :ensure t
  :config
  (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t"
	org-msg-startup "hidestars indent inlineimages"
	org-msg-greeting-fmt "\nHi *%s*,\n\n"
	org-msg-greeting-name-limit 3
	org-msg-text-plain-alternative t
	org-msg-signature "
Regards,
*Kaushik S Harith* ")
  (org-msg-mode)
)

(put 'scroll-left 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region 'disabled nil)
