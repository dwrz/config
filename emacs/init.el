;;; -*- lexical-binding: t -*-

(defalias 'yes-or-no-p 'y-or-n-p)
(defvar dwrz-current-theme "zenburn")

(setenv "GOPATH" "/home/dwrz/.go/")
(setenv "SHELL" (executable-find "bash"))

(setq auto-save-interval 30
      delete-by-moving-to-trash t
      echo-keystrokes 0.1
      exec-path '("/home/dwrz/.go/bin/"
		  "/home/dwrz/.local/bin/"
		  "/home/dwrz/.node_modules/bin/"
		  "/usr/bin"
		  "/usr/bin/core_perl"
		  "/usr/bin/site_perl"
		  "/usr/bin/vendor_perl"
		  "/usr/local/bin")
      indent-tabs-mode nil
      inhibit-splash-screen t
      inhibit-startup-message t
      initial-major-mode 'org-mode
      initial-scratch-message nil
      next-screen-context-lines 5
      ring-bell-function 'ignore
      scroll-conservatively 10000
      sentence-end-double-space nil
      split-height-threshold nil
      split-width-threshold 160
      use-dialog-box nil
      user-mail-address "dwrz@dwrz.net"
      user-full-name "David Wen Riccardi-Zhu"
      x-stretch-cursor t)

(setq-default fill-column 80
              shell-file-name (getenv "SHELL")
              truncate-lines nil)

;; FUNCTIONS

(defun dwrz-copy-filename ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
		      default-directory (buffer-file-name))))
    (when filename (kill-new filename)
          (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun dwrz-highlight-logs ()
  "Highlight error and warning lines in log files."
  (when (equal "log" (file-name-extension (buffer-file-name)))
    (hi-lock-mode 1)
    (highlight-lines-matching-regexp "ERROR:" 'hi-red-b)
    (highlight-lines-matching-regexp "WARN:" 'hi-yellow-b)
    (highlight-lines-matching-regexp "INFO:" 'hi-blue-b)))

(defun dwrz-open-calendar ()
  "Open a calfw calendar."
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list (cfw:org-create-source "forest green") (cfw:cal-create-source "red"))))

(defun dwrz-find-file-sudo ()
  "Reopen the current file as root, preserving point position."
  (interactive)
  (let ((p (point)))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))
    (goto-char p)))

(defun dwrz-lightswitch-theme ()
  "Switch from dark to light theme, and vice-versa."
  (interactive)
  (cond
   ((string-equal dwrz-current-theme "zenburn") (dwrz-set-light-theme))
   ((string-equal dwrz-current-theme "gruvbox-light") (dwrz-set-dark-theme))))

(defun dwrz-org-capture-at-point ()
  "Insert an org capture template at point."
  (interactive)
  (org-capture 0))

(defun dwrz-remove-bars ()
  "Remove menu, scroll, tool, and window-divider bars."
  (interactive)
  (when (fboundp 'menu-bar-mode)(menu-bar-mode -1))
  (when (fboundp 'scroll-bar-mode)(scroll-bar-mode -1))
  (when (fboundp 'tool-bar-mode)(tool-bar-mode -1))
  (when (fboundp 'window-divider-mode)(window-divider-mode -1)))

(defun dwrz-set-dark-theme ()
  "Apply the dark theme."
  (interactive)
  (load-theme 'zenburn t)
  (set-face-attribute 'fringe t :background "#3F3F3F")
  (disable-theme 'gruvbox-light-hard)
  (dwrz-remove-bars)
  (setq dwrz-current-theme "zenburn"))

(defun dwrz-set-light-theme ()
  "Apply the light theme."
  (interactive)
  (load-theme 'gruvbox-light-hard t)
  (set-face-attribute 'fringe t :background "#F9F5D7")
  (disable-theme 'zenburn)
  (dwrz-remove-bars)
  (setq dwrz-current-theme "gruvbox-light"))

(defun dwrz-unfill-paragraph ()
  "Unfill a paragraph."
  (interactive)
  (let ((fill-column (point-max))) (fill-paragraph nil)))

(defun dwrz-unfill-region ()
  "Unfill a region."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-region (region-beginning) (region-end) nil)))

;; PACKAGES
(package-initialize)

(require 'package)
(setq package-archives
      '(("org" . "https://orgmode.org/elpa/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)

(setq use-package-always-ensure t)

(use-package all-the-icons)

(use-package alert)

(use-package anzu
  :config
  (global-anzu-mode t)
  (global-set-key [remap query-replace] 'anzu-query-replace)
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp))

(use-package auto-compile
  :demand t
  :hook
  (auto-compile-inhibit-compile-hook .
				 auto-compile-inhibit-compile-detached-git-head)
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode)
  (setq auto-compile-display-buffer nil
        auto-compile-mode-line-counter t
        auto-compile-source-recreate-deletes-dest t
        auto-compile-toggle-deletes-nonlib-dest t
        auto-compile-update-autoloads t))

(use-package autorevert
  :config
  (setq auto-revert-verbose nil
	global-auto-revert-non-file-buffers t)
  (global-auto-revert-mode t))

(use-package avy
  :bind
  (("C-s-o" . avy-goto-char)
   ("C-s-a" . avy-goto-line))
  :config
  (setq avy-all-windows 'all-frames
        avy-background t
        avy-case-fold-search nil
        avy-keys '(?a ?o ?e ?u ?h ?t ?n ?s)
        avy-style 'at-full))

(use-package bookmark
  :demand t
  :config (setq bookmark-save-flag 1))

(use-package browse-url
  :custom
  (browse-url-browser-function 'eww-browse-url)
  (browse-url-generic-program "firefox"))

(use-package calc :bind ("H-n" . calc))

(use-package calendar
  :config
  (setq calendar-chinese-all-holidays-flag t
        calendar-week-start-day 1
        diary-file "~/ruck/oo/org/diary.org"
        holiday-bahai-holidays nil))

(use-package calfw)

(use-package calfw-cal :after calfw)

(use-package calfw-org :after org calfw)

(use-package cc-vars
  :ensure nil
  :config
  (setq-default c-basic-offset 8
		tab-width 8
		indent-tabs-mode t))

(use-package company
  :bind
  (:map company-active-map ("<return>" . nil) ( "RET" . nil)
	("<tab>" . company-complete-selection))
  :commands (company-mode company-indent-or-complete-common)
  :config
  (global-company-mode t)
  (setq company-backends
	'((company-yasnippet company-semantic company-clang company-xcode
			     company-cmake  company-capf company-files
			     company-gtags company-etags company-keywords)
	  (company-abbrev company-dabbrev company-dabbrev-code))
  company-begin-commands '(self-insert-command)
	company-idle-delay 0
	company-minimum-prefix-length 2
        company-show-numbers t
	company-tooltip-align-annotations t)
  :custom
  (company-quickhelp-color-foreground "#DCDCCC")
  (company-quickhelp-color-background "#4F4F4F"))

(use-package company-box :hook (company-mode . company-box-mode))

(use-package company-lsp
  :commands company-lsp
  :config
  (add-to-list 'company-backends 'company-lsp))

(use-package company-quickhelp :config (company-quickhelp-mode t))

(use-package compile :config (setq compilation-message-face 'default))

(use-package counsel
  :after ivy
  :demand t
  :bind
  (("C-h v" . counsel-describe-variable)
   ("C-h f" . counsel-describe-function)
   ("<f5> i" . counsel-info-lookup-symbol)
   ("<f5> l" . counsel-find-library)
   ("<f5> u" . counsel-unicode-char)
   ("C-c g" . counsel-git)
   ("C-c j" . counsel-git-grep)
   ("C-c k" . counsel-ag)
   ("C-r" . counsel-rg)
   ("C-s-e" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-x l" . counsel-locate)
   ("M-x" . counsel-M-x))
  :config
  (setq counsel-rg-base-command
	"rg -S -M 120 --no-heading --line-number --color never %s .")
  (setq counsel-find-file-at-point t))

(use-package counsel-tramp :commands counsel-tramp)

(use-package custom
  :ensure nil
  :config
  (setq custom-file "/tmp/custom.el")
  (when (file-exists-p custom-file) (load custom-file)))

(use-package dash :config (eval-after-load "dash" '(dash-enable-font-lock)))

(use-package delsel :config (delete-selection-mode nil))

(use-package dired
  :ensure nil
  :config (put 'dired-find-alternate-file 'disabled nil))

(use-package dired-hide-dotfiles
  :bind (:map dired-mode-map ("." . dired-hide-dotfiles-mode)))

(use-package dired-open
  :config
  (setq dired-open-extensions
        '(("mkv" . "mpv")
          ("mp4" . "mpv")
          ("avi" . "mpv"))))

(use-package dired-x
  :ensure nil
  :after dired
  :config
  (auto-compression-mode t)
  (setq dired-clean-up-buffers-too t
	dired-dwim-target t
	dired-listing-switches "-alh"
	dired-omit-files (concat dired-omit-files "\\|^\\..+$")
	dired-omit-verbose nil
	dired-recursive-copies 'always))

(use-package doc-view :config (setq doc-view-resolution 150))

(use-package dockerfile-mode)

(use-package docker :bind ("C-c d" . docker))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config (column-number-mode t))

(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :hook (prog-mode . dumb-jump-mode)
  :config (setq dumb-jump-force-searcher 'rg))

(use-package elisp-mode
  :ensure nil
  :config
  (add-to-list (make-local-variable 'company-backends) 'company-elisp))

(use-package eldoc :hook (emacs-lisp-mode . eldoc-mode))

(use-package elec-pair :hook (web-mode . electric-pair-mode))

(use-package emmet-mode :hook (css-mode html-mode web-mode))

(use-package emojify
  :bind ("C-c e" . emojify-insert-emoji)
  :config
  (setq emojify-emoji-styles '(unicode))
  (global-emojify-mode t))

(use-package epg-config
  :ensure nil
  :after epg
  :custom (epg-gpg-program "/usr/bin/gpg2"))

(use-package erc
  :commands (erc erc-tls)
  :config
  (setq erc-nick "dwrz")
  (add-to-list 'erc-modules 'notifications)
  (add-to-list 'erc-modules 'spelling))

(use-package eww
  :config (setq eww-search-prefix "https://www.ecosia.org/search/?q="))

(use-package f)

(use-package faces
  :ensure nil
  :defer t
  :custom
  (face-font-family-alternatives '(("hans" "adobe-source-han-sans-cn-font")))
  :init
  (set-face-attribute
   'default t
   :family "DejaVu Sans Mono"
   :foundry "PfEd"
   :slant 'normal
   :weight 'normal
   :height 143
   :width 'normal))

(use-package files
  :ensure nil
  :demand t
  :config
  (setq auto-save-visited-mode t)
  (add-hook 'find-file-hook 'dwrz-highlight-logs)
  (setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
	backup-directory-alist `((".*" .,temporary-file-directory))
	confirm-kill-emacs 'y-or-n-p))

(use-package flycheck :commands flycheck-mode :hook (prog-mode . flycheck-mode))

(use-package flycheck-golangci-lint
  :after go-mode
  :hook
  ((go-mode . flycheck-golangci-lint-setup)
   (go-mode . lsp)))

(use-package flyspell
  :hook
  ((prog-mode . flyspell-prog-mode)
   (text-mode . flyspell-mode)
   (org-mode . flyspell-mode))
  :init
  (setq ispell-program-name "/usr/bin/aspell"
        ispell-dictionary "en_US"
        ispell-extra-args '("--sug-mode=ultra" "--lang=en_US")
        ispell-list-command "--list"
        ispell-dictionary-alist
        '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "['‘’]"
	   t ("-d" "en_US") nil utf-8))))

(use-package git-commit
  :after magit
  :config (setq git-commit-summary-max-length 50))

(use-package gnutls :config (setq gnutls-verify-error t))

(use-package go-mode
  :bind
  (("C-c C-b" . pop-tag-mark)
   ("C-c t" . go-tag-add)
   ("C-c T" . go-tag-remove))
  :config
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'go-mode-hook 'lsp-go-install-save-hooks))

(use-package go-playground
  :config
  (setq go-playground-ask-file-name nil)
  (setq go-playground-basedir "/home/dwrz/.go/src/playground/"))

(use-package go-tag :config (setq go-tag-args (list "-transform" "camelcase")))

(use-package google-translate)

(use-package gruvbox-theme :defer t)

(use-package hippie-exp :bind ("C-s-h" . hippie-expand))

(use-package hl-line :demand t :config (global-hl-line-mode t))

(use-package htmlize
  :commands (htmlize-buffer
             htmlize-file
             htmlize-many-files
             htmlize-many-files-dired
             htmlize-region))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :hook
  (ibuffer-mode . (lambda ()
		    (ibuffer-switch-to-saved-filter-groups "default"))))

(use-package immortal-scratch :config (immortal-scratch-mode t))

(use-package ivy
  :config
  (ivy-mode t)
  (setq ivy-wrap t
	ivy-use-virtual-buffers t
	enable-recursive-minibuffers t
	ivy-count-format "(%d/%d) "
	ivy-re-builders-alist
	'((swiper . ivy--regex-plus) (t . ivy--regex-fuzzy))))

(use-package ivy-pass :bind (("C-s-p" . 'ivy-pass) ("C-x p" . 'ivy-pass)))

(use-package ivy-rich
  :after ivy
  :config
  (ivy-rich-mode t)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (setq ivy-rich-path-style 'abbrev))

(use-package js2-mode
  :mode "\\.js\\'"
  :interpreter "node"
  :hook   (js2-mode-hook . js2-imenu-extras-mode)
  :config
  (setq js-indent-level 2))

(use-package js2-refactor
  :hook (js2-mode . js2-refactor-mode)
  :bind (:map js2-mode-map ("C-k" . js2r-kill))
  :config (js2r-add-keybindings-with-prefix "C-c C-r"))

(use-package keychain-environment :config (keychain-refresh-environment))

(use-package ledger-mode)

(use-package lsp-mode :hook (go-mode . lsp) :commands lsp)

(use-package lsp-ui :disabled t :commands lsp-ui-mode)

(use-package magit :bind ("C-x g" . magit-status))

(use-package markdown-mode
  :commands (markdown-mode)
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))

(use-package message
  :ensure nil
  :config
  (setq message-directory "drafts"
	message-kill-buffer-on-exit t
	message-sendmail-envelope-from 'header
	message-sendmail-f-is-evil nil))

(use-package messages-are-flowing
  :hook
  (message-mode-hook . messages-are-flowing-use-and-mark-hard-newlines))

(use-package mule
  :ensure nil
  :no-require t
  :config
  (setq locale-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8))

(use-package mwheel
  :ensure nil
  :custom (mouse-wheel-scroll-amount '(1 ((shift) .1))))

(use-package notmuch
  :bind ("H-m" . notmuch)
  :config
  (setq notmuch-address-command  'internal
	notmuch-address-internal-completion  '(sent nil)
	notmuch-address-save-filename "~/ruck/social/notmuch-contacts"
	notmuch-address-use-company t
	notmuch-crypto-process-mime t
	notmuch-fcc-dirs "sent"
	notmuch-hello-hide-tags '("killed")
	notmuch-search-oldest-first nil)
  ;; Search tags
  (setq notmuch-saved-searches
        '((:name "inbox" :query "tag:inbox" :key "i")
          (:name "unread" :query "tag:unread" :key "u")
          (:name "new" :query "tag:new" :key "n")
          (:name "sent" :query "tag:sent" :key "e")
          (:name "drafts" :query "tag:draft" :key "d")
          (:name "all mail" :query "*" :key "a")
          (:name "todo" :query "tag:todo" :key "t")))
  ;; Keybindings
  (define-key notmuch-search-mode-map "S"
    (lambda ()
      "mark message as spam"
      (interactive)
      (notmuch-search-tag (list "-new" "-unread" "-inbox" "+spam"))
      (forward-line)))
  (define-key notmuch-show-mode-map "S"
    (lambda ()
      "mark message as spam"
      (interactive)
      (notmuch-show-tag (list "-new" "-unread" "-inbox" "+spam"))))
  (define-key notmuch-search-mode-map "N"
    (lambda ()
      "unmark message as new and unread"
      (interactive)
      (notmuch-search-tag (list "-new" "-unread"))
      (forward-line)))
  (define-key notmuch-show-mode-map "N"
    (lambda ()
      "unmark message as new and unread"
      (interactive)
      (notmuch-show-tag (list "-new" "-unread"))))
  (define-key notmuch-show-mode-map "r" 'notmuch-show-reply)
  (define-key notmuch-show-mode-map "R" 'notmuch-show-reply-sender)
  (define-key notmuch-search-mode-map "r" 'notmuch-search-reply-to-thread)
  (define-key notmuch-search-mode-map "R"
    'notmuch-search-reply-to-thread-sender))

(use-package nov)

(use-package novice :config (setq disabled-command-function nil))


(use-package ob-restclient)

(use-package ob-translate)

(use-package ol
  :ensure nil
  :after org
  :config
  (setq org-link-frame-setup
        '((vm . vm-visit-folder-other-frame)
          (vm-imap . vm-visit-imap-folder-other-frame)
          (gnus . org-gnus-no-new-news)
          (file . find-file)
          (wl . wl-other-frame))))

(use-package org
  :bind
  (("C-c a" . org-agenda)
   ("C-c b" . org-iswitchb)
   ("C-c c" . dwrz-org-capture-at-point)
   ("C-c l" . org-store-link)
   ("s-j" . org-goto))
  :mode ("\\.org\\'" . org-mode)
  :ensure org-plus-contrib
  :config
  (add-hook 'org-babel-after-execute-hook
            (lambda () (when org-inline-image-overlays
			 (org-redisplay-inline-images))))
  (add-to-list 'org-src-lang-modes '("js" . js2))
  (add-to-list 'org-modules 'org-habit)
  (add-to-list (make-local-variable 'company-backends) 'company-ispell)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((awk .t ) (calc . t) (C . t) (emacs-lisp . t) (gnuplot . t) (js . t)
     (latex . t) (ledger . t) (makefile .t )(org . t) (python . t)
     (shell . t) (sed .t) (sql . t) (sqlite . t)))

  (setq org-adapt-indentation nil
	org-export-backends '(ascii html icalendar latex md odt)
        org-catch-invisible-edits 'show
	org-fontify-done-headline t
        org-default-priority 49
        org-enforce-todo-dependencies t
        org-hide-emphasis-markers t
        org-highest-priority 49
        org-html-doctype "html5"
        org-html-html5-fancy t
        org-html-postamble nil
        org-image-actual-width '(800)
	org-list-demote-modify-bullet nil
        org-log-into-drawer t
        org-lowest-priority 53
        org-refile-targets '((nil :maxlevel . 8))
        org-src-fontify-natively t
	org-tags-column 0
	org-todo-keywords '((sequence "QUEUED(q)"
				      "IN-PROGRESS(i)" "RECURRING(r)"
				      "WAITING(w)" "SOMEDAY-MAYBE(s)" "|"
				      "DONE(d)" "DELEGATED(e)" "CANCELED(c)")
			    (sequence "AR(a)" "GOAL(g)"))))

(use-package org-agenda
  :ensure nil
  :after org
  :config
  (cond ((string-equal (system-name) "earth")
	 (setq org-agenda-files '("~/ruck/oo/org/dwrz.org")
	       org-archive-location
	       "/home/dwrz/ruck/oo/org/dwrz-org-archive.org::"))
	((string-equal (system-name) "gu-dwrz")
	 (setq org-agenda-files '("~/gu/org-dwrz/gu.org")
	       org-archive-location
	       "~/gu/org-dwrz/archives/gu-archive.org::")))
  (setq org-agenda-follow-indirect nil
	org-agenda-include-diary t
	org-agenda-prefix-format '((agenda . " %i %?-12t% s")
				   (timeline . "  % s")
				   (todo . " %i")
				   (tags . " %i")
				   (search . " %i"))
	org-agenda-tags-column 'auto))

(use-package org-capture
  :ensure nil
  :config
  (setq org-capture-templates
        '(("b" "bookmark" entry
           (file "")
           (file "~/ruck/oo/org/templates/bookmark.org")
           :jump-to-captured t
           :empty-lines-before 1
           :empty-lines-after 1)
          ("c" "contact" entry
           (file "")
           (file "~/ruck/oo/org/templates/contact.org")
           :jump-to-captured t
           :empty-lines-before 1
           :empty-lines-after 1)
          ("g" "goal" entry
           (file "")
           (file "~/ruck/oo/org/templates/goal.org")
           :prepend t
           :jump-to-captured t
           :empty-lines-before 1
           :empty-lines-after 1)
          ("e" "log entry" plain
           (file "")
           (file "~/ruck/oo/org/templates/log-entry.org")
           :jump-to-captured t)
          ("j" "journal" entry
           (file "")
           (file "~/ruck/oo/org/templates/journal.org")
           :prepend t
           :jump-to-captured t
           :empty-lines-before 1
           :empty-lines-after 1)
          ("l" "log" entry
           (file "")
           (file "~/ruck/oo/org/templates/log.org")
           :prepend t
           :jump-to-captured t
           :empty-lines-after 1)
          ("n" "note" entry
           (file "")
           (file "~/ruck/oo/org/templates/note.org")
           :prepend t
           :jump-to-captured t
           :empty-lines-before 1
           :empty-lines-after 1)
          ("r" "review" entry
           (file "")
           (file "~/ruck/oo/org/templates/review.org")
           :prepend t
           :jump-to-captured t
           :empty-lines-before 1
           :empty-lines-after 1))))

(use-package org-clock
  :ensure nil
  :after org
  :config (setq org-clock-into-drawer "CLOCKING"))

(use-package org-faces
  :ensure nil
  :after org
  :config
  (setq org-priority-faces
        '((?1 . (:foreground "red" :weight 'bold))
          (?2 . (:foreground "orange"))
          (?3 . (:foreground "yellow"))
          (?4 . (:foreground "green"))
          (?5 . (:foreground "purple"))))
  (setq org-todo-keyword-faces
	'(("QUEUED" . "red")
	  ("IN-PROGRESS" . "limegreen")
	  ("RECURRING" . "orange")
	  ("WAITING" . "yellow")
	  ("DONE" . "blue")
	  ("DELEGATED" . "gray50")
	  ("CANCELED" . "purple")
	  ("SOMEDAY-MAYBE" . "orchid")
	  ("AR" . "red")
	  ("GOAL" . "springgreen"))))

(use-package org-goto
  :ensure nil
  :after org :config (setq org-goto-max-level 8))

(use-package org-eww :ensure nil :after org eww)

(use-package ol-notmuch :ensure nil :after org notmuch)

(use-package org-src
  :ensure nil
  :after org
  :config
  (setq org-src-preserve-indentation t
        org-src-tab-acts-natively t))

(use-package pandoc-mode)

(use-package paren :demand t :custom (show-paren-mode t))

(use-package pdf-tools
  :config (setq pdf-view-midnight-colors '("#DCDCCC" . "#383838")))

(use-package plantuml-mode
  :config (setq plantuml-jar-path "/usr/bin/plantuml"))

(use-package pos-tip
  :config
  (setq pos-tip-background-color "#36473A"
	pos-tip-foreground-color "#FFFFC8"))

(use-package prog-mode
  :ensure nil
  :config
  (progn (font-lock-add-keywords
             nil '(("\\<\\(FIX\\|TODO\\|BUG\\):" 1
                    font-lock-warning-face t)))))

(use-package pyim :demand t :requires pyim-basedict)

(use-package pyim-basedict :config (pyim-basedict-enable))

(use-package rainbow-mode :pin gnu :hook (prog-mode text-mode))

(use-package rainbow-delimiters :hook (prog-mode . rainbow-delimiters-mode))

(use-package register
  :ensure nil
  :bind
  (("C-s-j" . jump-to-register)
   ("C-x j" . jump-to-register))
  :config
  (set-register ?9 '(file . "~/ruck/oo/journal/2019.org"))
  (set-register ?j '(file . "~/ruck/oo/journal/2020.org"))
  (set-register ?g '(file . "~/ruck/oo/org/gtd.org"))
  (set-register ?m '(file . "~/ruck/oo/org/mindsweep-trigger-list.org"))
  (set-register ?o '(file . "~/ruck/oo/org/dwrz.org")))

(use-package restclient :mode ("\\.restclient\\'" . restclient-mode))

(use-package rmsbolt
  :config
  (setq rmsbolt-command
	"gcc -O3 -Wall -Wstrict-prototypes -std=c17 -pedantic"))

(use-package s)

(use-package saveplace
  :init
  (setq-default save-place t)
  (setq save-place-forget-unreadable-files nil))

(use-package sendmail
  :config
  (setq mail-specify-envelope-from t
	mail-envelope-from 'header
	mail-specify-envelope-from t
	send-mail-function 'sendmail-send-it
	sendmail-program "~/.msmtpqueue/msmtp-enqueue.sh"))

(use-package shell-pop
  :bind (("C-s-t" . shell-pop)
         ("C-x t" . shell-pop))
  :custom
  (shell-pop-shell-type '("vterm" "*vterm*" (lambda nil (vterm))))
  :config
  (setq shell-pop-window-size 25
        shell-pop-full-span nil
        shell-pop-universal-key "C-s-t"
        shell-pop-window-position "bottom"
        shell-pop-in-after-hook 'end-of-buffer))

(use-package sh-script :config (setq sh-basic-offset 2))

(use-package simple
  :ensure nil
  :demand t
  :hook ((prog-mode text-mode) . visual-line-mode)
  :config
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (column-number-mode)
  (setq async-shell-command-buffer "new-buffer"
	backward-delete-char-untabify-method nil
	mail-user-agent 'message-user-agent
	shift-select-mode nil)
  (size-indication-mode))

(use-package smartparens :hook (emacs-lisp-mode . smartparens-mode))

(use-package sort-words)

(use-package subword :config (global-subword-mode t))

(use-package super-save
  :config
  (super-save-mode t)
  (setq super-save-auto-save-when-idle t))

(use-package swiper :bind ("C-s" . swiper))

(use-package systemd)

(use-package term
  :config (setq-default explicit-shell-file-name (getenv "SHELL")))

(use-package text-mode
  :ensure nil
  :config
  (add-to-list (make-local-variable 'company-backends) 'company-ispell))

(use-package time :config (setq display-time-mode nil))

(use-package tls :config (setq tls-checktrust t))

(use-package tramp
  :config (setq password-cache-expiry 3600 tramp-default-method "ssh"))

(use-package visual-fill-column
  :hook (visual-line-mode . visual-fill-column-mode)
  :config
  (setq split-window-preferred-function
	'visual-fill-column-split-window-sensibly))

(use-package volatile-highlights :config (volatile-highlights-mode t))

(use-package vterm :config (setq vterm-max-scrollback 32767))

(use-package web-mode
  :mode "\\.html\\'" "\\.gohtml\\'"
  :config
  (add-to-list (make-local-variable 'company-backends) 'company-web-html)
  (setq web-mode-code-indent-offset 2
	web-mode-css-indent-offset 2
	web-mode-indent-style 1
	web-mode-markup-indent-offset 2))

(use-package wgrep)

(use-package which-key :config (setq which-key-mode t))

(use-package yaml-mode)

(use-package yasnippet
  :demand t
  :bind (("C-s-s" . yas-expand)
	 (:map yas-keymap ("<tab>" . nil)))
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :config
  (yas-load-directory "/home/dwrz/.emacs.d/snippets")
  (yas-global-mode t))

(use-package zenburn-theme :defer t :hook (after-init . dwrz-set-dark-theme))

;; HYDRAS
(defhydra hydra-highlight (:color blue)
  "
^highlight^         ^do^                ^undo^
^─────────^─────────^──^────────────────^──────^────────────
_q_ quit            _s_ symbol          _u_ unhiglight
^^                  _l_ lines           ^^
^^                  _p_ phrase          ^^
^^                  _r_ regex           ^^
^^                  ^^                  ^^
"
  ("q" nil)
  ("s" highlight-symbol-at-point :color blue)
  ("l" highlight-lines-matching-regexp :color blue)
  ("p" highlight-phrase :color blue)
  ("r" highlight-regexp :color blue)
  ("u" unhighlight-regexp :color blue))

(defhydra hydra-lsp (:exit t :hint nil)
  "
 Buffer^^               Server^^                   Symbol
-------------------------------------------------------------------------------------
 [_f_] format           [_M-r_] restart            [_d_] declaration  [_i_] implementation  [_o_] documentation
 [_m_] imenu            [_S_]   shutdown           [_D_] definition   [_t_] type            [_r_] rename
 [_x_] execute action   [_M-s_] describe session   [_R_] references   [_s_] signature"
  ("D" lsp-ui-peek-find-definitions)
  ("M-r" lsp-workspace-restart)
  ("M-s" lsp-describe-session)
  ("R" lsp-ui-peek-find-references)
  ("S" lsp-workspace-shutdown)
  ("d" lsp-find-declaration)
  ("f" lsp-format-buffer)
  ("i" lsp-ui-peek-find-implementation)
  ("m" lsp-ui-imenu)
  ("o" lsp-describe-thing-at-point)
  ("r" lsp-rename)
  ("s" lsp-signature-help)
  ("t" lsp-find-type-definition)
  ("x" lsp-execute-code-action))

(defhydra hydra-point (:color blue)
  "
^point^       ^format^       ^insert^       ^act^
^───────^─────^──────^───────^──────^───────^────^───────────
_q_ quit        ^^             _t_ timestamp  _k_ delete word
^^              ^^             _d_ date       _s_ spell
^^              ^^             ^^             ^^
^^              ^^             ^^             ^^
"
  ("d" (insert (format-time-string "%Y%m%d")))
  ("k" kill-word)
  ("q" nil)
  ("s" ispell-word)
  ("t" (insert (format-time-string "%FT%T%z"))))

(defhydra hydra-region (:color red)
  "
^region^       ^format^       ^edit^           ^fill^
^───────^───────^─────^───────^────^───────────^────^───────────
_q_ quit        _d_ downcase  _o_ (un)commen   _U_ unfill-region
^^              _u_ upcase    _r_ reverse      _f_ fill-region
^^              _c_ cipher    _s_ sort         ^^
^^              ^^            ^^               ^^
"
  ("U" dwrz-unfill-region)
  ("c" rot13-region)
  ("d" downcase-region)
  ("f" fill-region)
  ("o" comment-or-uncomment-region)
  ("r" reverse-region)
  ("s" sort-lines)
  ("u" upcase-region)
  ("q" nil))

(defhydra hydra-windows (:color pink)
  "
^Windows^           ^Window^            ^Zoom^
^───────^───────────^──────^────────────^────^──────────────
_q_ quit            _b_ balance         _-_ out
^^                  _t_ taller          _+_ in
^^                  _n_ narrow          _=_ reset
^^                  _l_ lower           ^^
^^                  _w_ widen           ^^
^^                  ^^                  ^^
"
  ("q" nil)
  ("b" balance-windows)
  ("t" enlarge-window)
  ("n" shrink-window-horizontally)
  ("l" shrink-window)
  ("w" enlarge-window-horizontally)
  ("-" text-scale-decrease)
  ("+" text-scale-increase)
  ("=" (text-scale-increase 0)))

(defhydra hydra-meta-hydra (:color blue)
  "
^meta^      ^^              ^^
^───────^───^^──────────────^───────────
_q_ quit    _h_ highlight   _p_ point
^^          _l_ lsp         _r_ region
^^          ^^              _t_ theme
^^          ^^              _w_ windows
^^          ^^              ^^
^^          ^^              ^^
"
  ("q" nil)
  ("h" hydra-highlight/body)
  ("l" hydra-lsp/body)
  ("p" hydra-point/body)
  ("r" hydra-region/body)
  ("t" dwrz-lightswitch-theme)
  ("w" hydra-windows/body))

;; KEYBINDINGS

(global-set-key (kbd "C-s-c") 'async-shell-command)
(global-set-key (kbd "C-s-l") 'display-line-numbers-mode)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "H-c") 'dwrz-open-calendar)
(global-set-key (kbd "H-s") 'ispell-buffer)
(global-set-key (kbd "s-/") 'comment-line)
(global-set-key (kbd "s-b") 'bookmark-jump)
(global-set-key (kbd "s-d") 'ispell-word)
(global-set-key (kbd "s-h") 'hydra-highlight/body)
(global-set-key (kbd "s-m") 'hydra-meta-hydra/body)
(global-set-key (kbd "s-p") 'hydra-point/body)
(global-set-key (kbd "s-o") 'hydra-region/body)
(global-set-key (kbd "s-w") 'hydra-windows/body)
