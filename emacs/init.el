;;; -*- lexical-binding: t -*-
(defalias 'yes-or-no-p 'y-or-n-p)

(prefer-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(setenv "GOPATH" "/home/dwrz/.go/")
(setenv "SHELL" (executable-find "bash"))

(setq auto-save-interval 30
      compilation-message-face 'default
      custom-file "/tmp/custom.el"
      disabled-command-function nil
      display-time-mode nil
      delete-by-moving-to-trash t
      echo-keystrokes 0.1
      exec-path '("/home/dwrz/.cargo/bin/"
		  "/home/dwrz/.go/bin/"
		  "/home/dwrz/.local/bin/"
		  "/home/dwrz/.node_modules/bin/"
		  "/usr/bin"
		  "/usr/bin/core_perl"
		  "/usr/bin/site_perl"
		  "/usr/bin/vendor_perl"
		  "/usr/local/bin")
      eww-search-prefix "https://www.ecosia.org/search/?q="
      gnutls-verify-error t
      inhibit-splash-screen t
      inhibit-startup-message t
      initial-major-mode 'org-mode
      initial-scratch-message nil
      next-screen-context-lines 5
      password-cache-expiry 3600
      ring-bell-function 'ignore
      scroll-conservatively 10000
      sentence-end-double-space nil
      split-height-threshold nil
      split-width-threshold 160
      tls-checktrust t
      tramp-default-method "ssh"
      use-dialog-box nil
      user-full-name "David Wen Riccardi-Zhu"
      user-mail-address "dwrz@dwrz.net"
      select-enable-primary t
      select-enable-clipboard t
      x-stretch-cursor t)

(setq-default c-basic-offset 8
	      explicit-shell-file-name (getenv "SHELL")
	      fill-column 80
	      indent-tabs-mode t
	      shell-file-name (getenv "SHELL")
	      tab-width 8
	      truncate-lines nil)

(set-face-attribute
 'default t
 :family "DejaVu Sans Mono"
 :foundry "PfEd"
 :slant 'normal
 :weight 'normal
 :height 140
 :width 'normal)

;; FUNCTIONS

(defun dwrz-copy-filename ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
		      default-directory (buffer-file-name))))
    (when filename (kill-new filename)
          (message "Copied buffer file name '%s' to the clipboard." filename))))

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

(defun dwrz-org-capture-at-point ()
  "Insert an org capture template at point."
  (interactive)
  (org-capture 0))

(defun dwrz-terminal ()
  "Start a terminal."
  (interactive)
  (start-process "terminal" nil "st"))

(defun dwrz-remove-bars ()
  "Remove menu, scroll, tool, and window-divider bars."
  (interactive)
  (when (fboundp 'menu-bar-mode)(menu-bar-mode -1))
  (when (fboundp 'scroll-bar-mode)(scroll-bar-mode -1))
  (when (fboundp 'tool-bar-mode)(tool-bar-mode -1))
  (when (fboundp 'window-divider-mode)(window-divider-mode -1)))

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
(setq load-prefer-newer t)

(require 'package)
(setq package-archives
      '(("org" . "https://orgmode.org/elpa/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(require 'auto-compile)
(add-hook 'auto-compile-inhibit-compile-hook
	  'auto-compile-inhibit-compile-detached-git-head)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)
(setq auto-compile-display-buffer nil
      auto-compile-mode-line-counter t
      auto-compile-source-recreate-deletes-dest t
      auto-compile-toggle-deletes-nonlib-dest t
      auto-compile-update-autoloads t)

(require 'alert)
(require 'all-the-icons)
(require 'anzu)
(require 'avy)
(require 'calfw)
(require 'calfw-cal)
(require 'calfw-org)
(require 'company)
(require 'company-box)
(require 'company-lsp)
(require 'company-quickhelp)
(require 'counsel)
(require 'counsel-tramp)
(require 'csv-mode)
(require 'dired-hide-dotfiles)
(require 'dired-open)
(require 'dockerfile-mode)
(require 'doom-modeline)
(require 'emmet-mode)
(require 'emojify)
(require 'flycheck)
(require 'go-mode)
(require 'go-playground)
(require 'go-tag)
(require 'ibuffer)
(require 'ivy)
(require 'ivy-rich)
(require 'js2-mode)
(require 'keychain-environment)
(require 'ledger-mode)
(require 'lsp-mode)
;; (require 'lsp-ui)
(require 'lsp-ivy)
(require 'magit)
(require 'markdown-mode)
(require 'messages-are-flowing)
(require 'notmuch)
(require 'ob-restclient)
(require 'ol-notmuch)
(require 'org)
(require 'org-present)
(require 'pandoc-mode)
(require 'paren)
(require 'pdf-tools)
(require 'pos-tip)
(require 'pyim)
(require 'pyim-basedict)
(require 'rainbow-mode)
(require 'restclient)
(require 'rmsbolt)
(require 'sort-words)
(require 'subword)
(require 'super-save)
(require 'swiper)
(require 'systemd)
(require 'toc-org)
(require 'visual-fill-column)
(require 'web-mode)
(require 'wgrep)
(require 'which-key)
(require 'yaml-mode)
(require 'yasnippet)

;; PACKAGE CONFIGURATION
(setq auto-revert-verbose nil
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      auto-save-visited-mode t
      avy-all-windows 'all-frames
      avy-background t
      avy-case-fold-search nil
      avy-keys '(?a ?o ?e ?u ?h ?t ?n ?s)
      avy-style 'at-full
      backup-directory-alist `((".*" .,temporary-file-directory))
      bookmark-save-flag 1
      browse-url-browser-function 'eww-browse-url
      calendar-chinese-all-holidays-flag t
      calendar-week-start-day 1
      confirm-kill-emacs 'y-or-n-p
      diary-file "~/ruck/oo/org/diary.org"
      counsel-rg-base-command
      "rg -S -M 120 --no-heading --line-number --color never %s ."
      counsel-find-file-at-point t
      dired-open-extensions
      '(("mkv" . "mpv")
	("mp4" . "mpv")
	("avi" . "mpv"))
      dired-dwim-target t
      dired-listing-switches "-alh"
      dired-omit-files (concat dired-omit-files "\\|^\\..+$")
      dired-omit-verbose nil
      dired-recursive-copies 'always
      dired-clean-up-buffers-too t
      doc-view-resolution 150
      emojify-emoji-styles '(unicode)
      git-commit-summary-max-length 50
      global-auto-revert-non-file-buffers t
      holiday-bahai-holidays nil
      ispell-program-name "/usr/bin/aspell"
      ispell-dictionary "en_US"
      ispell-extra-args '("--sug-mode=ultra" "--lang=en_US")
      ispell-list-command "--list"
      ispell-dictionary-alist
      '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "['‘’]"
	 t ("-d" "en_US") nil utf-8))
      ivy-initial-inputs-alist nil
      ivy-rich-path-style 'abbrev
      ivy-wrap t
      ivy-use-virtual-buffers t
      enable-recursive-minibuffers t
      ivy-count-format "(%d/%d) "
      js-indent-level 2
      locale-coding-system 'utf-8
      markdown-command "pandoc"
      message-directory "drafts"
      message-kill-buffer-on-exit t
      message-sendmail-envelope-from 'header
      message-sendmail-f-is-evil nil
      rmsbolt-command "gcc -O3 -Wall -Wstrict-prototypes -std=c17 -pedantic"
      web-mode-code-indent-offset 2
      web-mode-css-indent-offset 2
      web-mode-indent-style 1
      web-mode-markup-indent-offset 2
      which-key-mode t
      sh-basic-offset 2
      async-shell-command-buffer "new-buffer"
      backward-delete-char-untabify-method nil
      mail-user-agent 'message-user-agent
      shift-select-mode nil
      super-save-auto-save-when-idle t
      split-window-preferred-function 'visual-fill-column-split-window-sensibly)

(with-eval-after-load 'company
  (define-key company-active-map (kbd "<return>") nil)
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "<tab>") 'company-complete-selection))
(setq company-backends
      '((company-yasnippet company-semantic company-clang company-cmake
			   company-capf company-files company-gtags
			   company-etags company-keywords)
	(company-abbrev company-dabbrev company-dabbrev-code))
      company-idle-delay 0
      company-minimum-prefix-length 1
      company-show-numbers t
      company-tooltip-align-annotations t)

(put 'dired-find-alternate-file 'disabled nil)

(with-eval-after-load 'dired-hide-dotfiles
  (define-key dired-mode-map (kbd ".") 'dired-hide-dotfiles-mode))

(with-eval-after-load 'erc
  (setq erc-nick "dwrz")
  (add-to-list 'erc-modules 'notifications)
  (add-to-list 'erc-modules 'spelling))

(with-eval-after-load 'go-mode
  (define-key go-mode-map (kbd "C-c C-b") 'pop-tag-mark)
  (define-key go-mode-map (kbd "C-c t") 'go-tag-add)
  (define-key go-mode-map (kbd "C-c T") 'go-tag-remove))

(setq go-playground-ask-file-name nil
      go-playground-basedir "/home/dwrz/.go/src/playground/"
      go-tag-args (list "-transform" "camelcase"))

(setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)

(eval-after-load "org-present"
  '(progn (add-hook 'org-present-mode-hook
		    (lambda ()
		      (org-present-big)
		      (org-display-inline-images)
		      (org-present-read-only)))
	  (add-hook 'org-present-mode-quit-hook
		    (lambda ()
		      (org-present-small)
		      (org-present-read-write)))))

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
(define-key notmuch-search-mode-map "R" 'notmuch-search-reply-to-thread-sender)

(setq org-link-frame-setup
      '((vm . vm-visit-folder-other-frame)
	(vm-imap . vm-visit-imap-folder-other-frame)
	(gnus . org-gnus-no-new-news)
	(file . find-file)
	(wl . wl-other-frame)))

(add-to-list 'org-src-lang-modes '("js" . js2))
(add-to-list 'org-modules 'org-habit)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((awk .t ) (calc . t) (C . t) (emacs-lisp . t) (gnuplot . t) (js . t) (js . t)
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
			  (sequence "AR(a)" "GOAL(g)")))

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
      org-agenda-span 'month
      org-agenda-tags-column 'auto
      org-agenda-window-setup 'current-window)

(setq org-capture-templates
      '(("g" "goal" entry
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
	 :empty-lines-after 1)))

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
	("GOAL" . "springgreen")))

(setq org-src-preserve-indentation t
      org-src-tab-acts-natively t)

(set-register ?j '(file . "~/ruck/oo/journal/2020.org"))
(set-register ?i '(file . "~/ruck/oo/config/emacs/init.el"))
(set-register ?g '(file . "~/ruck/oo/org/gtd.org"))
(set-register ?m '(file . "~/ruck/oo/org/mindsweep-trigger-list.org"))
(set-register ?o '(file . "~/ruck/oo/org/dwrz.org"))

(setq mail-specify-envelope-from t
      mail-envelope-from 'header
      mail-specify-envelope-from t
      send-mail-function 'sendmail-send-it
      sendmail-program "~/.msmtpqueue/msmtp-enqueue.sh")

(with-eval-after-load 'yasnippet (define-key yas-keymap (kbd "<tab>") nil))

;; CUSTOMIZE
(customize-set-variable 'epg-gpg-program "/usr/bin/gpg2")
(customize-set-variable 'face-font-family-alternatives
			'(("hans" "adobe-source-han-sans-cn-font")))
(customize-set-variable 'mouse-wheel-scroll-amount '(1 ((shift) .1)))

;; HOOKS
(add-hook 'after-init-hook 'doom-modeline-mode)
(add-hook 'after-init-hook '(lambda ()
			      (setq base16-theme-256-color-source 'colors)
			      (load-theme 'base16-tomorrow t)
			      (set-face-attribute
			       'fringe t :background "#ffffff")
			      (setq base16-distinct-fringe-background nil)
			      (dwrz-remove-bars)))
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'company-mode-hook 'company-box-mode)
(add-hook 'conf-space-mode-hook 'rainbow-mode)
(add-hook 'css-mode-hook 'html-mode 'web-mode)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'lsp)
(add-hook 'go-mode-hook 'lsp)
(add-hook 'js2-mode-hook 'js2-imenu-extras-mode)
(add-hook 'js2-mode-hook 'lsp)
(add-hook 'message-mode-hook 'messages-are-flowing-use-and-mark-hard-newlines)
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flycheck-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'prog-mode-hook 'rainbow-mode)
(add-hook 'prog-mode-hook 'visual-line-mode)
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook 'rainbow-mode)
(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'visual-line-mode-hook 'visual-fill-column-mode)
(add-hook 'web-mode-hook 'electric-pair-mode)
(add-hook 'emacs-lisp-mode-hook
          '(lambda () (set (make-local-variable 'company-backends)
			   '((company-lsp company-capf company-files)))))
(add-hook 'go-mode-hook
	  '(lambda ()
	     (set (make-local-variable 'company-backends)
		  '((company-lsp company-files)))
	     (set (make-local-variable 'before-save-hook)
		  '(lsp-organize-imports
		    lsp-format-buffer
		    delete-trailing-whitespace))))
(add-hook 'ibuffer-mode-hook
	  (lambda () (ibuffer-switch-to-saved-filter-groups "default")))
(add-hook 'js2-mode-hook
	  '(lambda ()
	     (set (make-local-variable 'company-backends)
		  '((company-lsp company-capf company-files)))))
(add-hook 'text-mode-hook
          '(lambda ()
	     (set (make-local-variable 'company-backends) '((company-capf company-files)))))
(add-hook 'org-babel-after-execute-hook
	  (lambda () (when org-inline-image-overlays
		       (org-redisplay-inline-images))))
(add-hook 'org-mode-hook
          '(lambda () (set (make-local-variable 'company-backends)
			   '((company-capf company-yasnippet company-files)))))
(add-hook 'web-mode-hook '(lambda () (set (make-local-variable 'company-backends) '((company-web-html company-capf company-yasnippet company-files)))))

(font-lock-add-keywords 'prog-mode '(("\\<\\(FIX\\|TODO\\|NB\\)" 1
				      font-lock-warning-face t)))
(font-lock-add-keywords 'go-mode '(("\\<\\(FIX\\|TODO\\|NB\\)" 1
				    font-lock-warning-face t)))
(font-lock-add-keywords 'emacs-lisp-mode '(("\\<\\(FIX\\|TODO\\|NB\\)" 1
					    font-lock-warning-face t)))

;; PACKAGE ENABLE
(auto-compression-mode t)
(column-number-mode t)
(column-number-mode)
(company-quickhelp-mode t)
(delete-selection-mode nil)
(global-anzu-mode t)
(global-auto-revert-mode t)
(global-company-mode t)
(global-emojify-mode t)
(global-hl-line-mode t)
(global-subword-mode t)
(ivy-mode t)
(ivy-rich-mode t)
(keychain-refresh-environment)
(pdf-loader-install)
(pyim-basedict-enable)
(show-paren-mode t)
(size-indication-mode)
(super-save-mode t)
(yas-global-mode t)
(yas-load-directory "/home/dwrz/.emacs.d/snippets")

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

(defhydra hydra-point (:color blue)
  "
^point^       ^format^       ^insert^       ^act^
^───────^─────^──────^───────^──────^───────^────^───────────
_q_ quit        ^^             _t_ timestamp  _h_ hippie-x
^^              ^^             _d_ date       _k_ delete word
^^              ^^             ^^             _s_ spell
^^              ^^             ^^             ^^
"
  ("d" (insert (format-time-string "%Y%m%d")))
  ("h" hippie-expand :color red)
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
^^          ^^              _r_ region
^^          ^^              _w_ windows
^^          ^^              ^^
^^          ^^              ^^
"
  ("q" nil)
  ("h" hydra-highlight/body)
  ("p" hydra-point/body)
  ("r" hydra-region/body)
  ("w" hydra-windows/body))

;; TODO: Hydra
;; Help
;; Execute (calendar, mail, terminal)
;; Display (line numbers)
;; Navigation (goto char, gotoline)

;; KEYBINDINGS
(global-set-key [remap query-replace] 'anzu-query-replace)
(global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'ivy-switch-buffer)
(global-set-key (kbd "C-c c") 'dwrz-org-capture-at-point)
(global-set-key (kbd "C-c d") nil)
(global-set-key (kbd "C-c e") 'counsel-M-x)
(global-set-key (kbd "C-c f") 'counsel-find-file)
(global-set-key (kbd "C-c g") 'avy-goto-line)
(global-set-key (kbd "C-c h") nil)
(global-set-key (kbd "C-c i") nil)
(global-set-key (kbd "C-c j") 'jump-to-register)
(global-set-key (kbd "C-c k") nil)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c m") 'emojify-insert-emoji)
(global-set-key (kbd "C-c n") 'hydra-meta-hydra/body)
(global-set-key (kbd "C-c o") 'avy-goto-char)
(global-set-key (kbd "C-c p") 'hydra-point/body)
(global-set-key (kbd "C-c q") nil)
(global-set-key (kbd "C-c r") nil)
(global-set-key (kbd "C-c s") nil)
(global-set-key (kbd "C-c t") nil)
(global-set-key (kbd "C-c u") nil)
(global-set-key (kbd "C-c v") nil)
(global-set-key (kbd "C-c w") 'hydra-windows/body)
(global-set-key (kbd "C-c x") 'yas-expand)
(global-set-key (kbd "C-c y") nil)
(global-set-key (kbd "C-c z") nil)
(global-set-key (kbd "C-c #") 'display-line-numbers-mode)
(global-set-key (kbd "C-h f") 'counsel-describe-function)
(global-set-key (kbd "C-h v") 'counsel-describe-variable)
(global-set-key (kbd "C-r") 'counsel-rg)
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-x b") 'ibuffer)
(global-set-key (kbd "C-x c") 'dwrz-open-calendar)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x m") 'notmuch)
(global-set-key (kbd "C-x t") 'dwrz-terminal)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "M-n") 'hydra-meta-hydra/body)
(global-set-key (kbd "M-p") 'hydra-region/body)

;; Load custom.el.
(when (file-exists-p custom-file) (load custom-file))
