;;; -*- lexical-binding: t -*-
(setq debug-on-error t)
(add-to-list 'load-path "~/.emacs.d/elisp")
(load-theme 'real-mono-eink t)
(setq-default ;; Use setq-default to define global default
 ;; Who I am
 user-mail-address "llqingsong@qq.com"
 user-full-name "Qingsong Liao"
 calendar-longitude +106.5
 calendar-latitude +29.5
 ;; Enable all disabled commands
 disabled-command-function nil
 word-wrap-by-category t
 ;; unsafe theme
 custom-safe-themes t
 ;; Enable recursive minibuffer edit
 enable-recursive-minibuffers t
 ;; Don't show scratch message, and use fundamental-mode for *scratch*
 ;; Remove splash screen and the echo area message
 inhibit-startup-message t
 inhibit-startup-echo-area-message t
 initial-scratch-message 'nil
 initial-major-mode 'fundamental-mode
 ;; Emacs modes typically provide a standard means to change the
 ;; indentation width -- eg. c-basic-offset: use that to adjust your
 ;; personal indentation width, while maintaining the style (and
 ;; meaning) of any files you load.
 indent-tabs-mode nil ; don't use tabs to indent
 tab-width 4 ; but maintain correct appearance
 ;; Use one space as sentence end
 sentence-end-double-space 'nil
 ;; Newline at end of file
 require-final-newline t
 ;; Don't adjust window-vscroll to view tall lines.
 auto-window-vscroll nil
 ;; Don't create lockfiles.
 ;; recentf frequently prompts for confirmation.
 create-lockfiles nil
 ;; Leave some rooms when recentering to top, useful in emacs ipython notebook.
 recenter-positions '(middle 1 bottom)
 ;; Move files to trash when deleting
 delete-by-moving-to-trash t
 ;; Show column number
 column-number-mode t
 ;; Don't break lines for me, please
 truncate-lines t
 ;; More message logs
 message-log-max 16384
 ;; Don't prompt up file dialog when click with mouse
 use-file-dialog nil
 ;; Place all auto-save files in one directory.
 backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
 ;; more useful frame title, that show either a file or a
 ;; buffer name (if the buffer isn't visiting a file)
 frame-title-format '((:eval (if (buffer-file-name)
                                 (abbreviate-file-name (buffer-file-name))
                               "%b")))
 ;; warn when opening files bigger than 100MB
 large-file-warning-threshold 100000000
 ;; Don't create backup files
 make-backup-files nil ; stop creating backup~ files
 ;; Remember my location when the file is last opened
 ;; activate it for all buffers
 save-place-file (expand-file-name "saveplace" user-emacs-directory)
 ;; turn off the bell
 ring-bell-function 'ignore
 ;; Smoother scrolling
 mouse-wheel-scroll-amount '(1 ((shift) . 1)) ;; one line at a time
 mouse-wheel-progressive-speed nil ;; don't accelerate scrolling
 mouse-wheel-follow-mouse 't ;; scroll window under mouse
 scroll-step 1 ;; keyboard scroll one line at a time
 view-read-only t ;; make read-only buffers in view mode
 ;; Native comp
 package-native-compile t
 comp-async-report-warnings-errors nil
 ;; Ignore 'ad-handle-definition' redefined warnings
 ad-redefinition-action 'accept
 ;; mouse auto follow
 mouse-autoselect-window t
 focus-follow-mouse 'auto-raise
 ;; Hide warnings and display only errors
 warning-minimum-level :error
 ;; Display of line numbers in the buffer:
 ;; Track changes in the window configuration, allowing undoing actions such as
 ;; closing windows.
 ;; Configure Emacs to ask for confirmation before exiting
 confirm-kill-emacs 'y-or-n-p
 dictionary-server "localhost"
 browse-url-firefox-program "firefox-beta"
 browse-url-firefox-arguments "-P firefox"
 browse-url-new-window-flag nil
 browse-url-firefox-new-window-is-tab t
 dired-movement-style 'cycle
 line-number-mode t
 column-number-mode t
 mode-line-position-column-line-format '("%l:%C")
 isearch-allow-scroll t
 package-install-upgrade-built-in t
 ;;; to the of the compilation
 compilation-scroll-output t
 ;;; no message of revert buffer
 auto-revert-verbose nil
 ;;; no fringe bookmark
 bookmark-fringe-mark nil
 ;;; wdired
 wdired-allow-to-change-permissions t
 wdired-create-parent-directories t
 ;;;  scroll
 scroll-preserve-screen-position t
 scroll-conservatively 1 ; affects scroll-step'
 scroll-margin 0
 next-screen-context-lines 0
 tooltip-hide-delay 20
 tooltip-delay 0.4
 tooltip-short-delay 0.08
 org-latex-compiler "xelatex"
 compilation-window-height 5
 woman-fill-column 80
 Man-width 80
 compilation-auto-jump-to-first-error nil)
(require 'use-package)
(use-package consult
  :ensure t
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.5)
  (setq register-preview-function #'consult-register-format)
  (setq xref-show-xrefs-function #'consult-xref)
  (setq xref-show-definitions-function #'consult-xref)
  (advice-add #'register-preview :override #'consult-register-window)
  :config
  (move-text-default-bindings)
  (use-package surround :bind-keymap ("C-<tab>" . surround-keymap))
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<")
  :bind (

         ([remap comment-line] . comment-or-uncomment-region-or-line)
         ([remap dabbrev-expand] . hippie-expand)
         ([remap imenu] . consult-imenu)
         ([remap indent-rigidly] . smart-shift-right)
         ([remap kill-region] . kill-line-or-region)
         ([remap list-buffers] . ibuffer)
         ([remap project-switch-to-buffer] . consult-project-buffer)
         ([remap switch-to-buffer] . consult-buffer)
         ([remap yank-pop] . consult-yank-pop)

         ("C-<backspace>" . avy-goto-word-0)
         ("C-<iso-lefttab>" . surround-insert)
         ("C-<return>" . (lambda () (interactive) (duplicate-dwim)(next-line)))
         ("C-M-<return>" . copy-from-above-command)
         ("C-M-<tab>" . surround-delete)
         ("C-S-<return>" . avy-goto-line)
         ("C-s-<tab>" . surround-change)
         ("C-x C-s" . (lambda () (interactive) (save-buffer)(donothing)))
         ("S-<return>" . comment-indent-new-line)

         ;; No Need "qbj'" , they are finger killers
         ;; MOS
         ;; a comment  c-x c-;
         ;; r quit c-g
         ;; s find-file c-x c-f
         ;; t switch-buffer c-x b
         ;; b ibuffer  c-x c-b

         ("C-c C-; d" .  dired)
         ("C-c C-; f" . rg-dwim)
         ("C-c C-; g" . magit-status)
         ("C-c C-; k" . kill-current-buffer)
         ("C-c C-; m" . devdocs-browser-open)
         ("C-c C-; p" . disproject-dispatch)
         ("C-c C-; u" . delete-all-space)
         ("C-c C-; v" . multi-vterm-project)
         ("C-c C-; y" . yas-insert-snippet)
         ("C-c C-; z" . isearch-forward-symbol-at-point)

         ("C-c C-; x" . consult-complex-command)
         ("C-c C-; c" . cleanup-buffer)
         ("C-c C-; l" . git-link-dispatch)
         ("C-c C-; w" . (lambda () (interactive)     (completion-in-region (point) (point) (list
                                                                                            (format "P: %s : %s" (line-number-at-pos)(current-column))
                                                                                            (format "%s" (if (equal major-mode 'dired-mode) default-directory (buffer-file-name)))
                                                                                            (format "%s" (buffer-name))
                                                                                            ;; (format "%s" (current-time-string))
                                                                                            ;; (format "S: %s" easysession--current-session-name)
                                                                                            ))))

         ("C-c C-; '" . donothing)      ; b for ibuffer
         ("C-c C-; q" . donothing)
         ("C-c C-; j" . donothing)

         ;; NAV
         ("C-c C-~ c" .  split-window-below)
         ("C-c C-~ d" .  recompile)
         ("C-c C-~ g" .  er/expand-region)
         ("C-c C-~ l" .  magit-log-buffer-file)
         ("C-c C-~ u" . consult-mark)
         ("C-c C-~ x" .  delete-other-windows)
         ("C-c C-~ y" .  quick-sdcv-search-at-point)
         ("C-c C-~ z" .  delete-window)

         ("C-c C-~ f" . consult-line)
         ("C-c C-~ k" . consult-bookmark)
         ("C-c C-~ m" . man)
         ("C-c C-~ p" . (lambda () (interactive) (consult-fd "~/Zen/C/" nil)))
         ("C-c C-~ v" . compile)
         ("C-c C-~ w" . consult-org-agenda)

         ("C-c C-~ q" .  donothing)
         ("C-c C-~ b" .  (lambda () (interactive) (consult-ripgrep "~/Zen/C/" nil)))
         ("C-c C-~ j" .  donothing)
         ("C-c C-~ '" .  donothing)

         ;; get sym back, all kind of toggles
         ("C-c C-& l" . my-toggle-font)
         ("C-c C-& u" . vertico-flat-mode)
         ("C-c C-& y" . global-hide-mode-line-mode)
         ("C-c C-& m" . toggle-truncate-lines)
         ("C-c C-& k" . toggle-letter-case)
         ("C-c C-& h" . git-timemachine-toggle)
         ("C-c C-& ," . toggle-input-method)
         ("C-c C-& ." . calculator)
         ("C-c C-& /" . avy-isearch)

         ("C-c C-& '" . donothing)      ; q and b used on sym layer
         ("C-c C-& j" . donothing)

         ;; minor
         ("M-n". embark-next-symbol)
         ("M-p". embark-previous-symbol)
         ("C-v". (lambda() (interactive)(recenter-top-bottom 0)))
         ("M-v". (lambda() (interactive)(recenter-top-bottom 38)))
         ("M-*". my/leetcode-open)
         ;; ("M-*". (lambda () (interactive) (my/leetcode-open (string-to-number(current-word)))))
         ("M-I" . consult-imenu-multi)
         ("M-i". imenu)
         ("M-o". other-window)
         ("M-O". (lambda () (interactive) (other-window -1)))
         ("C-;". iedit-mode)
         ("C-,". toggle-solution-question)
         ("ESC <f5>". hibernatecall)
         ("<WakeUp>". donothing)
         ("M-#" . consult-register-load)
         ("M-$" . consult-register-store)
         ("C-M-#" . consult-register)
         ("M-g g" . consult-goto-line)
         ("M-s u" . consult-global-mark)
         ("M-s O" . multi-occur)
         :map isearch-mode-map
         ("M-r" . consult-isearch-history)
         :map minibuffer-local-map
         ("M-r" . consult-history)
         :map transient-map
         ("M-w". transient-copy-menu-text)))

(set-face-attribute 'default nil
                    :height 150)
(setq minimal-emacs-user-directory user-emacs-directory)
(setq minimal-emacs-var-dir
      (expand-file-name "var/" minimal-emacs-user-directory))
(setq user-emacs-directory minimal-emacs-var-dir)
(define-key key-translation-map (kbd "C-n") (kbd "C-x"))
(define-key key-translation-map (kbd "C-x") (kbd "C-n"))
(define-key key-translation-map (kbd "M-n") (kbd "M-x"))
(define-key key-translation-map (kbd "M-x") (kbd "M-n"))
(define-key key-translation-map (kbd "M-N") (kbd "M-X"))
(define-key key-translation-map (kbd "M-X") (kbd "M-N"))
;; (use-package real-mono-themes :config (load-theme 'real-mono-eink t))
(use-package compile-angel
  :ensure t
  :demand t
  :custom
  (compile-angel-verbose nil)
  :config
  (push "/post-init.el" compile-angel-excluded-files)
  (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode)
  (compile-angel-on-load-mode))
(use-package savehist
  :commands (savehist-mode savehist-save)
  :hook
  (after-init . savehist-mode)
  :custom
  (savehist-autosave-interval 600))
(use-package markdown-mode
  :commands (gfm-mode
             gfm-view-mode
             markdown-mode
             markdown-view-mode)
  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode))
  :bind
  (:map markdown-mode-map
        ("C-c C-e" . markdown-do)))
(use-package envrc :defer 2 :config (envrc-global-mode 1)
  (advice-add 'org-babel-eval :around #'envrc-propagate-environment))
(use-package vertico
  :ensure t
  :defer t
  :commands vertico-mode
  :bind(
        :map vertico-map
             ("<next>" . scroll-up-command)
             ("<prior>" . scroll-down-command)
             ("M-<next>" . vertico-next-group)
             ("M-<prior>" . vertico-previous-group)
        )
  :hook (after-init . vertico-mode))
(use-package vertico-flat
    :after vertico
    :ensure nil
    :init
    (vertico-flat-mode))
(use-package marginalia :ensure t :defer t
  :commands (marginalia-mode marginalia-cycle)
  :hook (after-init . marginalia-mode))
(use-package embark :ensure t :defer t
  :commands (embark-act
             embark-dwim
             embark-export
             embark-collect
             embark-bindings
             embark-prefix-help-command)
  :bind (("C-." . embark-act)
         ("M-." . embark-dwim)
         ("C-h B" . embark-bindings))
  :init (setq prefix-help-command #'embark-prefix-help-command)
  :config (add-to-list 'display-buffer-alist
                       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                         nil
                         (window-parameters (mode-line-format . none)))))
(use-package embark-consult :ensure t :hook
  (embark-collect-mode . consult-preview-at-point-mode))
(use-package autorevert
  :ensure nil
  :commands (auto-revert-mode global-auto-revert-mode)
  :hook
  (after-init . global-auto-revert-mode)
  :custom
  (auto-revert-interval 3)
  (auto-revert-remote-files nil)
  (auto-revert-use-notify t)
  (auto-revert-avoid-polling nil)
  (auto-revert-verbose nil))
(use-package orderless :ensure t :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))
(use-package eglot :ensure nil :defer t
  :hook (eglot-managed-mode . (lambda ()
                                (eglot-inlay-hints-mode -1)
                                (eldoc-mode -1) ; bloate
                                (flymake-mode -1) ; bloate
                                ))
  :custom
  (eglot-sync-connect 0)
  (eldoc-mode -1)
  (eglot-autoshutdown t)
  (eglot-extend-to-xref t)
  (eglot-events-buffer-config '(:size 0 :format short))
  (eglot-ignored-server-capabilities '(:documentLinkProvider
                                       :documentOnTypeFormattingProvider
                                       :foldingRangeProvider
                                       :colorProvider
                                       :inlayHintProvider))
  :config
  (setq flymake-no-changes-timeout nil)  ; Don't run on idle
  (setq flymake-start-on-flymake-mode nil) ; Don't auto-start
  (setq flymake-start-on-save-buffer nil)
  (setq flymake-fringe-indicator-position nil)
  (setq flymake-margin-indicator-position nil)
  (setq flymake-margin-indicators-string
        '((error "" compilation-error) (warning "" compilation-warning)
          (note "" compilation-info)))
  (setq flymake-indicator-type nil)
  (setq eglot-server-programs
        '((c-mode . ("clangd"))
          (c++-mode . ("clangd"))
          (zig-mode . ("zls"))
          (latex-mode . ("texlab"))
          (rust-mode . ("rust-analyzer")))))
(use-package helpful
  :ensure t
  :defer t
  :commands (helpful-callable
             helpful-variable
             helpful-key
             helpful-command
             helpful-at-point
             helpful-function)
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-function] . helpful-callable)
  ([remap describe-key] . helpful-key)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  :custom
  (helpful-max-buffers 7))
(use-package corfu
  :ensure t
  :commands (corfu-mode global-corfu-mode)
  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode))
  :custom
  (read-extended-command-predicate #'command-completion-default-include-p)
  (text-mode-ispell-word-completion nil)
  (tab-always-indent 'complete)
  :config (global-corfu-mode))
(use-package cape :ensure t :defer t
  :commands (cape-dabbrev cape-file cape-elisp-block)
  :bind ("C-c p" . cape-prefix-map)
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))
(use-package which-key
  :ensure nil
  :defer t
  :commands which-key-mode
  :hook (after-init . which-key-mode)
  :custom
  (which-key-idle-delay 1.5)
  (which-key-idle-secondary-delay 0.25)
  (which-key-add-column-padding 1)
  (which-key-max-description-length 40)
  (which-key-use-C-h-commands nil))
(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'reverse)
  (uniquify-separator "•")
  (uniquify-after-kill-buffer-p t))
(use-package dired
  :ensure nil
  :commands (dired)
  :bind
  ( :map dired-mode-map
    ("SPC" . scroll-up-command)
    ("DEL" . scroll-down-command)
    ("," . dired-omit-mode)
    ("e" . wdired-change-to-wdired-mode))
  :hook
  ((dired-mode . dired-hide-details-mode)
   (dired-mode . dired-omit-mode))
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t)
  :custom
  (dired-omit-files
   (rx (or (seq bol ".")                      ; dotfiles
           (seq ".js" (? ".meta") eos)        ; .js.meta
           (seq "." (or "elc" "cls" "fls" "bib" "fdb_latexmk" "log" "aux" "a" "bbl" "o" "pyc" "pyo" "swp" "class") eos)
           (seq bol ".DS_Store")
           (seq bol "." (or "svn" "git") eos)
           (seq bol ".ccls-cach" eos)
           (seq bol "__pycache__" eos)
           (seq bol ".project" (? "ile") eos)
           (seq bol (or "flake.lock" "Cargo.lock" "LICENSE") eos)
           (seq bol (or "flycheck_" "flymake_"))))))
;; dired: Group directories first
(with-eval-after-load 'dired
  (let ((args "--group-directories-first -ahlv"))
    (when (or (eq system-type 'darwin) (eq system-type 'berkeley-unix))
      (if-let* ((gls (executable-find "gls")))
          (setq insert-directory-program gls)
        (setq args nil)))
    (when args
      (setq dired-listing-switches args))))
;; Enables visual indication of minibuffer recursion depth after initialization.
(add-hook 'after-init-hook #'minibuffer-depth-indicate-mode)
;; Configure Emacs to ask for confirmation before exiting
(setq confirm-kill-emacs 'y-or-n-p)
;; Enabled backups save your changes to a file intermittently
(setq make-backup-files t)
(setq vc-make-backup-files t)
(setq kept-old-versions 10)
(setq kept-new-versions 10)
(use-package dired-subtree
  :after dired
  :bind ( :map dired-mode-map
          ("<tab>" . dired-subtree-toggle))
  :config (setq dired-subtree-use-backgrounds nil))
(use-package trashed
  :ensure t
  :commands (trashed)
  :config
  (setq trashed-action-confirmer 'y-or-n-p
        trashed-use-header-line t
        trashed-sort-key '("Date deleted" . t)
        trashed-date-format "%Y-%m-%d %H:%M:%S"))
(use-package org
  :ensure t
  :defer t
  :commands (org-mode org-version)
  :mode
  ("\\.org\\'" . org-mode)
  :custom
  (org-latex-src-block-backend 'listings)
  (org-export-with-broken-links t)
  (org-export-with-tags nil)
  (org-export-with-drawers nil)
  (org-export-with-toc nil)
  (org-babel-no-eval-on-export t)
  (org-hide-leading-stars nil)
  (org-startup-folded 'showeverything)
  (org-agenda-span 'week)
  (org-log-into-drawer t)
  (org-startup-indented nil)
  (org-adapt-indentation nil)
  (org-edit-src-content-indentation 0)
  (org-startup-truncated nil)
  (org-fontify-done-headline nil)
  (org-fontify-todo-headline nil)
  (org-hide-emphasis-markers nil)
  (org-fontify-whole-heading-line nil)
  (org-fontify-quote-and-verse-blocks nil)
  (org-confirm-babel-evaluate nil)
  (org-startup-with-inline-images t)
  (org-link-descriptive nil)
  ;; (org-agenda-files '("~/Leere/oooo.org"))
  (org-agenda-files (directory-files-recursively "~/Leere/NestorLiao.github.io/" "\\.org$"))
  (org-todo-keywords ;; t要做的，f要修的，e暂时的，a失败的，k有缺陷的，o完成的
   '((sequence  "TODO(t)" "DONE(d)" "|" "FIXME(f)")
     (sequence "TEMP(e)" "FAIL(a)" "KLUDGE(k)"    "|" "OKAY(o)")))
  :bind (( "C-c a" . org-agenda)
         ( "C-c l" . org-store-link))
  :config
  (add-hook 'org-mode-hook  #'toggle-truncate-lines)
  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "C-,") nil)
    (define-key org-mode-map (kbd "C-<return>") nil)
    (define-key org-mode-map (kbd "C-S-<return>") #'org-insert-heading-respect-content))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     (emacs-lisp . t)
     (shell . t)
     (python . t))))
(use-package tldr
  :bind ( :map tldr-mode-map
          ("SPC" . scroll-up-command)
          ("DEL" . scroll-down-command)
          ("t" . tldr)))
(use-package ligature
  :config
  (let ((ligs '("ff" "fi" "ffi" "fl" "ffl")))
    (ligature-set-ligatures 't ligs))
  (global-ligature-mode 1))
(use-package avy
  :ensure t
  :bind (("M-s M-k" . avy-kill-whole-line))
  :config
  (setq avy-all-windows t)
  (setq avy-keys '( ?r ?s ?t ?d ?h ?n ?e ?i))
  (avy-setup-default)
  (setq isearch-allow-motion t)
  (defun avy-action-kill-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (kill-whole-line))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)
  (defun avy-action-copy-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end)
          (bounds-of-thing-at-point 'line)
        (copy-region-as-kill start end)))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)
  (defun avy-action-yank-whole-line (pt)
    (avy-action-copy-whole-line pt)
    (save-excursion (yank))
    t)
  (defun avy-action-mark-to-char (pt)
    (activate-mark)
    (goto-char pt))
  (defun avy-quick-sdcv-search-at-point (pt)
    "Jump to the point PT and search the definition of the word at that point."
    (interactive "d")
    (goto-char pt)
    (quick-sdcv-search-at-point))
  (defun avy-copy-region-between-words ()
    "Use avy to mark region between two words (including both words) and copy to kill ring."
    (interactive)
    (let ((orig-pos (point)))
      (avy-goto-word-0 1)
      (forward-word)  ; Move to end of first word
      (let ((start (point)))
        (avy-goto-word-0 1)
        (forward-word)  ; Move to end of second word
        (copy-region-as-kill start (point))
        (goto-char orig-pos))))
  (defun avy-action-copy-between-words (pt)
    (save-excursion
      (goto-char pt)
      (let ((start (point)))
        (avy-goto-word-0 1)
        (forward-word 1)  ; Move to end of first word
        (copy-region-as-kill start (point))))
    t)
  (setf (alist-get ?y avy-dispatch-alist) 'avy-action-yank
        (alist-get ?w avy-dispatch-alist) 'avy-action-copy
        (alist-get ?W avy-dispatch-alist) 'avy-action-copy-whole-line
        (alist-get ?Y avy-dispatch-alist) 'avy-action-yank-whole-line
        (alist-get ?o avy-dispatch-alist) 'avy-action-copy-between-words
        (alist-get ?k avy-dispatch-alist) 'avy-action-kill-stay
        (alist-get ?K avy-dispatch-alist) 'avy-action-kill-whole-line
        (alist-get ?  avy-dispatch-alist) 'avy-action-mark-to-char
        (alist-get ?q avy-dispatch-alist) 'avy-quick-sdcv-search-at-point))
(use-package diff-mode :ensure nil
  :custom
  (diff-default-read-only t)
  (diff-font-lock-syntax 'hunk-also)
  (diff-font-lock-prettify t))
(use-package buffer-terminator
  :ensure t
  :custom
  (buffer-terminator-verbose nil)
  (buffer-terminator-inactivity-timeout 3600)
  (buffer-terminator-interval 1800)
  :config
  (buffer-terminator-mode 1))
(use-package magit
  :config
  (setq magit-bury-buffer-function 'magit-restore-window-configuration)
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  (setq magit-diff-refine-hunk  'all)
  (magit-todos-mode 1)
  (setq hl-todo-keyword-faces
        '(("HOLD"   . "#000000")
          ("TODO"   . "#000000")
          ("NEXT"   . "#000000")
          ("THEM"   . "#000000")
          ("PROG"   . "#000000")
          ("OKAY"   . "#000000")
          ("DONT"   . "#000000")
          ("FAIL"   . "#000000")
          ("DONE"   . "#000000")
          ("NOTE"   . "#000000")
          ("MAYBE"  . "#000000")
          ("KLUDGE" . "#000000")
          ("HACK"   . "#000000")
          ("TEMP"   . "#000000")
          ("FIXME"  . "#000000")
          ("XXXX*"  . "#000000"))))
(use-package quick-sdcv
  :custom
  (quick-sdcv-dictionary-prefix-symbol "►")
  (quick-sdcv-ellipsis " ▼ ")
  :bind
  ( :map quick-sdcv-mode-map
    ("SPC" . scroll-up-command)
    ("DEL" . scroll-down-command)
    ;; ("y" . previous-buffer)
    ("y" . (lambda () (interactive) (previous-buffer) (other-window 1)))
    ("u" . quick-sdcv-search-at-point)))
(use-package elisp-autofmt
  :commands (elisp-autofmt-mode
             elisp-autofmt-buffer
             elisp-autofmt-region))
(use-package multiple-cursors
  :bind (
         ("C-S-c C-S-c" . mc/edit-lines)
         ("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C-$"     . mc/mark-all-like-this)
         ("C-$"        . mc/skip-to-next-like-this)
         ("C-^"         . mc/skip-to-previous-like-this)
         :map mc/keymap
         ("<return>" . electric-newline-and-maybe-indent)
))
(use-package recentf
  :ensure nil
  :commands (recentf-mode recentf-cleanup)
  :hook
  (after-init . recentf-mode)
  :custom
  (recentf-auto-cleanup (if (daemonp) 300 'never))
  (recentf-exclude
   (list "\\.tar$" "\\.tbz2$" "\\.tbz$" "\\.tgz$" "\\.bz2$"
         "\\.bz$" "\\.gz$" "\\.gzip$" "\\.xz$" "\\.zip$"
         "\\.7z$" "\\.rar$"
         "COMMIT_EDITMSG\\'"
         "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
         "-autoloads\\.el$" "autoload\\.el$"))
  :config
  (add-hook 'kill-emacs-hook #'recentf-cleanup -90))
(defun my/suppress-recentf-messages (orig-fun &rest args)
  "Run `recentf-cleanup` without showing messages in the echo area."
  (let ((inhibit-message t)
        (message-log-max nil))
    (apply orig-fun args)))
(advice-add 'recentf-cleanup :around #'my/suppress-recentf-messages)
(use-package super-save :config (super-save-mode 1)
  (setq super-save-auto-save-when-idle t
        super-save-silent t
        super-save-all-buffers  t
        super-save-remote-files nil
        super-save-idle-duration 4))
(use-package undo-fu :ensure t :defer t
  :commands (undo-fu-only-undo
             undo-fu-only-redo
             undo-fu-only-redo-all
             undo-fu-disable-checkpoint))
(use-package undo-fu-session :ensure t :defer t
  :commands undo-fu-session-global-mode
  :hook (after-init . undo-fu-session-global-mode))
(use-package saveplace
  :ensure nil
  :commands (save-place-mode save-place-local-mode)
  :hook
  (after-init . save-place-mode)
  :custom
  (save-place-limit 400))
(use-package yasnippet  :config
  (setq yas-snippet-dirs '("~/.config/snippets"))
  (yas-global-mode 1))
(use-package no-emoji
  :config
  (setq no-emoji-display-table (make-display-table))
  (set-face-attribute 'no-emoji nil
                      :background (face-attribute 'default :background)
                      :foreground (face-attribute 'default :background)
                      :height 0.1)
  (global-no-emoji-minor-mode 1))
(use-package eww
  :ensure nil
  :config
  (setq
   webjump-sites
   '(("Linux Kernel" . "http://linux.doc:3000/")
     ("C++ Reference" . "http://cpp.doc:3002/")
     ("C Reference" . "http://c.doc:3001/")
     ("GNU Project FTP Archive"
      . [mirrors "https://ftp.gnu.org/pub/gnu/" "https://ftpmirror.gnu.org"])
     ("GNU Project Website" . "https://www.gnu.org")
     ("Emacs Website" . "https://www.gnu.org/software/emacs/emacs.html")
     ("Savannah Emacs page" . "https://savannah.gnu.org/projects/emacs")
     ("Emacs Wiki"
      . [simple-query "https://www.emacswiki.org"
                      "https://www.emacswiki.org/cgi-bin/wiki/" ""])
     ("DuckDuckGo"
      . [simple-query "https://duckduckgo.com" "https://duckduckgo.com/?q=" ""])
     ("Wikipedia"
      . [simple-query "https://wikipedia.org" "https://wikipedia.org/wiki/" ""])
     ("Association for Computing Machinery" . "https://www.acm.org")
     ("Computer Professionals for Social Responsibility" . "http://www.cpsr.org")
     ("Electronic Frontier Foundation" . "https://www.eff.org")
     ("IEEE Computer Society" . "https://www.computer.org")
     ("Risks Digest" . webjump-to-risks)
     ("Supplemental Web site list for webjump"
      . "https://www.neilvandyke.org/webjump/")))
  (setq browse-url-browser-function 'eww-browse-url
        shr-use-colors nil
        eww-header-line-format ""
        nov-header-line-format ""
        shr-bullet "• "
        shr-folding-mode t
        shr-use-fonts nil
        shr-inhibit-images t
        shr-width 80
        eww-search-prefix nil
        url-privacy-level 'none
        eww-auto-rename-buffer 'url
        eww-prompt-history '(
                             "http://c.doc:3001/" ; "https://en.cppreference.com/w/c"
                             "http://cpp.doc:3002/" ; "https://en.cppreference.com/w/cpp"
                             "http://linux.doc:3000/" ;"https://www.kernel.org/doc/html/latest/"
                             ;; C-h I "https://www.gnu.org/software/emacs/manual/"
                             ))
  (defun my-eww-edit-url ()
    "Edit the current EWW URL and reload the page."
    (interactive)
    (unless (derived-mode-p 'eww-mode)
      (user-error "Not in EWW buffer"))
    (let ((current-url (plist-get eww-data :url)))
      (setq eww-data (plist-put eww-data :url
                                (read-string "Edit URL: " current-url)))
      (eww-reload)))
  (add-hook 'eww-after-render-hook 'eww-readable)
  :bind (:map eww-mode-map
              ("e" . my-eww-edit-url)))
(defun extract-base-urls ()
  "Extract base URLs from the current buffer containing text file links."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((urls '()))
      (while (re-search-forward "https?://\\([^:/]+\\)" nil t)
        (push (match-string 0) urls))
      (with-output-to-temp-buffer "*Base URLs*"
        (dolist (url (delete-dups (nreverse urls)))
          (princ url)
          (princ "\n"))))))
(defun url-to-hosts-line ()
  "Convert a URL on the current line to '    0.0.0.0 hostname' format."
  (interactive)
  (let* ((line (thing-at-point 'line t))
         (url (when line
                (string-match "https?://\\([^/]+\\)" line)
                (match-string 0 line)))
         (hostname (when url
                     (replace-regexp-in-string
                      "^https?://\\([^/]+\\).*" "\\1" url))))
    (when hostname
      (beginning-of-line)
      (kill-line)
      (insert (concat "0.0.0.0 " hostname))
      (beginning-of-line))))
;; PEN 笔
;; 1. <tab>
;;    - double: <enter>
;;    - long: C-M-i
;; 2. <prior>
;;    - long: <esc>/<f5>
;; 3. <next>
;;    - long: <b>
;; 4. laser
(keymap-global-set "ESC <next>" 'switchepubinfo)
(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :bind (:map nov-mode-map
              ("p" . 'scroll-down-command)
              ("n" . 'scroll-up-command)
              ("ESC <prior>" . (lambda () (interactive) (bookmark-set "epub")))
              ("C-M-i" .              (lambda () (interactive) (bookmark-jump "epub")))
              ("<prior>" . nov-scroll-down)
              ("<next>" . nov-scroll-up)))
(use-package pdf-tools
  :ensure t
  :defer t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :init (pdf-loader-install)
  :commands (pdf-view-mode)
  :bind (:map pdf-view-mode-map
              ("n" . pdf-view-next-page)
              ("p" . pdf-view-previous-page)
              ("C-M-i" . donothing)
              ("RET" . donothing)
              ("TAB" . donothing)
              ("M-v" . pdf-view-scroll-down-or-previous-page)
              ("C-v" . pdf-view-scroll-up-or-next-page)
              ("<prior>" . pdf-view-previous-page)
              ("<next>" . pdf-view-next-page))
  :config (add-to-list 'revert-without-query ".pdf"))
(use-package info
  :bind (:map Info-mode-map
              ("<mouse-8>" . scroll-up-record)
              ("<mouse-9>" . scroll-down-record)
              ("C-M-i" . Info-history-back)
              ("<prior>" . scroll-down-record)
              ("<next>" . scroll-up-record)
              ("b" . Info-next-preorder)))
(setq enable-dir-local-variables nil)
(setq shell-file-name "fish")
(setq vterm-shell "fish")
;; (setq shell-command-switch "-ic")
(setq shell-command-switch "-c")
(set-buffer-file-coding-system 'utf-8-unix)
(setq face-font-rescale-alist '(("Source Han" . 0.9)))
(defvar my-alternate-font "-DAMA-UbuntuMono Nerd Font-regular-normal-normal-*-13-*-*-*-m-0-iso10646-1")
(defvar my-default-font "bookerly")
(defvar fontfont 0)
(defun my-toggle-font ()
  "Toggle between UbuntuMono and bookerly fonts."
  (interactive)
  (if (= fontfont 1)
      (progn (set-face-attribute 'default nil :font my-default-font :height 150) (setq fontfont 0))
    (progn (set-face-attribute 'default nil :font my-alternate-font :height 200) (setq fontfont 1))))
(set-face-attribute 'default nil :font my-default-font)
(defun my/nix-store-shorten-paths ()
  "Replace long /nix/store paths with shortened ...-pkg-version."
  (let ((inhibit-read-only t))
    (goto-char (point-min))
    (while (re-search-forward
            "/nix/store/[a-z0-9]+-\\([^[:space:]]+\\)" nil t)
      (replace-match "...-\\1" t nil))))
(defun my/compilation-filter-hook ()
  (my/nix-store-shorten-paths))
(add-hook 'compilation-filter-hook #'my/compilation-filter-hook)
(defun hibernatecall()
  (interactive)
  (about-emacs)
  (call-process "systemctl" nil nil nil "hibernate"))
(defvar justonebookonetimelessismore "index.org")
(setq alert-default-style 'libnotify)
(defun xah-clean-whitespace ()
  (interactive)
  (let (xbegin xend)
    (if (region-active-p)
        (setq xbegin (region-beginning) xend (region-end))
      (setq xbegin (point-min) xend (point-max)))
    (save-excursion
      (save-restriction
        (narrow-to-region xbegin xend)
        (goto-char (point-min))
        (while (re-search-forward "[ \t]+\n" nil 1) (replace-match "\n"))
        (goto-char (point-min))
        (while (re-search-forward "\n\n\n+" nil 1) (replace-match "\n\n"))
        (goto-char (point-max))
        (while (eq (char-before) 32) (delete-char -1)))))
  (message "%s done" real-this-command))
(defun donothing () (interactive)(message ""))
(use-package pyim
  :ensure t
  :custom
  (default-input-method "pyim")
  :config
  (cl-defmethod pyim-page-info-format ((_style (eql minibuffer)) page-info)
    (string-replace ")" ""
                    (string-replace "(" "" (format "%s %s"
                                                   ;; (if (plist-get page-info :assistant-enable) " P|" "")
                                                   (if (plist-get page-info :assistant-enable)  (plist-get page-info :candidates) "")
                                                   (if (plist-get page-info :assistant-enable)  (plist-get page-info :current-page) "")))))
  (setq pyim-indicator-list (list #'my-pyim-indicator-with-cursor-color #'pyim-indicator-with-modeline))
  (setq pyim-english-input-switch-functions nil)
  (defun hd()
    "Show hmdz for the word at point."
    (interactive)
    (let ((char (char-after)))
      (if char
          (search-hmdz (char-to-string  char)))))
  (defun search-hmdz(char)
    "search hmdz for the input"
    (interactive "p\ncChar: ")
    (let ((old (current-buffer))
          (exsist 0))
      (save-excursion
        (find-file "~/.config/hmdz.pyim")
        (beginning-of-buffer)
        (search-forward char nil (setq exsist 1))
        (when (= exsist 1)
          (search-backward "/")
          (right-char)
          (message "%s" (string-trim (current-word) "hmdz/")))
        (when (= exsist 0)
          (message "no such shit"))
        (kill-buffer)
        (switch-to-buffer old))))
  (pyim-scheme-add
   '(hmdz
     :document "虎码单字"
     :class xingma
     :code-prefix "hmdz/"
     :first-chars "abcdefghijklmnopqrstuvwxyz"
     :rest-chars "abcdefghijklmnopqrstuvwxyz"
     :code-prefix-history ("_")
     :code-split-length 4
     :code-maximum-length 4))
  (pyim-default-scheme 'hmdz)
  (setq pyim-process-autoselector nil)
  (setq pyim-dhook-verbose nil)
  (setq pyim-dicts nil)  ; Initialize the list if it's not already defined
  (setq pyim-cloudim nil)
  (setq pyim-candidates-search-buffer-p nil)
  (setq pyim-enable-shortcode nil)
  (setq pyim-punctuation-dict '(("^" "…")("\\" "、")("." "。")("," "，")("'" "‘" "’") ("\"" "“" "”")))
  (add-to-list 'pyim-dicts '(:name "hmdz" :file "~/.config/hmdz.pyim")))
(add-hook 'shell-mode-hook  'with-editor-export-editor)
(add-hook 'eshell-mode-hook 'with-editor-export-editor)
(add-hook 'term-exec-hook   'with-editor-export-editor)
(add-hook 'vterm-mode-hook  #'with-editor-export-editor)
;; (remove-hook 'before-save-hook #'xah-clean-whitespace)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(tooltip-mode -1)(delete-selection-mode 1)
(global-font-lock-mode 1)(global-hide-mode-line-mode 1)(global-eldoc-mode -1)
(show-paren-mode -1)(window-divider-mode -1)(winner-mode -1)
(repeat-mode -1)(display-time-mode -1)(display-line-numbers-mode -1)
(use-package face-remap :config (defun text-scale-adjust (inc) (interactive "p") (let ((ev last-command-event) (echo-keystrokes nil) (message-log-max nil)) (let* ((base (event-basic-type ev)) (step (pcase base ((or ?+ ?=) inc) (?- (- inc)) (?0 0) (_ inc)))) (text-scale-increase step) (set-transient-map (let ((map (make-sparse-keymap))) (dolist (mods '(() (control))) (dolist (key '(?+ ?= ?- ?0)) (define-key map (vector (append mods (list key))) (lambda () (interactive) (text-scale-adjust (abs inc)))))) map) nil nil nil)))))
(use-package easysession
  :commands (easysession-switch-to
             easysession-save-as
             easysession-save-mode
             easysession-load-including-geometry)
  :bind
(("C-c sl" . easysession-switch-to)
("C-c ss" . easysession-save-as) )
  :custom
  (easysession-mode-line-misc-info t)
  (easysession-save-interval (* 10 60))
  :init
  (add-hook 'emacs-startup-hook #'easysession-load-including-geometry 102)
  (add-hook 'emacs-startup-hook #'easysession-save-mode 103))
(setq savehist-file (expand-file-name "history" user-emacs-directory))
;; (defun my-set-fringe-face ()
;;   "auto hide fringe face depending on major mode."
;;   (if (derived-mode-p '(occur-mode gud-mode))
;;       (set-face-attribute 'fringe nil
;;                           :background (face-attribute 'default :background)
;;                           :foreground (face-attribute 'default :foreground))
;;     (set-face-attribute 'fringe nil
;;                         :background (face-attribute 'default :background)
;;                         :foreground (face-attribute 'default :background))))
;; (add-hook 'after-change-major-mode-hook #'my-set-fringe-face)
(defun fancyhelloworld()
  (interactive)
  (animate-string "// -*- compile-command: \"gcc -Wall -o main main.c;./main\" -*-" 0 1)
  (sit-for 0.5)
  (animate-string "#include <stdio.h>" 1 1)
  (sit-for 0.5)
  (animate-string "int main(int argc, char *argv[])" 3 1)
  (sit-for 0.5)
  (animate-string "{" 4 1)
  (sit-for 0.5)
  (animate-string "    printf(\"hello, world\");" 5 1)
  (sit-for 0.5)
  (animate-string "    return 0;" 6 1)
  (sit-for 0.5)
  (animate-string "}" 7 1)
  (sit-for 0.5)
  (save-buffer)
  (revert-buffer)
  (compile "gcc -Wall -o main main.c;./main"))
(defun addheader()
  (interactive)
  (save-excursion
    (goto-line 1)
    (newline)
    (previous-line)
    (insert
     (format "// -*- compile-command: \"gcc -Wall -o %s %s;./%s\" -*-"
             (c-get-current-file)
             (concat (c-get-current-file) ".c")
             (c-get-current-file)))(save-buffer)(revert-buffer)))
;; stop addiction of configing emacs here.
(defun list-packages()(interactive)(animate-birthday-present "别上瘾折腾emacs了，写点代码吧"))
(defun org-agenda()(interactive)(animate-birthday-present "别上瘾折腾emacs了，写点代码吧"))
(defun package-list-packages()(interactive)(animate-birthday-present "别上瘾折腾emacs了，写点代码吧"))
(defun package-show-package-list()(interactive)(animate-birthday-present "别上瘾折腾emacs了，写点代码吧"))
(defun customize-create-theme()(interactive)(animate-birthday-present "别上瘾折腾emacs了，写点代码吧"))
(defun customize-themes()(interactive)(animate-birthday-present "别上瘾折腾emacs了，写点代码吧"))
(defun gomoku()(interactive)(animate-birthday-present "别上瘾折腾emacs了，写点代码吧"))
(defun calendar()(interactive)(animate-birthday-present "别上瘾折腾emacs了，写点代码吧"))
(use-package vterm :ensure t :defer t
  :commands (vterm--internal)
  :bind (:map vterm-mode-map
              ("C-p" . vterm-copy-mode)
              :map vterm-copy-mode-map
              ("C-p" . vterm-previous-prompt )
              ("C-f" . vterm-next-prompt )
              ("C-<return>" . compile-goto-error))
  :config
  (setq vterm-always-compile-module t)
  (setq vterm-timer-delay 0.01)
  (with-eval-after-load 'vterm
    (setq vterm-kill-buffer-on-exit t)
    (advice-add 'vterm :after
                (lambda (buf)
                  (with-current-buffer buf
                    (set-process-query-on-exit-flag
                     (get-buffer-process (current-buffer)) nil))))))
;; (add-hook 'vterm-mode-hook (lambda () (compilation-shell-minor-mode 1) (define-key vterm-copy-mode-map (kbd "C-<return>") 'compile-goto-error))
(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("elegantbook"
                 "\\documentclass[lang=cn,math=cm,10pt,scheme=chinese,toc=twocol,bibend=bibtex]{elegantbook}"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))
(defun newday()
  (interactive)
  (switch-to-buffer "*New Day, Same Wish*")
  (erase-buffer)
  (insert
   "- 眼不见生物本能与工业革命驱动的越发泛滥的瘾品失范世界，心为净极简自然身心|黑白断网编程
  远离这几点：随机、即时、不必、匿名、免费、易得、广告、失范
  做到这几点：多做事、乐观心、吃饭慢、心跳缓、有手劲、走路快、体重适、无三高、
  衣着素、清淡食、租房住、乡间息、温度低、睡眠香、独处思、归自然、无名言、无社媒
- 空腹光弱放松|整理计划|静暗凉累|精力充沛|恢复身体|提升大脑|增强免疫|调节激素|健康长寿
- 避免受伤|养成运动习惯|兼顾力量有氧平衡柔韧|全天保持活动
  走路|跑步|抡锤|俯卧撑|引体向上
  摇摆|高拉|相扑|深蹲|拉起|侧拉|划船|起立|绕头|绕身|绕腿|风车
  拉举|抓举|挺举|弯举|军推|实心推|借力推|单双手|单流水|双抓举|农夫行走
- 少糖盐油脂加工食品，多绿豆果菜自然有机
  [大豆|氢化|玉米|芥花]油|反式脂肪
  [口香糖|高果糖|苏氯胺|阿斯巴甜]糖
  [油炸|垃圾|高度加工]食品|面[包|条|饼]|奶酒咖
  番茄|红薯|菜花|香菇|小萝卜|鹰嘴豆|牛油果|夏威夷果
  肌酸|苹果醋|坚果奶|红曲米|发酵食品|胶原蛋白粉|墨西哥辣椒粉
  蓝莓|葡萄|柠檬|香菜|大蒜|孜然|菊粉|可可粉|鱼油|特级初榨橄榄油
- emacs = Evenings, Mornings, And a Couple of Saturdays
  不随机(c-h/info，全部文档代码给你，你不服就改)
  不即时(忍住上古体验需是延迟满足高手，编辑器挑人)
  生存感(人生不过吃睡动和一颗极简又折腾的emacs心)
  不匿名(写包/做实名贡献，代码为万人所用，成就感拉满)
  不免费(自由不是免费，自由无私人文精神残存无限计算世界)
  不易得(当今难得的有难度还有生活具体用处的编程素养积累感)
  无广告(完全可控纯文本，空无至虚感宇宙计算禅意与人类崇高理想)
  不失范(极具宗教感，去传教需冥想自省、禁欲克己来数十年如一日修道)
- 刷leetgo|写project，无色无味不闻不问无欲无求禁游戏戒手机卸浏览器罢搜索恨视频
  通过阅读lfs/lkd等相关英文文档和kernel最佳编程实践
  在emacs中使用[c++|c|zig|rust|makefile|bash]-mode和compilation|magit开发:
  基于多种SOC的linux|rtos的spi、can、wifi、audio、video相关驱动程序和
  有良好的低功耗设计和稳定性优化的高性能|高并发|多线程|多进程|socket网络程序
  然后在qemu|docker|k8s|nix环境中使用perf|ftrace|gprof|gdb工具调试程序
  真实的技术哲学是亲身学会技术底层、真正的人生智慧是用技术找工作然后回到生活远离技术
  生活是吃饭、睡觉、读书、编程、走路、壶铃，生命是健康、乐观、会意、精进、闲适、力量
- While the world is tremendously large, the items are anomalously rich,
  only me writting with leere feeling, ture happiness come from nothing but
  within.")
  (beginning-of-buffer)
  (sit-for 1)
  (org-mode)
  (view-mode))
(defvar my/leetcode-root "~/Leere/Leetcode/src/"
  "Root directory of leetgo-generated problems.")
(defun my/leetcode-format-number (n)
  "Return N formatted like 1 → \"0001\"."
  (format "%04d" n))
(defun my/leetcode--find-problem-dir (n)
  "Return directory path for problem number N."
  (let* ((prefix (my/leetcode-format-number n))
         (dirs (directory-files my/leetcode-root t
                                (concat "^" prefix "\\."))))
    (car dirs)))
(defun my/leetcode--ensure-compile-header (cpp-file)
  "Ensure compile-command header exists exactly once in CPP-FILE."
  (when (file-exists-p cpp-file)
    (with-temp-buffer
      (insert-file-contents cpp-file)
      (goto-char (point-min))
      ;; If ANY occurrence exists, do nothing
      (if (re-search-forward "-*- compile-command:" nil t)
          nil
        ;; Otherwise insert at beginning
        (goto-char (point-min))
        (kill-whole-line)
        (kill-whole-line)
        (insert "// -*- compile-command: \"make -f ../Makefile submit\" -*-\n"))
      ;; Save back
      (write-region (point-min) (point-max) cpp-file nil 'quiet))))
(defun my/leetcode--pandoc-md-to-org (md-file org-file)
  "Convert MD-FILE → ORG-FILE using pandoc."
  (call-process "pandoc" nil nil nil md-file "-o" org-file "--wrap=none"))
(defun my/leetcode--download-image (url dest)
  "Download image URL to DEST file path."
  (url-copy-file url dest t))
(defun my/leetcode--process-org-images (org-file problem-dir)
  "Download remote images using wget and replace links with local filenames."
  (with-temp-buffer
    (insert-file-contents org-file)
    (goto-char (point-min))
    ;; 匹配所有 http 图片链接
    (while (re-search-forward "\\[\\[\\(https?://[^]]+\\.\\(png\\|jpg\\|jpeg\\|gif\\)\\)\\]\\]" nil t)
      (let* ((url (match-string 1))
             (filename (file-name-nondirectory url))  ;; 保留原始图片名
             (local-path (expand-file-name filename problem-dir)))
        ;; 如果图片不存在 -> 用 wget 下载
        (unless (file-exists-p local-path)
          (message "Downloading image via wget: %s" url)
          (call-process "wget" nil nil nil "-q" "-O" local-path url))
        ;; 替换 org 链接为相对路径
        (replace-match (format "[[./%s]]" filename) t t)))
    (write-region (point-min) (point-max) org-file nil 'quiet)))
(defun my/leetcode-open (n)
  "Open Leetcode problem N. If not found, auto-fetch using `leetgo pick -l cpp N`."
  (interactive "nProblem number: ")
  (let* ((dir (my/leetcode--find-problem-dir n)))
    (unless dir
      (message "Problem %d not found. Fetching via `leetgo pick -l cpp %d`..." n n)
      (let ((default-directory (expand-file-name "../" my/leetcode-root)))
        (call-process "leetgo" nil "*leetgo-pick*" t
                      "pick" "-l" "cpp" (number-to-string n)))
      (setq dir (my/leetcode--find-problem-dir n))
      (unless dir
        (error "After running `leetgo pick`, problem %d still not found." n)))
    (let* ((md (expand-file-name "question.md" dir))
           (org (expand-file-name "question.org" dir))
           (cpp (expand-file-name "solution.cpp" dir)))
      ;; 1. Convert MD → ORG
      (my/leetcode--pandoc-md-to-org md org)
      ;; 2. Replace remote images → local
      (my/leetcode--process-org-images org dir)
      ;; 3. Add include block to org (idempotent optional)
      (with-temp-buffer
        (insert-file-contents org)
        (goto-char (point-max))
        (insert "\n\\newpage \n *题解如下:* \n#+INCLUDE: \"./solution.cpp\" src C++ \n \\newpage")
        (write-region (point-min) (point-max) org nil 'quiet))
      ;; 4. Fix: add compile header exactly once
      (my/leetcode--ensure-compile-header cpp)
      ;; 5. Open buffers
      (find-file org)
      (save-window-excursion
        (find-file cpp))
      (message "Loaded LeetCode %d from %s" n dir))))
(defun my/leetcode-generate-includes ()
  "Generate #+INCLUDE lines for all LeetCode question org files."
  (let* ((root (expand-file-name "src" default-directory))
         (dirs (directory-files root t "^[0-9]+\\..+"))
         results)
    (dolist (d dirs (nreverse results))
      (let ((q (expand-file-name "question.org" d)))
        (when (file-exists-p q)
          (push (format "#+INCLUDE: \"%s\" :minlevel 1"
                        (file-relative-name q default-directory))
                results))))))
(defun my/leetcode-fetch-one-silent (n)
  "Fetch problem N silently (no opening buffers), generating question.org and downloading images."
  (let* ((dir (my/leetcode--find-problem-dir n)))
    ;; pick if missing
    (unless dir
      (message "Fetching %d via leetgo pick ..." n)
      (let ((default-directory (expand-file-name "../" my/leetcode-root)))
        (call-process "leetgo" nil nil nil
                      "pick" "-l" "cpp" (number-to-string n)))
      (setq dir (my/leetcode--find-problem-dir n)))
    (unless dir
      (message "Failed to fetch problem %d" n)
      (cl-return-from my/leetcode-fetch-one-silent nil))
    (let* ((md (expand-file-name "question.md" dir))
           (org (expand-file-name "question.org" dir))
           (cpp (expand-file-name "solution.cpp" dir)))
      ;; convert md → org
      (my/leetcode--pandoc-md-to-org md org)
      (my/leetcode--process-org-images org dir)
      ;; include solution block
      (with-temp-buffer
        (insert-file-contents org)
        (goto-char (point-max))
        (insert "\n\\newpage \n *题解如下:* \n#+INCLUDE: \"./solution.cpp\" src C++ \n \\newpage")
        (write-region (point-min) (point-max) org nil 'quiet))
      ;; FIX: ensure compile header only once
      (my/leetcode--ensure-compile-header cpp))
    (message "Fetched %d OK" n)))
(defun my/leetcode-fetch-batch (numbers)
  "Fetch multiple problems silently.
NUMBERS is a string like \"1 2 3 11 17 19\"."
  (interactive "sProblem numbers (e.g. \"1 2 3 11\"): ")
  (let ((nums (mapcar #'string-to-number (split-string numbers))))
    (dolist (n nums)
      (ignore-errors
        (my/leetcode-fetch-one-silent n))))
  (message "Batch fetch done."))
(defun my/leetcode-update-includes ()
  "Find marker '# begin of leetcode include', erase old includes, insert new ones."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    ;; 找到标记行
    (if (search-forward "# begin of leetcode include" nil t)
        (progn
          ;; 到下一行（开始删除旧的内容）
          (forward-line 1)
          ;; 记录起点
          (let ((start (point)))
            ;; 删除所有旧的 #+INCLUDE 行
            (while (looking-at "^#\\+INCLUDE:")
              (forward-line 1))
            (delete-region start (point))
            ;; 生成新的 include 内容
            (let* ((root (expand-file-name "src" default-directory))
                   (dirs (directory-files root t "^[0-9]+\\..+")))
              (dolist (d dirs)
                (let ((q (expand-file-name "question.org" d)))
                  (when (file-exists-p q)
                    (insert
                     (format "#+INCLUDE: \"%s\" :minlevel 2\n"
                             (file-relative-name q default-directory)))))))))
      (message "Marker '# begin of leetcode include' not found!"))))
(global-set-key (kbd "M-+") 'shift-number-up)
(global-set-key (kbd "M-_") 'shift-number-down)
(use-package xref :config
  (defun xref-go-back ()
    "Go back to the previous position in xref history.
To undo, use \\[xref-go-forward]."
    (interactive)
    (let ((history (xref--get-history)))
      (if (null (car history))
          (progn (previous-buffer) (user-error "At start of xref history, back to previous buffer"))
        (let ((marker (pop (car history))))
          (xref--push-forward (point-marker))
          (switch-to-buffer (or (marker-buffer marker)
                                (user-error "The marked buffer has been deleted")))
          (goto-char (marker-position marker))
          (set-marker marker nil nil)
          (run-hooks 'xref-after-return-hook))))))
(defun my/delete-line-and-append-to-hhh ()
  "Delete the current line and append it to a file named 'hhh' in the current directory."
  (interactive)
  (let* ((current-file (buffer-file-name))
         (dir (if current-file
                  (file-name-directory current-file)
                default-directory))     ;; fallback when buffer has no file
         (target-file (expand-file-name "hhh" dir))
         (line (thing-at-point 'line t)))
    ;; Append line to file
    (with-temp-buffer
      (insert line)
      (write-region (point-min) (point-max) target-file t))
    ;; Delete the current line in original buffer
    (delete-region (line-beginning-position)
                   (line-beginning-position 2))))
(defun my/delete-all-duplicate-lines ()
  "Delete all lines in the buffer that appear more than once."
  (interactive)
  (let ((seen (make-hash-table :test 'equal))
        (dups (make-hash-table :test 'equal)))
    ;; 第一次遍历：找重复行
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (string-trim-right
                     (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position)))))
          (if (gethash line seen)
              (puthash line t dups)
            (puthash line t seen)))
        (forward-line 1)))
    ;; 第二次遍历：删掉所有出现过多次的行
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((beg (line-beginning-position))
               (end (line-end-position))
               (line (string-trim-right
                      (buffer-substring-no-properties beg end))))
          (if (gethash line dups)
              (delete-region beg (1+ end)) ;; 删除整行
            (forward-line 1)))))))
(use-package forge
  :after magit)
(defun toggle-solution-question ()
  "Toggle between solution.cpp and question.org in the same directory."
  (interactive)
  (let* ((current-file (buffer-file-name))
         (dir (file-name-directory current-file))
         (target-file (cond
                       ((string-match "solution\\.cpp$" current-file)
                        (expand-file-name "question.org" dir))
                       ((string-match "question\\.org$" current-file)
                        (expand-file-name "solution.cpp" dir))
                       (t nil))))
    (if target-file
        (if (file-exists-p target-file)
            (find-file target-file)
          (message "Target file %s does not exist" target-file))
      (message "Not in solution.cpp or question.org"))))
(use-package cc-mode
  :mode (("\\.\\(cc\\|hh\\)\\'" . c++-mode)
         ("\\.[ch]\\(pp\\|xx\\|\\+\\+\\)\\'" . c++-mode)
         ("\\.\\(CC?\\|HH?\\)\\'" . c++-mode)
         ("\\.[ch]\\'" . c-mode)
         ("\\.y\\(acc\\)?\\'" . c-mode)
         ("\\.lex\\'" . c-mode)
         ("\\.i\\'" . c-mode)
         ("\\.ii\\'" . c++-mode))
  :config
  (advice-add 'c-update-modeline :override #'ignore)
  (defun c-compile-current-file ()
    (interactive)
    (unless (file-exists-p "Makefile")
      (let ((file (file-name-nondirectory buffer-file-name)))
        (compile (format "%s -o %s.out %s %s %s"
                         (or (getenv "CC") "gcc")
                         (file-name-sans-extension file)
                         (or (getenv "CPPFLAGS") "")
                         (or (getenv "CFLAGS") "-Wall -g")
                         file)))))
  (dolist (hook '(c-mode-hook c++-mode-hook asm-mode-hook))
    (add-hook hook (lambda ()
                     (define-key c-mode-base-map (kbd "C-c C-c") 'c-compile-current-file)
                     (define-key c-mode-base-map (kbd "C-c C-M-c") (lambda () (interactive)
                                                                     (setq-local compilation-read-command nil)
                                                                     (call-interactively 'compile)))
                     (define-key c-mode-base-map (kbd "C-M-q") nil)
                     (define-key c-mode-base-map (kbd "(") nil)
                     (define-key c-mode-base-map (kbd "{") nil)))))
(setq confirm-kill-processes nil)
(defun thunar-open-default-directory ()
  (interactive)
  (let ((curr-dir (if-let* ((curr-line (dired-get-filename nil t)))
                      (file-name-directory curr-line)
                    default-directory)))
    (start-process-shell-command
     "thunar" "*thunar*"
     (concat "thunar " curr-dir))))
(defun my/get-project-vterm-buffer ()
  "Return the project vterm buffer, or nil if not exists."
  (let* ((proj (project-current t))
         (root (directory-file-name (project-root proj)))
         (pattern (format "\\*vterminal - %s" (regexp-quote root))))
    (catch 'found
      (dolist (buf (buffer-list))
        (when (string-match pattern (buffer-name buf))
          (throw 'found buf)))
      nil)))
(defun my/ensure-project-vterm ()
  "Ensure project vterm exists."
  (let ((buf (my/get-project-vterm-buffer)))
    (unless buf
      (multi-vterm-project)
      (setq buf (my/get-project-vterm-buffer)))
    buf))
(defun my/send-to-vterm ()
  "Send active region or current line to project vterm without touching kill-ring."
  (interactive)
  (let* ((text (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
         (vbuf (my/ensure-project-vterm)))
    (unless vbuf (user-error "No project vterm buffer"))
    (let ((proc (get-buffer-process vbuf)))
      (unless proc (user-error "No process for vterm buffer"))
      (process-send-string proc (concat text "\n")))))
(defalias 'll 'list-processes)
(setq epg-pinentry-mode 'loopback)      ;使用minibuffer输入密码
(defvar wc-regexp-chinese-char-and-punc
  (rx (category chinese)))
(defvar wc-regexp-chinese-punc
  "[。，！？；：「」『』（）、【】《》〈〉※—]")
(defvar wc-regexp-english-word
  "[a-zA-Z0-9-]+")
;;;###autoload
(defun swint-count-words-region ()
  (interactive)
  (let* ((words-to-be-counted (if mark-active
                                  (buffer-substring-no-properties (region-beginning) (region-end))
                                (buffer-substring-no-properties (point-min) (point-max)))) ;取全文或mark区域
         (v-buffer-string
          (progn
            (if (eq major-mode 'org-mode) ;去掉org文件的OPTIONS
                (setq v-buffer-string (replace-regexp-in-string "^#\\+.+" ""
                                                                words-to-be-counted))
              (setq v-buffer-string words-to-be-counted))
            (replace-regexp-in-string (format "^ *%s *.+" comment-start) "" v-buffer-string))) ;把注释行删掉
         (chinese-char-and-punc 0)
         (chinese-punc 0)
         (english-word 0)
         (chinese-char 0))
    (with-temp-buffer
      (insert v-buffer-string)
      (goto-char (point-min))
      ;; 中文（含標點、片假名）
      (while (re-search-forward wc-regexp-chinese-char-and-punc nil :no-error)
        (setq chinese-char-and-punc (1+ chinese-char-and-punc)))
      ;; 中文標點符號
      (goto-char (point-min))
      (while (re-search-forward wc-regexp-chinese-punc nil :no-error)
        (setq chinese-punc (1+ chinese-punc)))
      ;; 英文字數（不含標點）
      (goto-char (point-min))
      (while (re-search-forward wc-regexp-english-word nil :no-error)
        (setq english-word (1+ english-word))))
    (setq chinese-char (- chinese-char-and-punc chinese-punc))
    (message
     (format "中文字数(不含标点)：%s
中文字数(包含标点)：%s
英文字数(不含标点)：%s
========================
中英文合计(不含标点)：%s
中英文合计(包含标点)：%s"
             chinese-char chinese-char-and-punc english-word
             (+ chinese-char english-word)
             (+ chinese-char-and-punc english-word)))))
(defun foot-default-directory (&optional arg)
  (interactive "P")
  (let ((curr-dir (if-let* ((curr-line (dired-get-filename nil t)))
                      (file-name-directory curr-line)
                    default-directory)))
    (start-process "Foot" nil shell-file-name shell-command-switch
                   (concat "foot  -D" "\"" (expand-file-name curr-dir) "\"" ))))
(defun kill-line-or-region (&optional arg)
  "Kill the current line, or current text selection."
  (interactive "p")
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (if arg
        (kill-region (line-beginning-position) (line-beginning-position (+ arg 1)))
      (kill-region (line-beginning-position) (line-beginning-position 2)))))
(defun toggle-letter-case ()
  "Toggle the letter case of current word or text selection.
Toggles between: “all lower”, “Init Caps”, “ALL CAPS”."
  (interactive)
  (let (p1 p2 (deactivate-mark nil) (case-fold-search nil))
    (if (region-active-p)
        (setq p1 (region-beginning) p2 (region-end))
      (let ((bds (bounds-of-thing-at-point 'word)))
        (setq p1 (car bds) p2 (cdr bds))))
    (when (not (eq last-command this-command))
      (save-excursion
        (goto-char p1)
        (cond
         ((looking-at "[[:lower:]][[:lower:]]") (put this-command 'state "all lower"))
         ((looking-at "[[:upper:]][[:upper:]]") (put this-command 'state "all caps"))
         ((looking-at "[[:upper:]][[:lower:]]") (put this-command 'state "init caps"))
         ((looking-at "[[:lower:]]") (put this-command 'state "all lower"))
         ((looking-at "[[:upper:]]") (put this-command 'state "all caps"))
         (t (put this-command 'state "all lower")))))
    (cond
     ((string= "all lower" (get this-command 'state))
      (upcase-initials-region p1 p2) (put this-command 'state "init caps"))
     ((string= "init caps" (get this-command 'state))
      (upcase-region p1 p2) (put this-command 'state "all caps"))
     ((string= "all caps" (get this-command 'state))
      (downcase-region p1 p2) (put this-command 'state "all lower")))))
(defun rotate-windows ()
  "Rotate your windows."
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))
                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))
                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1 b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))
(use-package doc-view
  :defer t
  :config
  (setq doc-view-continuous t)
  :bind (:map doc-view-mode-map
              ("M-v" . doc-view-scroll-down-or-previous-page)
              ("C-v" . doc-view-scroll-up-or-next-page)
              ("C-p" . (lambda () (interactive) (doc-view-previous-line-or-previous-page 3)))
              ("C-n" . (lambda () (interactive) (doc-view-next-line-or-next-page 3)))))
(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (let ((start (if (region-active-p) (region-beginning) (point-min)))
        (end (if (region-active-p) (region-end) (point-max))))
    (untabify start end)
    (indent-region start end)
    (xah-clean-whitespace)
    (call-interactively 'delete-trailing-whitespace)
    (set-buffer-file-coding-system 'utf-8)))
(defun comment-or-uncomment-region-or-line ()
  "Like comment-or-uncomment-region, but if there's no mark \(that means no
region\) apply comment-or-uncomment to the current line"
  (interactive)
  (if (not mark-active)
      (comment-or-uncomment-region
       (line-beginning-position) (line-end-position))
    (if (< (point) (mark))
        (comment-or-uncomment-region (point) (mark))
      (comment-or-uncomment-region (mark) (point)))))
(defvar monitor-state 1
  "Current monitor state, either 0 for read or  1 for watch.")
(defun monitor ()
  "swtich paperlike-hd monitor from read mode to watch mode using paperlike-cli"
  (interactive)
  (let ((monitorprotocol "-i2c")
        (monitorpath "/dev/i2c-4")
        (monitorcli "paperlike-cli")
        (monitorarg '("-contrast" "-speed" "-mode" "-clear"))
        (mode-state '(("9" "5" "1")  ("9" "5" "1"))))
    (dotimes (number 3)
      (call-process monitorcli nil nil nil
                    monitorprotocol
                    monitorpath
                    (car (nthcdr number monitorarg))
                    (car (nthcdr number (car (nthcdr monitor-state  mode-state)))))
      (sleep-for 1))
    (setq monitor-state  (if (= 0 monitor-state) 1 0 ))
    (sleep-for 1)
    (call-process monitorcli nil nil nil monitorprotocol monitorpath (car (nthcdr 3 monitorarg))))
  (donothing))
(global-smart-shift-mode 1)
(defvar my-last-buffer nil
  "Stores the last buffer before switching.")
(defun my-toggle-buffer ()
  "Toggle between current buffer and last visited buffer."
  (interactive)
  (let ((current (current-buffer)))
    (when (and my-last-buffer
               (buffer-live-p my-last-buffer)
               (not (eq current my-last-buffer)))
      (switch-to-buffer my-last-buffer))
    (setq my-last-buffer current)))
(defun rc/buffer-file-name ()
  (if (equal major-mode 'dired-mode)
      default-directory
    (buffer-file-name)))
(defun rc/parent-directory (path)
  (file-name-directory (directory-file-name path)))
(defun rc/root-anchor (path anchor)
  (cond
   ((string= anchor "") nil)
   ((file-exists-p (concat (file-name-as-directory path) anchor)) path)
   ((string-equal path "/") nil)
   (t (rc/root-anchor (rc/parent-directory path) anchor))))
(defun rc/clipboard-org-mode-file-link (anchor)
  (interactive "sRoot anchor: ")
  (let* ((root-dir (rc/root-anchor default-directory anchor))
         (org-mode-file-link (format "file:%s::%d"
                                     (if root-dir
                                         (file-relative-name (rc/buffer-file-name) root-dir)
                                       (rc/buffer-file-name))
                                     (line-number-at-pos))))
    (kill-new org-mode-file-link)
    (message org-mode-file-link)))
(defun rc/put-file-name-on-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (rc/buffer-file-name)))
    (when filename
      (kill-new filename)
      (message filename))))
(defun rc/put-buffer-name-on-clipboard ()
  "Put the current buffer name on the clipboard"
  (interactive)
  (kill-new (buffer-name))
  (message (buffer-name)))
(setq-default
 c-basic-offset 4
 c-backslash-column 99
 c-backslash-max-column 99
 c-default-style '((java-mode . "java")
                   (awk-mode . "awk")
                   (other . "bsd")))
(add-hook 'c-mode-hook (lambda ()
                         (interactive)
                         (c-toggle-comment-style -1)))
(donothing)                             ; don't annoy me anymore.
;;; new things during last 10days

(defvar rotate 0
  "Current sway rotate state, either 0 for horizental or  1 for vertical.")
(defun swaywindow()
  (interactive)
(let ((direction '("down" "right")))
  (if (= (call-process-shell-command "swaymsg -t get_tree | jq -e '.. | select(.focused? == true and .fullscreen_mode == 1)' >/dev/null;" nil t t) 4)
  (start-process  "swaymsg" nil  "swaymsg" "focus" (car(nthcdr rotate direction)))(progn
    (start-process  "fullscreen1" nil  "swaymsg" "fullscreen" )
    (start-process  "swaymsg" nil  "swaymsg" "focus" (car(nthcdr rotate direction)))
    (start-process  "fullscreen2" nil  "swaymsg" "fullscreen" ))
  )))
(defun swayrotate()
  (interactive)
  (start-process  "fullscreen1" nil  "swaymsg" "layout toggle split" )
  (setq rotate   (if (= 0 rotate) 1 0 )))
(global-set-key (kbd "s-<") 'swaywindow)
(global-set-key (kbd "s->") 'swayrotate)
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
(add-hook 'css-mode-hook #'aggressive-indent-mode)
