;;; post-init.el --- emacs as my Yak-Shaving daily -*- lexical-binding: t; -*-
(require 'use-package)
(use-package saveplace
  :bind (
         ;; 通过Ctrl/Alt/Meta/Shift快捷使用功能
         ;; -------------------------------------------------------------------------------------------------------------
         ("C-f" . undo)
         ("C-b" . undo-redo)
         ("C-p" . goto-last-change)
         ("M-p". goto-last-change-reverse)
         ;; ("C-SPC C-SPC" . set-mark-command)
         ("M-SPC" . consult-mark)
         ;; -------------------------------------------------------------------------------------------------------------
         ("C-<backspace>" . duplicate-dwim)
         ;; ("C-M-<backspace>" . backward-kill-sexp)
         ;; ("C-S-<backspace>" . kill-whole-line)
         ;; ("M-<backspace>" . backward-kill-word)
         ("S-<backspace>" . kill-sexp)
         ;; -------------------------------------------------------------------------------------------------------------
         ("S-<return>" . comment-indent-new-line)
         ("M-<return>" . dabbrev-expand)
         ("C-<return>" . avy-goto-word-0)
         ("C-S-<return>" . avy-copy-line)
         ("C-M-<return>" . avy-goto-line)
         ;; -------------------------------------------------------------------------------------------------------------
         ("M-<tab>" . hippie-expand)
         ;; ("C-<tab>" . surround-mark)          ;; mark括号
         ("<backtab>" . surround-insert)       ;; 加括号
         ("C-<iso-lefttab>" . surround-delete) ;; 删括号
         ("C-M-<tab>" . surround-kill)         ;; 删括号内外(根据左右定内外)
         ("s-<tab>" . surround-change)         ;; 换括号
         ;; -------------------------------------------------------------------------------------------------------------
         ;; C-i   type ;
         ;; C-S-i  type :
         ("M-i"         . comment-or-uncomment-region-or-line)
         ("M-I"         . consult-imenu)
         ("M-g I"       . consult-imenu-multi)
         ;; -------------------------------------------------------------------------------------------------------------
         ("C-v"           . (lambda ()(interactive) (recenter-top-bottom 0)))
         ("M-v"         . (lambda ()(interactive) (recenter-top-bottom 38)))
         ("M-e"         . other-window)
         ("M-E"         . (lambda ()(interactive)  (other-window -1)))
         ;; -------------------------------------------------------------------------------------------------------------
         ("M-*"         . leetcode-open-cursor)
         ("s-*"         . my/leetcode-open)
         ("C-*"         . toggle-solution-question)
         ;; -------------------------------------------------------------------------------------------------------------
         ("C-%"         . iedit-mode)
         ("C-S-c C-S-c" . mc/edit-lines)
         ("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C-%"     . mc/mark-all-like-this)
         ("C-$"         . mc/skip-to-next-like-this)
         ("C-^"         . mc/skip-to-previous-like-this)
         ;; -------------------------------------------------------------------------------------------------------------
         ("<C-m>"    . embark-dwim)
         ("<C-S-m>" . embark-act)
         ("C-h B" . embark-bindings)
         ;; -------------------------------------------------------------------------------------------------------------
         ("M-+". rotate-text)
         ("M-_". rotate-text-backward)
         ;; -------------------------------------------------------------------------------------------------------------

         ;;;; 通过ZMK的NAV/MOS/SYM/NUM层来快捷使用功能
         ;; MOS-begin-----------------------------------------------------------------
         ("C-c C-; d" .  dired)
         ("C-c C-; k" .  kill-current-buffer)
         ("C-c C-; p" . magit-status)
         ("C-c C-; g" . er/expand-region)
         ("C-c C-; f" . consult-bookmark)
         ("C-c C-; m" . compile)
         ("C-c C-; u" . bookmark-set)
         ("C-c C-; h" . woman)
         ("C-c C-; v" . multi-vterm-project)
         ("C-c C-; y" . yas-insert-snippet)
         ("C-c C-; x" . multi-vterm-dedicated-toggle)
         ("C-c C-; q" . quick-sdcv-search-at-point)
         ("C-c C-; c" . devdocs-browser-open)
         ("C-c C-; l" . git-link-dispatch)
         ("C-c C-; j" . magit-log-buffer-file)
         ("C-c C-; w" . show-useful-list)
         ;; MOS-end-----------------------------------------------------------------

         ;; NAV-begin--------------------------------------------------------------
         ("C-c C-~ x" .  delete-other-windows)
         ("C-c C-~ c" .  split-window-below)
         ("C-c C-~ d" .  delete-window)
         ("C-c C-~ g" .  er/expand-region)
         ("C-c C-~ j" .  consult-ripgrep-symbol-at-point)
         ("C-c C-~ l" .   consult-line-symbol-at-point)
         ("C-c C-~ u" . occur)
         ("C-c C-~ y" . toggle-special-buffer)
         ("C-c C-~ k" . switch-to-man)
         ("C-c C-~ v" . switch-to-eww)
         ("C-c C-~ m" . recompile)
         ("C-c C-~ p" . rg-dwim)
         ("C-c C-~ b" . rg-dwim-current-file)
         ;; NAV-end---------------------------------------------------------

         ;; SYM-begin-------------------------------------------------------
         ("C-c C-& l" . my-toggle-font)
         ("C-c C-& j" . global-hide-mode-line-mode)
         ("C-c C-& u" . toggle-vertico)
         ("C-c C-& m" . toggle-truncate-lines)
         ("C-c C-& y" . toggle-book-mode)
         ("C-c C-& h" . git-timemachine-toggle)
         ("C-c C-& k" . swint-count-words-region)
         ("C-c C-& ," . toggle-input-method)
         ("C-c C-& ." . toggle-letter-case)
         ("C-c C-& DEL" . xah-clean-whitespace)
         ;; SYM-end-------------------------------------------------------

         ;; NUM-begin--------------------------------------------
         ("s-m" . switch-to-output)
         ("s-k" . (lambda () (interactive) (project-list-buffers-ibuffer (project-current t))))
         ;; s-h, s-< 由sway使用
         ("s->" . switch-to-code)
         ("s-j" . (lambda ()(interactive)  (find-file (concat myconfigpath "post-init.el"))))
         ("s-\"" . (lambda () (interactive) (find-file (concat myconfigpath "flake.nix"))))
         ("s-u" . previous-buffer)
         ("s-l" . next-buffer)
         ;; NUM-end--------------------------------------------

         ;; Minor-begin--------------------------------------------
         ("M-#" . consult-register-load)
         ("M-$" . consult-register-store)
         ("C-M-#" . consult-register)
         ("M-g g" . consult-goto-line)
         ("<f20>" . rotate-windows)
         ("<f21>" . export-my-book)
         ("<f22>" . record-time)
         ("M-s SPC" . consult-global-mark)
         ("M-s <backspace>" . delete-all-space)
         ("M-s <return>" . comment-kill)
         ("M-s K" . consult-keep-lines)
         ("M-s M-k" . avy-kill-whole-line)
         ("M-s b" . magit-blame-addition)
         ("M-s d" . delete-duplicate-lines)
         ("M-s e" . ediff)
         ("M-s f" . consult-fd)
         ("M-s i" . irc)
         ("M-s k" . delete-this-buffer-and-file)
         ("M-s n" . flush-lines)
         ("M-s o" . buffer-occur)
         ("M-s p" . consult-gh)
         ("M-s r" . trashed)
         ("M-s s" . sort-lines)
         ("M-s t" . check-parens)
         ("M-s u" . url-to-hosts-line)
         ("M-s w" .  search-github-code)
         ("<WakeUp>". wakeupcall) ("ESC <f5>". hibernatecall)
         ;; Minor-end--------------------------------------------

         ;; 为防止左手右手小拇指/食指的过度使用，我禁止了这些键:
         ("C-a" . sayshit) ("M-a" . sayshit) ("M-z" . sayshit) ("C-c C-; z" . sayshit) ("C-c C-~ z" . sayshit) ("C-x C-;" . sayshit)("<end>" . sayshit)("C-S-<right>" . sayshit) ("C-M-S-<right>" . sayshit)("C-/" . sayshit) ("C-?" . sayshit) ("M-/" . sayshit) ("M-o" . sayshit) ("M-O" . sayshit) ("C-x k" . sayshit) ("C-\\" . sayshit) ("C-n " . sayshit) ("M-n". sayshit) ("C-c C-& /" . sayshit) ("C-c C-& '" . sayshit)  ("C-c C-~ '" .  sayshit) ("C-c C-; '" . sayshit) ("C-c C-; /" . sayshit) ;; ("<right>" . sayshit) ("C-<right>" . sayshit) ("C-c C-~ q" .  sayshit)  ("C-c C-; q" . sayshit)
         )
  :ensure nil
  :commands (save-place-mode save-place-local-mode)
  :hook (after-init . save-place-mode)
  :custom
  (save-place-forget-unreadable-files nil)
  (save-place-limit 400)
  (save-place-file
   (expand-file-name "saveplace" user-emacs-directory)))

(use-package recentf
  :ensure nil
  :commands (recentf-mode recentf-cleanup)
  :hook (after-init . recentf-mode)
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
  (defun my/suppress-recentf-messages (orig-fun &rest args)
    "Run `recentf-cleanup` without showing messages in the echo area."
    (let ((inhibit-message t)
          (message-log-max nil))
      (apply orig-fun args)))
  (advice-add 'recentf-cleanup :around #'my/suppress-recentf-messages)
  (add-hook 'kill-emacs-hook #'recentf-cleanup -90))

(use-package autorevert
  :ensure nil
  :commands (auto-revert-mode global-auto-revert-mode)
  :hook (after-init . global-auto-revert-mode)
  :custom
  (auto-revert-interval 3)
  (auto-revert-remote-files nil)
  (auto-revert-use-notify t)
  (auto-revert-avoid-polling nil)
  (auto-revert-verbose nil))

(use-package which-key
  :ensure nil
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

(use-package image-mode
  :ensure nil
  :defer t
  :bind(:map image-mode-map
             ([remap scroll-down-command] . image-previous-file)
             ([remap scroll-up-command] . image-next-file)))

(use-package cc-mode
  :ensure nil
  :defer t
  :mode (("\\.\\(cc\\|hh\\)\\'" . c++-mode)
         ("\\.[ch]\\(pp\\|xx\\|\\+\\+\\)\\'" . c++-mode)
         ("\\.\\(CC?\\|HH?\\)\\'" . c++-mode)
         ("\\.[ch]\\'" . c-mode)
         ("\\.y\\(acc\\)?\\'" . c-mode)
         ("\\.lex\\'" . c-mode)
         ("\\.i\\'" . c-mode)
         ("\\.ii\\'" . c++-mode))
  :init
  (setq-default
   c-basic-offset 8
   c-backslash-column 99
   c-backslash-max-column 99
   c-default-style '((java-mode . "java")
                     (awk-mode . "awk")
                     (other . "bsd")))

  ;; begin of linus monkey typing recommend  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defun c-lineup-arglist-tabs-only (ignored)
    "Line up argument lists by tabs, not spaces"
    (let* ((anchor (c-langelem-pos c-syntactic-element))
           (column (c-langelem-2nd-pos c-syntactic-element))
           (offset (- (1+ column) anchor))
           (steps (floor offset c-basic-offset)))
      (* (max steps 1)
         c-basic-offset)))

  (dir-locals-set-class-variables
   'linux-kernel
   '((c-mode . (
                (c-basic-offset . 8)
                (c-label-minimum-indentation . 0)
                (c-offsets-alist . (
                                    (arglist-close         . c-lineup-arglist-tabs-only)
                                    (arglist-cont-nonempty .
                                                           (c-lineup-gcc-asm-reg c-lineup-arglist-tabs-only))
                                    (arglist-intro         . +)
                                    (brace-list-intro      . +)
                                    (c                     . c-lineup-C-comments)
                                    (case-label            . 0)
                                    (comment-intro         . c-lineup-comment)
                                    (cpp-define-intro      . +)
                                    (cpp-macro             . -1000)
                                    (cpp-macro-cont        . +)
                                    (defun-block-intro     . +)
                                    (else-clause           . 0)
                                    (func-decl-cont        . +)
                                    (inclass               . +)
                                    (inher-cont            . c-lineup-multi-inher)
                                    (knr-argdecl-intro     . 0)
                                    (label                 . -1000)
                                    (statement             . 0)
                                    (statement-block-intro . +)
                                    (statement-case-intro  . +)
                                    (statement-cont        . +)
                                    (substatement          . +)
                                    ))
                (indent-tabs-mode . t)
                (show-trailing-whitespace . t)
                ))))
  (dir-locals-set-directory-class
   (expand-file-name "~/linux-trees")
   'linux-kernel)
  ;; end of monkey typing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (advice-add 'c-update-modeline :override #'ignore)
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
  (dolist (hook '(c-mode-common-hook zig-mode-hook asm-mode-hook))
    (add-hook hook (lambda ()
                     (define-key c-mode-base-map (kbd "M-o") 'cff-find-other-file)
                     (electric-operator-mode)
                     (smart-semicolon-mode)
                     (c-toggle-comment-style -1)
                     ))))

(use-package compile
  :ensure nil
  :custom
  (compilation-window-height nil)
  (compilation-scroll-output nil)
  (compilation-auto-jump-to-first-error nil)
  :config

  (defun my/nix-store-shorten-paths ()
    "Replace long /nix/store paths with shortened ...-pkg-version."
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (while (re-search-forward
              "/nix/store/[a-z0-9]+-\\([^[:space:]]+\\)" nil t)
        (replace-match "...-\\1" t nil)))
    )
  (defun compilation-start (command &optional mode name-function highlight-regexp continue) "Run compilation command COMMAND (low level interface). If COMMAND starts with a cd command, that becomes the `default-directory'. The rest of the arguments are optional; for them, nil means use the default. MODE is the major mode to set in the compilation buffer.  Mode may also be t meaning use `compilation-shell-minor-mode' under `comint-mode'. If NAME-FUNCTION is non-nil, call it with one argument (the mode name) to determine the buffer name.  Otherwise, the default is to reuses the current buffer if it has the proper major mode, else use or create a buffer with name based on the major mode. If HIGHLIGHT-REGEXP is non-nil, `next-error' will temporarily highlight the matching section of the visited source line; the default is to use the global value of `compilation-highlight-regexp'. If CONTINUE is non-nil, the buffer won't be emptied before compilation is started.  This can be useful if you wish to combine the output from several compilation commands in the same buffer.  The new output will be at the end of the buffer, and point is not changed. Returns the compilation buffer created." (or mode (setq mode 'compilation-mode)) (let* ((name-of-mode (if (eq mode t) "compilation" (replace-regexp-in-string "-mode\\'" "" (symbol-name mode)))) (thisdir default-directory) (thisenv compilation-environment) (buffer-path (and (local-variable-p 'exec-path) exec-path)) (buffer-env (and (local-variable-p 'process-environment) process-environment)) outwin outbuf) (with-current-buffer (setq outbuf (get-buffer-create (compilation-buffer-name name-of-mode mode name-function))) (let ((comp-proc (get-buffer-process (current-buffer)))) (if comp-proc (if (or (not (eq (process-status comp-proc) 'run)) (eq (process-query-on-exit-flag comp-proc) nil) (yes-or-no-p (format "A %s process is running; kill it? " name-of-mode))) (condition-case () (progn (interrupt-process comp-proc) (sit-for 1) (delete-process comp-proc)) (error nil)) (error "Cannot have two processes in `%s' at once" (buffer-name))))) (setq default-directory thisdir) (let ((inhibit-read-only t) (default-directory thisdir)) (cd (cond ((not (string-match "\\`\\s *cd\\(?:\\s +\\(\\S +?\\|'[^']*'\\|\"\\(?:[^\"`$\\]\\|\\\\.\\)*\"\\)\\)?\\s *[;&\n]" command)) default-directory) ((not (match-end 1)) "~") ((eq (aref command (match-beginning 1)) ?\') (substring command (1+ (match-beginning 1)) (1- (match-end 1)))) ((eq (aref command (match-beginning 1)) ?\") (replace-regexp-in-string "\\\\\\(.\\)" "\\1" (substring command (1+ (match-beginning 1)) (1- (match-end 1))))) (t (let* ((substituted-dir (substitute-env-vars (match-string 1 command))) (expanded-dir (file-expand-wildcards substituted-dir))) (if (= (length expanded-dir) 1) (car expanded-dir) substituted-dir))))) (if continue (progn (setq continue (point)) (goto-char (point-max))) (erase-buffer)) (if (not (eq mode t)) (progn (buffer-disable-undo) (funcall mode)) (setq buffer-read-only nil) (with-no-warnings (comint-mode)) (compilation-shell-minor-mode)) (setq-local compilation-directory thisdir) (setq-local compilation-environment thisenv) (if buffer-path (setq-local exec-path buffer-path) (kill-local-variable 'exec-path)) (if buffer-env (setq-local process-environment buffer-env) (kill-local-variable 'process-environment)) (if highlight-regexp (setq-local compilation-highlight-regexp highlight-regexp)) (if (or compilation-auto-jump-to-first-error (eq compilation-scroll-output 'first-error)) (setq-local compilation-auto-jump-to-next t))  (compilation-insert-annotation command "\n") (setq compilation--start-time (float-time)) (setq thisdir default-directory)) (set-buffer-modified-p nil)) (setq outwin (display-buffer outbuf '(nil (allow-no-window . t)))) (with-current-buffer outbuf (let ((process-environment (append compilation-environment (and (derived-mode-p 'comint-mode) (comint-term-environment)) (list (format "INSIDE_EMACS=%s,compile" emacs-version)) (list "PAGER=") (copy-sequence process-environment)))) (setq-local compilation-arguments (list command mode name-function highlight-regexp)) (setq-local revert-buffer-function 'compilation-revert-buffer) (when (and outwin (not continue) (not compilation-scroll-output)) (set-window-start outwin (point-min))) (let ((desired-visible-point (cond (continue continue) (compilation-scroll-output (point-max)) (t (point-min))))) (goto-char desired-visible-point) (when (and outwin (not (eq outwin (selected-window)))) (set-window-point outwin desired-visible-point))) (if compilation-process-setup-function (funcall compilation-process-setup-function)) (and outwin (compilation-set-window-height outwin)) (if (fboundp 'make-process) (let ((proc (if (eq mode t) (with-connection-local-variables (get-buffer-process (with-no-warnings (comint-exec outbuf (compilation--downcase-mode-name mode-name) shell-file-name nil `(,shell-command-switch ,command))))) (start-file-process-shell-command (compilation--downcase-mode-name mode-name) outbuf command)))) (setq mode-line-process '((:propertize ":%s" face compilation-mode-line-run) compilation-mode-line-errors)) (when compilation-always-kill (set-process-query-on-exit-flag proc nil)) (set-process-sentinel proc #'compilation-sentinel) (unless (eq mode t) (set-process-filter proc #'compilation-filter)) (set-marker (process-mark proc) (point-max) outbuf) (when compilation-disable-input (condition-case nil (process-send-eof proc) (error nil))) (run-hook-with-args 'compilation-start-hook proc) (compilation--update-in-progress-mode-line) (push proc compilation-in-progress)) (message "Executing `%s'..." command) (setq mode-line-process '((:propertize ":run" face compilation-mode-line-run) compilation-mode-line-errors)) (force-mode-line-update) (sit-for 0) (save-excursion (goto-char (point-max)) (let* ((inhibit-read-only t) (compilation-filter-start (point)) (status (call-process shell-file-name nil outbuf nil "-c" command))) (run-hooks 'compilation-filter-hook) (cond ((numberp status) (compilation-handle-exit 'exit status (if (zerop status) "finished\n" (format "exited abnormally with code %d\n" status)))) ((stringp status) (compilation-handle-exit 'signal status (concat status "\n"))) (t (compilation-handle-exit 'bizarre status status))))) (set-buffer-modified-p nil) (message "Executing `%s'...done" command))) (setq default-directory thisdir) (when compilation-scroll-output (goto-char (point-max)))) (setq next-error-last-buffer outbuf))) (defun compilation-handle-exit (process-status exit-status msg) "Write MSG in the current buffer and hack its `mode-line-process'." (let ((inhibit-read-only t) (status (if compilation-exit-message-function (funcall compilation-exit-message-function process-status exit-status msg) (cons msg exit-status))) (omax (point-max)) (opoint (point)) (cur-buffer (current-buffer))) (goto-char omax) (compilation-insert-annotation ?\n mode-name " " (car status)) (if (and (numberp compilation-window-height) (zerop compilation-window-height)) (message "%s" (cdr status))) (if (bolp) (forward-char -1)) (compilation-insert-annotation ", duration " (let ((elapsed (- (float-time) compilation--start-time))) (cond ((< elapsed 10) (format "%.2f s" elapsed)) ((< elapsed 60) (format "%.1f s" elapsed)) (t (format-seconds "%h:%02m:%02s" elapsed))))) (goto-char (point-max)) (add-text-properties omax (point) (append '(compilation-handle-exit t) nil)) (setq mode-line-process (list (let ((out-string (format ":%s [%s]" process-status (cdr status))) (msg (format "%s %s" mode-name (replace-regexp-in-string "\n?$" "" (car status))))) (message "%s" msg) (propertize out-string 'help-echo msg 'face (if (> exit-status 0) 'compilation-mode-line-fail 'compilation-mode-line-exit))) compilation-mode-line-errors)) (force-mode-line-update) (if (and opoint (< opoint omax)) (goto-char opoint)) (run-hook-with-args 'compilation-finish-functions cur-buffer msg)))
  (with-eval-after-load 'compile
    ;; (add-to-list 'compilation-environment "TERM=dumb-emacs-ansi")
    ;; (remove-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
    ;; (add-hook 'compilation-filter-hook #'my/nix-store-shorten-paths)
    )
  )

(use-package eww
  :ensure nil
  :init
  (setq
   ;; browse-url-browser-function 'browse-url-firefox
   browse-url-browser-function 'eww
   shr-use-colors nil
   eww-header-line-format nil
   eww-form-checkbox-selected-symbol "[X]"
   eww-form-checkbox-symbol "[ ]"
   shr-bullet "- "
   shr-folding-mode t
   shr-use-fonts t
   shr-inhibit-images t
   shr-width nil
   shr-max-image-proportion 0.6
   eww-search-prefix nil
   eww-default-download-directory "~/"
   url-privacy-level 'none
   eww-auto-rename-buffer 'url
   eww-prompt-history '(
                        "https://learnxinyminutes.com/"
                        ;; "https://ziggit.dev/"
                        ;; "http://c.doc:3001/" ; "https://en.cppreference.com/w/c"
                        ;; "http://cpp.doc:3002/" ; "https://en.cppreference.com/w/cpp"
                        ;; "http://linux.doc:3000/" ;"https://www.kernel.org/doc/html/latest/"
                        ;; C-h I "https://www.gnu.org/software/emacs/manual/"
                        ))

  :config
  (defun my-eww-edit-url ()
    "Edit the current EWW URL and reload the page."
    (interactive)
    (unless (derived-mode-p 'eww-mode)
      (user-error "Not in EWW buffer"))
    (let ((current-url (plist-get eww-data :url)))
      (setq eww-data (plist-put eww-data :url
                                (read-string "Edit URL: " current-url)))
      (eww-reload)))
  (defun my-eww-forward-url()   (interactive) (eww-forward-url) (recenter-top-bottom 0))
  (defun my-eww-back-url()   (interactive) (eww-back-url) (recenter-top-bottom 0))
  (defun eww-follow-link (&optional external mouse-event)
    "Browse the URL under point.
    If EXTERNAL is single prefix, browse the URL using
    `browse-url-secondary-browser-function'.
    If EXTERNAL is double prefix, browse in new buffer."
    (interactive
     (list current-prefix-arg last-nonmenu-event)
     eww-mode)
    (mouse-set-point mouse-event)
    (let* ((orig-url (get-text-property (point) 'shr-url))
           (url (eww--transform-url orig-url))
           target)
      (cond
       ((not url)
        (message "No link under point"))
       ((string-match-p eww-use-browse-url url)
        ;; This respects the user options `browse-url-handlers'
        ;; and `browse-url-mailto-function'.
        (browse-url url))
       ((and (consp external) (<= (car external) 4))
        (funcall browse-url-secondary-browser-function url)
        (shr--blink-link))
       ;; This is a #target url in the same page as the current one.
       ((and (setq target (url-target (url-generic-parse-url url)))
             (eww-same-page-p url (plist-get eww-data :url)))
        (let ((point (point)))
          (eww-save-history)
          (eww--before-browse)
          (plist-put eww-data :url url)
          (goto-char (point-min))
          (if-let ((match (text-property-search-forward 'shr-target-id target #'member)))
              (goto-char (prop-match-beginning match))
            (goto-char (if (equal target "top")
                           (point-min)
                         point))))
        (recenter-top-bottom 0))
       (t
        (eww-browse-url orig-url external)))))
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
  ;; (add-hook 'eww-after-render-hook 'eww-readable)
  :bind (:map eww-mode-map
              ("L" . eww-list-bookmarks)
              ("r" . my-eww-forward-url)
              ("l" . my-eww-back-url)
              ("<return>" . eww-follow-link)
              ("C-M-i" . my-eww-back-url)
              ("b" . my-eww-forward-url)
              ("E" . my-eww-edit-url)))

(use-package eglot
  :ensure nil
  :defer t
  :hook (eglot-managed-mode .
                            (lambda ()
                              (eglot-inlay-hints-mode -1)
                              (eldoc-mode -1)
                              (flymake-mode -1)))
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
  :init
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

(use-package org
  :ensure t
  :defer t
  :commands (org-mode org-version)
  :mode
  ("\\.org\\'" . org-mode)
  :custom
  (org-latex-toc-command "\\tableofcontents\n\n \\mainmatter\n\n")
  (org-support-shift-select t)
  (org-latex-compiler "xelatex")
  (org-latex-src-block-backend 'listings)
  (org-export-with-broken-links t)
  (org-export-with-tags nil)
  (org-export-with-drawers nil)
  (org-export-with-toc nil)
  (org-hide-leading-stars nil)
  (org-startup-folded 'showeverything)
  (org-agenda-span 'week)
  (org-log-into-drawer t)
  (org-startup-indented nil)
  (org-adapt-indentation nil)
  (org-edit-src-content-indentation 0)
  (org-startup-truncated t)
  (org-fontify-done-headline nil)
  (org-fontify-todo-headline nil)
  (org-hide-emphasis-markers nil)
  (org-fontify-whole-heading-line nil)
  (org-fontify-quote-and-verse-blocks nil)
  (org-confirm-babel-evaluate nil)
  (org-startup-with-inline-images t)
  (org-link-descriptive nil)
  (org-todo-keywords ;; t要做的，f要修的，e暂时的，a失败的，k有缺陷的，o就这样吧
   '((sequence  "TODO(t)" "DONE(d)" "|" "FIXME(f)")
     (sequence "TEMP(e)" "FAIL(a)" "KLUDGE(k)"    "|" "OKAY(o)")))
  :config
  (with-eval-after-load 'ox-latex
    (add-to-list 'org-latex-classes
                 '("elegantbook"
                   "\\documentclass[lang=cn,math=cm,10pt,scheme=chinese,toc=twocol,bibend=bibtex]{elegantbook}"
                   ("\\chapter{%s}" . "\\chapter*{%s}")
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))
  ;; (with-eval-after-load 'org
  ;;   (define-key org-mode-map (kbd "<f22>")
  ;;               ))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     (emacs-lisp . t)
     (shell . t)
     (python . t))))

(use-package info
  :custom
  (Info-use-header-line nil)
  :bind (:map Info-mode-map
              ([remap scroll-up-command] . Info-scroll-up)
              ([remap scroll-down-command] . Info-scroll-down)
              ("C-M-i" . Info-history-back)
              ("b" . Info-next-preorder)))

(use-package dired
  :ensure nil
  :commands (dired)
  :bind( :map dired-mode-map
         ("SPC" . scroll-up-command)
         ("p" . (lambda () (interactive)(dired-find-file-other-window) (other-window 1)(next-line)))
         ("DEL" . scroll-down-command)
         ("," . dired-omit-mode)
         ("h" . dired-do-eww)
         ("<tab>" . dired-subtree-toggle)
         ("e" . wdired-change-to-wdired-mode))
  :hook ((dired-mode . dired-hide-details-mode) (dired-mode . dired-omit-mode))
  :config
  (defun foot-default-directory (&optional arg)
    (interactive "P")
    (let ((curr-dir (if-let* ((curr-line (dired-get-filename nil t)))
                        (file-name-directory curr-line)
                      default-directory)))
      (start-process "Foot" nil shell-file-name shell-command-switch
                     (concat "foot  -D" "\"" (expand-file-name curr-dir) "\"" ))))
  (defun thunar-open-default-directory ()
    (interactive)
    (let ((curr-dir (if-let* ((curr-line (dired-get-filename nil t)))
                        (file-name-directory curr-line)
                      default-directory)))
      (start-process-shell-command
       "thunar" "*thunar*"
       (concat "thunar " curr-dir))))
  (use-package dired-subtree
    :defer t
    :custom
    (dired-subtree-use-backgrounds nil)
    :after dired)
  (with-eval-after-load 'dired
    (let ((args "--group-directories-first -ahlv"))
      (when (or (eq system-type 'darwin) (eq system-type 'berkeley-unix))
        (if-let* ((gls (executable-find "gls")))
            (setq insert-directory-program gls)
          (setq args nil)))
      (when args
        (setq dired-listing-switches args))))
  :custom
  (dired-movement-style 'bounded-files)
  (delete-by-moving-to-trash t)
  (dired-recursive-deletes 'always)
  (dired-omit-files
   (rx (or (seq bol ".")
           (seq "." (or "css" "cls") eos)
           (seq bol "." (or "svn" "git" "lock") eos)
           (seq bol (or "LICENSE" "LICENCE" "Downloads") eos)
           ))))

(use-package envrc :defer 2 :config (envrc-global-mode 1)
  (advice-add 'org-babel-eval :around #'envrc-propagate-environment))


(use-package psession
  :init
  (let ((nonono (concat user-emacs-directory "elisp-objects/buffer-name-history.elc")))
    (when (file-exists-p nonono) (delete-file nonono)))
  (defconst emacs-start-time (format-time-string "%c"))
  (fset #'display-startup-echo-area-message #'ignore)
  (add-hook 'emacs-startup-hook (lambda () (message "")))
  (add-hook 'temp-buffer-window-show-hook #'window-divider-mode)
  (add-hook 'Info-mode-hook (lambda () (text-scale-set -0.2)))
  (setq-default
   face-font-rescale-alist '(("Source Han Sans". 0.95))
   tab-bar-new-button-show nil
   tab-bar-close-button-show nil
   text-scale-mode-amount 0
   bookmark-fringe-mark nil
   browse-url-firefox-program "firefox-beta"
   calendar-latitude +29.5
   calendar-longitude +106.5
   column-number-mode t
   left-fringe-width  8
   right-fringe-width 1
   myconfigpath "~/.me/"
   comp-async-report-warnings-errors nil
   confirm-kill-processes nil
   cursor-type 'box
   default-major-mode                'fundamental-mode
   delete-by-moving-to-trash t
   duplicate-line-final-position 1
   duplicate-region-final-position 1
   enable-recursive-minibuffers t
   epg-pinentry-mode 'loopback
   line-spacing 0.3
   font-lock-maximum-decoration nil
   frame-inhibit-implied-resize t
   hs-hide-comments-when-hiding-all nil
   indent-tabs-mode nil
   inhibit-compacting-font-caches t
   inhibit-default-init t
   inhibit-face-set-after-default t
   inhibit-free-realized-faces t
   inhibit-menubar-update t
   inhibit-startup-echo-area-message user-login-name
   inhibit-startup-screen            t
   inhibit-x-resources t
   initial-major-mode                'fundamental-mode
   initial-scratch-message           nil
   isearch-allow-motion t
   isearch-allow-scroll t
   large-file-warning-threshold nil
   make-backup-files nil
   next-screen-context-lines 0
   package-native-compile t
   read-buffer-completion-ignore-case t
   read-file-name-completion-ignore-case t
   require-final-newline t
   resize-mini-windows t
   scroll-conservatively 101
   scroll-preserve-screen-position t
   sentence-end-double-space nil
   shell-file-name "dash"
   shift-select-mode nil
   tab-width 4
   treesit-font-lock-level 1
   truncate-lines t
   user-full-name "Qingsong Liao"
   user-mail-address "llqingsong@qq.com"
   visible-cursor nil
   warning-minimum-level :error
   wdired-allow-to-change-permissions t
   wdired-create-parent-directories t
   word-wrap-by-category t
   x-stretch-cursor t

   )
  (global-font-lock-mode -1)
  (global-hide-mode-line-mode 1)
  (repeat-mode -1)
  (show-paren-mode -1)
  (global-eldoc-mode -1)
  (dolist (my-font-lock-list   '(
                                 dired-mode-hook
                                 rg-mode-hook
                                 magit-status-mode-hook
                                 ))
    (add-hook my-font-lock-list
              (lambda ()
                (font-lock-mode))))
  (dolist (my-fringe-list   '(
                              occur-mode-hook
                              magit-status-mode-hook
                              compilation-mode-hook
                              ))
    (add-hook my-fringe-list
              (lambda ()
                (setq-local left-fringe-width  20)
                )))
  (set-face-attribute 'default nil  :height 190)
  (add-hook 'after-make-frame-functions
            (defun setup-blah-keys (frame)
              (with-selected-frame frame
                (when (display-graphic-p)
                  (define-key input-decode-map (kbd "C-i") [C-i])
                  (define-key input-decode-map (kbd "C-S-i") [C-S-i])
                  (define-key input-decode-map (kbd "C-[") [C-lsb])
                  (define-key input-decode-map (kbd "C-m") [C-m])
                  (define-key input-decode-map (kbd "C-S-m") [C-S-m])
                  (global-unset-key (kbd "C-z"))
                  ))))
  :config
  (setq psession-object-to-save-alist
        (assq-delete-all 'buffer-name-history psession-object-to-save-alist))
  (setf (alist-get 'truncation fringe-indicator-alist) '(nil nil))
  (setf (alist-get 'continuation fringe-indicator-alist) '(nil nil))
  :hook
  ((after-init . psession-mode)
   (after-init . psession-savehist-mode)))

(use-package consult
  :bind(
        ([remap completion-at-point] . hippie-expand)
        ([remap complete-symbol] . hippie-expand)
        ([remap delete-window] . delete-window-window-divider)
        ([remap split-window-below] . split-window-below-window-divider)
        ([remap delete-other-windows] .  delete-other-windows-window-divider)
        ([remap imenu] . consult-imenu)
        ([remap switch-to-buffer] . consult-buffer)
        ([remap yank-pop] . consult-yank-pop)
        ([remap indent-rigidly] . cleanup-buffer)
        ([remap list-buffers] . ibuffer)
        )
  :ensure t
  :demand t
  :hook
  (completion-list-mode . consult-preview-at-point-mode)
  (before-save . delete-trailing-whitespace)
  (after-init . minibuffer-depth-indicate-mode)
  :init
  (setq register-preview-delay 0.5)
  (setq register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref)
  (setq xref-show-definitions-function #'consult-xref)
  (rg-enable-default-bindings) ;; C-c s
  (move-text-default-bindings) ;; M-up/down
  (use-package surround
    :defer t
    :bind-keymap
    ("C-<tab>" . surround-keymap))
  (setq consult-async-input-debounce 0.02
        consult-async-input-throttle 0.05
        consult-async-refresh-delay 0.02)

  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult-source-bookmark consult-source-file-register
   consult-source-recent-file consult-source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<"))

  (use-package pyim
    :ensure t
    :defer t
    :bind(:map pyim-mode-map
               ("<up>" . pyim-quit-no-clear)
               ("<down>" . pyim-quit-no-clear)
               ("<left>" . pyim-quit-no-clear)
               ("<right>" . pyim-quit-no-clear))
    :commands (toggle-input-method)
    :custom
    (default-input-method "pyim")
    :config
    (cl-defmethod pyim-page-info-format ((_style (eql minibuffer)) page-info)
      (string-replace ")" ""
                      (string-replace "(" "" (format "%s %s"
                                                     ;; (if (plist-get page-info :assistant-enable) " P|" "")
                                                     (if (plist-get page-info :assistant-enable)  (plist-get page-info :candidates) "")
                                                     (if (plist-get page-info :assistant-enable)  (plist-get page-info :current-page) "")))))
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
          (find-file "~/.local/share/mysource/hmdz.pyim")
          (beginning-of-buffer)
          (search-forward char nil (setq exsist 1))
          (when (= exsist 1)
            (search-backward "/")
            (right-char)
            (message "%s" (string-trim (current-word) "hmdz/")))
          (when (= exsist 0)
            (sayshit))
          (kill-buffer)
          (switch-to-buffer old))))

    (when
        (file-exists-p "~/.local/share/mysource/hmdz.pyim")
      (add-to-list 'pyim-dicts '(:name "hmdz" :file "~/.local/share/mysource/hmdz.pyim"))
      (pyim-scheme-add
       '(hmdz
         :document "虎码单字"
         :class xingma
         :code-prefix "hmdz/"
         :first-chars "abcdefghijklmnopqrstuvwxyz"
         :rest-chars "abcdefghijklmnopqrstuvwxyz"
         :code-prefix-history ("_")
         :code-split-length 100  ;; 不要自动按我的空格: )
         :code-maximum-length 100 ;; 不要把我的英文消灭掉: )
         ))
      (pyim-default-scheme 'hmdz)
      (donothing))

    :custom
    (pyim-indicator-list (list #'my-pyim-indicator-with-cursor-color #'pyim-indicator-with-modeline))
    (pyim-english-input-switch-functions nil)
    (pyim-process-autoselector nil)
    (pyim-dhook-verbose nil)
    (pyim-page-tooltip '(minibuffer popup posframe))
    (pyim-dicts nil)  ; Initialize the list if it's not already defined
    (pyim-cloudim nil)
    (pyim-candidates-search-buffer-p nil)
    (pyim-enable-shortcode nil)
    (pyim-punctuation-dict '(("^" "…")("\\" "、")("." "。")("," "，")("'" "‘" "’") ("\"" "“" "”"))))

(use-package quick-sdcv
  :defer t
  :bind ( :map quick-sdcv-mode-map
          ("SPC" . scroll-up-command)
          ("DEL" . scroll-down-command)
          ("y" . (lambda () (interactive)(previous-buffer) (other-window 1)))
          ("u" . delete-window)))

(use-package undo-fu-session
  :ensure t
  :commands undo-fu-session-global-mode
  :config
  (use-package undo-fu
    :ensure t
    :bind(([remap undo] . undo-fu-only-undo)
          ([remap redo] . undo-fu-only-redo))
    :commands (undo-fu-only-undo
               undo-fu-only-redo
               undo-fu-only-redo-all
               undo-fu-disable-checkpoint)
    :custom
    (undo-fu-allow-undo-in-region nil))
  :hook
  (after-init . undo-fu-session-global-mode))

(use-package vterm
  :defer t
  :ensure t
  :commands (vterm--internal multi-vterm-project)
  :bind (:map vterm-mode-map
              ("C-p" . vterm-copy-mode)
              ("M-e" . other-window)
              :map vterm-copy-mode-map
              ("C-p" . vterm-previous-prompt)
              ("C-f" . vterm-next-prompt )
              ("C-<return>" . compile-goto-error))
  :custom
  (vterm-shell "fish")
  (vterm-always-compile-module t)
  (vterm-timer-delay 0.01)
  (vterm-kill-buffer-on-exit t)
  :hook
  (vterm-mode . compilation-shell-minor-mode))

(use-package magit
  :defer t
  :config
  (use-package diff-mode
    :defer t
    :custom
    (diff-default-read-only t)
    (diff-font-lock-syntax 'hunk-also)
    (diff-font-lock-prettify t))
  (setq magit-bury-buffer-function 'magit-restore-window-configuration)
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  (setq magit-diff-refine-hunk  'all)
  (setq magit-log-margin-show-committer-date t)
  (setq magit-log-margin '(t age magit-log-margin-width t 3))
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
          ("XXXX*"  . "#000000")))
  (use-package forge
    :defer t
    :after magit))

(use-package super-save
  :commands (super-save-mode)
  :hook (after-init . super-save-mode)
  :custom
  (super-save-silent t)
  (super-save-all-buffers  t)
  (super-save-remote-files nil)
  (super-save-auto-save-when-idle nil)
  (super-save-idle-duration nil))

(use-package avy
  :defer t
  :ensure t
  :init
  (setq avy-all-windows t)
  (setq avy-keys '( ?t ?n ?r ?i ?s ?e))
  :config
  (avy-setup-default)
  (defun avy-action-mark-to-char (pt)
    (activate-mark)
    (goto-char pt))
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
  (defun avy-action-kill-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (kill-whole-line))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)
  (defun avy-action-yank-whole-line (pt)
    (avy-action-copy-whole-line pt)
    (save-excursion (yank))
    t)
  (setf
   (alist-get ?  avy-dispatch-alist) 'avy-action-mark-to-char
   (alist-get ?k avy-dispatch-alist) 'avy-action-kill-stay
   (alist-get ?K avy-dispatch-alist) 'avy-action-kill-whole-line
   (alist-get ?y avy-dispatch-alist) 'avy-action-yank
   (alist-get ?Y avy-dispatch-alist) 'avy-action-yank-whole-line
   (alist-get ?w avy-dispatch-alist) 'avy-action-copy
   (alist-get ?W avy-dispatch-alist) 'avy-action-copy-whole-line
   ))

(use-package trashed
  :ensure t
  :defer t
  :commands (trashed)
  :init
  (setq trashed-action-confirmer 'y-or-n-p
        trashed-use-header-line t
        trashed-sort-key '("Date deleted" . t)
        trashed-date-format "%m-%d %H:%M:%S"))

(use-package ligature
  :demand t
  :config
  (let ((ligs '("ff" "fi" "ffi" "fl" "ffl")))
    (ligature-set-ligatures 't ligs))
  (global-ligature-mode 1))

(use-package dumb-jump
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  :custom
  (dumb-jump-force-searcher 'rg)
  (dumb-jump-default-project "/home/leeao/lib"))

(use-package multiple-cursors
  :defer t
  :bind (:map mc/keymap
              ("<return>" . electric-newline-and-maybe-indent)))

(use-package aggressive-indent
  :defer t
  :ensure t
  :hook
  (emacs-lisp-mode . aggressive-indent-mode)
  )

(use-package embark
  :ensure t
  :defer t
  :commands (embark-act
             embark-dwim
             embark-export
             embark-collect
             embark-bindings
             embark-prefix-help-command)
  :init (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (use-package embark-consult
    :ensure t
    :defer t
    :hook
    (embark-collect-mode . consult-preview-at-point-mode))
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package real-mono-themes
  :demand t
  :config
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'real-mono-eink t)
  (custom-set-faces
   '(corfu-quick1 ((t (:inherit bold))))
   '(corfu-quick2 ((t (:inherit bold))))
   '(rst-level-1 ((t )))
   '(rst-level-2 ((t )))
   '(rst-level-3 ((t )))
   '(rst-level-4 ((t )))
   '(rst-level-5 ((t )))
   '(rst-level-6 ((t )))
   '(region ((t (:extend t :underline (:color foreground-color :style double-line :position 5)))))
   '(trailing-whitespace ((t )))
   '(tab-bar-tab ((t (:inherit tab-bar))))
   '(tab-bar-tab-inactive ((t (:box (:line-width (-1 . -1) :color "black" :style flat-button)))))
   '(makefile-space ((t (:underline t))))
   '(dictionary-word-definition-face ((t )))
   '(dictionary-word-entry-face ((t )))))

(use-package vertico
  :ensure t
  :demand t
  :hook
  (after-init . vertico-mode)
  :commands vertico-mode
  :bind(:map vertico-map
             ("<next>" . scroll-up-command)
             ("<prior>" . scroll-down-command)
             ("<right>" . sayshit)
             ("M-<next>" . vertico-next-group)
             ("M-<prior>" . vertico-previous-group))
  :custom
  (vertico-scroll-margin 0)
  (vertico-count 5)
  (vertico-resize nil)
  (vertico-cycle nil)
  :config
  (use-package orderless
    :ensure t
    :demand t
    :custom
    (completion-styles '(orderless basic))
    (completion-category-defaults nil)
    (completion-category-overrides '((file (styles partial-completion)))))
  (use-package vertico-flat
    :ensure nil
    :demand t
    :after vertico)

  (use-package vertico-directory
    :ensure nil
    :after vertico
    :demand t
    :bind (:map vertico-map
                ("RET" . vertico-directory-enter)
                ("DEL" . vertico-directory-delete-char)
                ("M-DEL" . vertico-directory-delete-word))
    ;; Tidy shadowed file names
    :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)))

(use-package yasnippet
  :ensure t
  :hook
  (after-init . yas-global-mode)
  :custom
  (yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-also-auto-indent-first-line t)  ; Indent first line of snippet
  (yas-also-indent-empty-lines t)
  (yas-snippet-revival nil)  ; Setting this to t causes issues with undo
  :init
  (setq yas-verbosity 0))

(use-package helpful
  :ensure t
  :defer t
  :commands (helpful-callable
             helpful-variable
             helpful-key
             helpful-command
             helpful-at-point
             helpful-function)
  :bind(([remap describe-command] . helpful-command)
        ([remap describe-function] . helpful-callable)
        ([remap describe-key] . helpful-key)
        ([remap describe-symbol] . helpful-symbol)
        ([remap describe-variable] . helpful-variable))
  :custom
  (helpful-max-buffers 3))

(use-package markdown-mode
  :defer t
  :commands (gfm-mode
             gfm-view-mode
             markdown-mode
             markdown-view-mode)
  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode))
  :bind(:map markdown-mode-map
             ("C-c C-e" . markdown-do)))

(use-package nov
  ;; PEN 笔
  ;; 1. <tab>
  ;;    - double: <enter>
  ;;    - long: C-M-i
  ;; 2. <prior>
  ;;    - long: <esc>/<f5>
  ;; 3. <next>
  ;;    - long: <b>
  ;; 4. laser
  :defer t
  :custom
  (nov-header-line-format nil)
  :mode ("\\.epub\\'" . nov-mode)
  :bind (:map nov-mode-map
              ([remap scroll-up-command] . nov-scroll-up)
              ([remap scroll-down-command] . nov-scroll-down)))

(use-package devdocs-browser
  :config
  (dolist (lang '(
                  "c"
                  ))

    (unless (file-exists-p
             (concat devdocs-browser-data-directory "/"
                     devdocs-browser--docs-dir "/"
                     lang))
      (devdocs-browser-install-doc lang))

    (unless (file-exists-p
             (concat
              devdocs-browser-data-directory "/"
              devdocs-browser--docs-dir "/"
              lang "/"
              devdocs-browser--offline-data-dir-name))
      (devdocs-browser-download-offline-data lang))))

(use-package pdf-tools
  :ensure t
  :defer t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :init (pdf-loader-install)
  :commands (pdf-view-mode)
  :bind ( :map pdf-view-mode-map
          ([remap scroll-down-command] . pdf-view-scroll-down-or-previous-page)
          ([remap scroll-up-command] . pdf-view-scroll-up-or-next-page))
  :config
  (add-to-list 'revert-without-query ".pdf")
  (use-package saveplace-pdf-view))

(use-package gptel
  :defer t
  :config
  (setq deepseek-api-key
        (with-temp-buffer
          (insert-file-contents "/run/secrets/dk")
          (buffer-string)))
  (setq gptel-model   'deepseek-chat
        gptel-backend (gptel-make-deepseek "deepseek"
                        :stream nil
                        :key deepseek-api-key)))

(use-package consult-gh
  :defer t
  :after consult
  :custom
  (consult-gh-default-clone-directory "~/")
  (consult-gh-default-save-directory "~/")
  (consult-gh-confirm-before-clone nil)
  (consult-gh-ask-for-path-before-save nil)
  (consult-gh-show-preview t)
  (consult-gh-preview-key "C-o")
  (consult-gh-repo-action #'consult-gh--repo-browse-files-action)
  (consult-gh-large-file-warning-threshold 2500000)
  (consult-gh-confirm-name-before-fork nil)
  (consult-gh-notifications-show-unread-only nil)
  (consult-gh-prioritize-local-folder nil)
  (consult-gh-issues-state-to-show "all") ; show readmes in their original format
  (consult-gh-group-dashboard-by :reason)
  (consult-gh-repo-preview-major-mode nil) ; show readmes in their original format
  (consult-gh-preview-major-mode 'markdown-mode)
  :config
  (consult-gh-enable-default-keybindings)
  (use-package consult-gh-forge
    :after consult-gh
    :config
    (consult-gh-forge-mode +1))
  (use-package consult-gh-embark
    :after consult-gh
    :config
    (consult-gh-embark-mode +1)
    (setq consult-gh-forge-timeout-seconds 20)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            begin of rotate-text                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'cl)

(defgroup rotate-text nil
  "Cycle through words, symbols and patterns."
  :group 'abbrev
  :group 'convenience
  :group 'matching)

(defcustom rotate-text-patterns
  '(("\\_<[^-]\\(\\sw\\|\\s_\\)*[0-9]+" rotate-text-increment-number-in-symbol)
    ("-?0x?[0-9a-fA-F]+" rotate-text-increment-hex-number)
    ("-?[0-9]+" rotate-text-increment-number))
  "*Patterns and functions to rotate them.
Each entry is a list.  Its first element should be the regular expression to
replace, the second element is a function.  When rotating, it is called with the
matching text and an integer determining the rotation amount and direction."
  :group 'rotate-text
  :type '(repeat (list (string :tag "Regular expression")
                       (function :tag "Rotation function"))))

(defcustom rotate-text-symbols '(("private" "protected" "public"))
  "*List of symbol names to rotate.
Each element is a list of symbols that should be cycled through."
  :group 'rotate-text
  :type '(repeat (repeat :tag "Rotation group" (string :tag "Symbol"))))

(defcustom rotate-text-words '(("width" "height")
                               ("left" "right" "top" "bottom"))
  "*List of words to rotate.
Each element is a list of words that should be cycled through.  Individual
segments in symbol names are recognized as words, i.e. windowWidth can be
replaced with windowHeight.
All entries must be in lower case. The case is determined by the rotated
text."
  :group 'rotate-text
  :type '(repeat (repeat :tag "Rotation group" (string :tag "Word"))))

(defvar rotate-text-local-patterns nil
  "*Buffer local additions to `rotate-text-patterns'.")
(make-variable-buffer-local 'rotate-text-local-patterns)

(defvar rotate-text-local-symbols nil
  "*Buffer local additions to `rotate-text-symbols'.")
(make-variable-buffer-local 'rotate-text-local-symbols)

(defvar rotate-text-local-words nil
  "*Buffer local additions to `rotate-text-words'.")
(make-variable-buffer-local 'rotate-text-local-words)

;;; numbers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rotate-text-increment-number (original arg &optional minimum)
  (number-to-string (max (+ (string-to-number original) arg)
                         (or minimum most-negative-fixnum))))

(defun rotate-text-increment-hex-number (original arg)
  (when (string-match "\\`-?\\(0x\\)" original)
    (setq original (replace-match "" t t original 1)))
  (let ((result (+ (string-to-number original 16) arg)))
    (format "%s0x%x" (if (< result 0) "-" "") (abs result))))

(defun rotate-text-increment-number-in-symbol (original arg)
  (when (string-match "[0-9]+" original)
    (replace-match (rotate-text-increment-number (match-string 0 original)
                                                 arg 0)
                   t t original)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rotate-text-replacement (replacements original dir)
  "Find the replacement for ORIGINAL in REPLACEMENTS."
  (save-match-data
    (if (functionp (car replacements))
        ;; function
        (if (and (< dir 0) (functionp (cadr replacements)))
            (funcall (cadr replacements) original (- dir))
          (funcall (car replacements) original dir))
      ;; list
      (let ((rest-pattern (member original replacements)))
        (when rest-pattern
          (cl-nth-value (mod (- dir (length rest-pattern)) (length replacements))
                        replacements))))))

(defun rotate-text-match-at-point (regexp)
  (save-excursion
    (let ((pos (point)))
      (goto-char (point-at-bol))
      (catch 'match
        (while (re-search-forward regexp (1+ (point-at-eol)) t)
          (and (>= pos (match-beginning 0))
               (<= pos (match-end 0))
               (throw 'match (match-string-no-properties 0))))))))

(defun rotate-text-symbol-at-point ()
  "Rotate the symbol at point."
  (rotate-text-match-at-point "\\_<\\(\\s_\\|\\sw\\)+\\_>"))

(defun rotate-text-word-at-point ()
  "Rotate the word at point."
  (let ((case-fold-search nil))
    (or (rotate-text-match-at-point "\\(\\<\\|[[:upper:]]\\)[[:lower:]]+")
        (rotate-text-match-at-point "\\<[[:upper:]]+"))))

(defun rotate-text-match-case (original new)
  "Match the case of ORIGINAL in NEW."
  (let ((case-fold-search nil))
    (save-match-data
      (cond
       ((string-match "\\`[[:upper:]][[:lower:]]" original) (capitalize new))
       ((string-match "\\`[[:upper:]][[:upper:]]" original) (upcase new))
       (t new)))))

(defvar rotate-text-last-offset nil)

(defun rotate-text (arg &optional default-string com-symbols com-words com-patterns)
  "Rotate the text at point. If there is nothing to rotate at point and DEFAULT-STRING is non-nil,
DEFAULT-STRING is inserted at point.

COM-SYMBOLS, COM-WORDS and COM-PATTERNS are per-command addition to `rotate-text-symbols',
`rotate-text-words' and `rotate-text-patterns', respectively."
  (interactive (list (if (consp current-prefix-arg)
                         -1
                       (prefix-numeric-value current-prefix-arg))))
  (let ((pos (point))
        (offset 0)
        match replacement)
    (or ;; symbols
     (when (setq match (rotate-text-symbol-at-point))
       (dolist (symbols (append com-symbols rotate-text-local-symbols
                                rotate-text-symbols))
         (when (setq replacement
                     (rotate-text-replacement symbols match arg))
           (cl-return t))))
     ;; words
     (when (setq match (rotate-text-word-at-point))
       (dolist (words (append com-words rotate-text-local-words
                              rotate-text-words))
         (when (setq replacement
                     (rotate-text-replacement words (downcase match) arg))
           (setq replacement (rotate-text-match-case match replacement))
           (cl-return t))))
     ;; regexp
     (dolist (pattern (append com-patterns rotate-text-local-patterns
                              rotate-text-patterns))
       (when (setq match (rotate-text-match-at-point (car pattern)))
         (setq replacement (rotate-text-replacement (cdr pattern) match arg))
         (cl-return t))))

    (if (not replacement)
        (progn (unless default-string
                 (error "Nothing to rotate"))
               (insert default-string)
               (setq rotate-text-last-offset nil))

      (progn
        (unless (and rotate-text-last-offset
                     (eq last-command this-command))
          (setq rotate-text-last-offset
                (if (eq pos (match-end 0))
                    'end
                  (- pos (match-beginning 0)))))

        (replace-match replacement nil t)

        (goto-char (if (eq rotate-text-last-offset 'end)
                       (match-end 0)
                     (min (+ (match-beginning 0) rotate-text-last-offset)
                          (match-end 0))))))))

(defun rotate-text-backward (arg &optional default-string com-symbols com-words com-patterns)
  "Rotate the text at point backwards. If there is nothing to rotate at point and DEFAULT-STRING is non-nil,
DEFAULT-STRING is inserted at point.

COM-SYMBOLS, COM-WORDS and COM-PATTERNS are per-command addition to `rotate-text-symbols',
`rotate-text-words' and `rotate-text-patterns', respectively."
  (interactive (list (if (consp current-prefix-arg)
                         -1
                       (prefix-numeric-value current-prefix-arg))))
  (rotate-text (- arg) default-string com-symbols com-words com-patterns))

(setq rotate-text-symbols '(
                            ("t" "nil")
                            ("in" "out" "err" "io")
                            ("a" "b" "c" "d")
                            ("i" "j" "k")
                            ("private" "protected" "public")
                            ("break" "continue")
                            ("x" "y" "z")))

(setq rotate-text-words '(
                          ("argc" "argv")
                          ("add" "remove")
                          ("column" "line")
                          ("const" "statis")
                          ("cos" "sin")
                          ("enable" "disable")
                          ("fixme" "todo" "okay" "temp")
                          ("float" "double")
                          ("init" "creat" "close")
                          ("input" "output")
                          ("left" "right" "top" "bottom")
                          ("max" "min")
                          ("new" "old")
                          ("printf" "fprintf" "sprintf" "snprintf")
                          ("size" "uint")
                          ("start" "end")
                          ("stdout" "stdin" "stderr" "stdio")
                          ("str" "list")
                          ("timer" "keyboard" "display")
                          ("true" "false")
                          ("void" "bool" "char" "int")
                          ("width" "height")
                          ("yes" "no")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             end of rotate-text                             ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               begin of tavily                              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq tavily-api-key
      (with-temp-buffer
        (insert-file-contents "/run/secrets/ty")
        (buffer-string)))
(defun tavily-search-async (callback query &optional search-depth max-results exclude_domains country include_domains)
  "Perform a search using the Tavily API and return results as JSON string.
API-KEY is your Tavily API key.
QUERY is the search query string.
Optional SEARCH-DEPTH is either \"basic\" (default) or \"advanced\".
Optional MAX-RESULTS is the maximum number of results (default 5)."
  (require 'plz)
  (let* ((plz-curl-default-args (cons "-k" plz-curl-default-args))
         (url "https://api.tavily.com/search")
         (search-depth (or search-depth "basic"))
         (max-results (or max-results 3))
         (include_answer  nil)
         (country (or country "united states"))
         (include_domains (or include_domains '(
                                                "nixos.org"
                                                "git.sr.ht"
                                                "codeberg.org"
                                                "github.com"
                                                "gitlab.com"
                                                ;; "hackaday.com"
                                                "lwn.net"
                                                "osdev.org"
                                                ;; "ziggit.dev"
                                                ;; others moved to index.org
                                                )))
         (request-data
          `(("api_key" . ,tavily-api-key)
            ("query" . ,query)
            ("search_depth" . ,search-depth)
            ("country" . ,country)
            ("include_domains" . ,include_domains)
            ("include_answer" . ,include_answer)
            ("exclude_domains" . ,exclude_domains)
            ("max_results" . ,max-results))))
    (plz 'post url
         :headers '(("Content-Type" . "application/json"))
         :body (json-encode request-data)
         :as 'string
         :then (lambda (result) (funcall callback result)))))
(defun tavily-search (query)
  (interactive "sQuery: ")
  (tavily-search-async
   (lambda (result)
     (let ((buf (get-buffer-create "*tavily-search-result*")))
       (switch-to-buffer buf)
       (read-only-mode 0)
       (erase-buffer)
       (org-mode)
       (insert (tavily-result-to-org result))
       (goto-char (point-min))
       (read-only-mode 1)
       (setq-local truncate-lines nil)
       ))
   query))
(defun tavily-result-to-org (json-result)
  "Convert a Tavily JSON response string into Org-mode formatted entries."
  (let* ((json-object-type 'alist)
         (json-array-type 'list)
         (json-key-type 'symbol)
         (data (json-parse-string json-result
                                  :object-type 'alist
                                  :array-type 'list
                                  :null-object nil))
         (results (alist-get 'results data)))
    (mapconcat
     (lambda (item)
       (let ((url     (or (alist-get 'url item) ""))
             (title   (or (alist-get 'title item) ""))
             (content (or (alist-get 'content item) "")))
         (format "* [[%s][%s]]\n%s"
                 url
                 title
                 (replace-regexp-in-string
                  "^" "  "
                  (string-trim content)))))
     results
     "\n\n")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                end of tavily                               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              begin of leetcode                             ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar  my/leetcode-root "~/Leetcode/src/" "where is the Leetcode?")
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
      (progn
        (find-file org)
        (setq-local truncate-lines nil)
        (setq-local org-startup-truncated nil))
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

(defun leetcode-open-cursor ()
  (interactive)
  (my/leetcode-open
   (string-to-number(current-word))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               end of leetcode                              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                begin of sway                               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar sway-rotate 0
  "Current sway rotate state, either 0 for horizental or  1 for vertical.")

(defun swayfullstate()
  (= (call-process-shell-command "swaymsg -t get_tree | jq -e '.. | select(.focused? == true and .fullscreen_mode == 1)' >/dev/null;" nil t t) 4))

(defun swaywindow()
  (interactive)
  (let ((direction '("down" "right")))
    (if (swayfullstate)
        (start-process  "swaymsg" nil  "swaymsg" "focus" (cl-nth-value sway-rotate direction))
      (and
       (start-process  "fullscreen1" nil  "swaymsg" "fullscreen" )
       (start-process  "swaymsg" nil  "swaymsg" "focus" (cl-nth-value sway-rotate direction))
       (start-process  "fullscreen2" nil  "swaymsg" "fullscreen" )
       ))))

(defun swayrotate()
  (interactive)
  (start-process  "fullscreen1" nil  "swaymsg" "layout toggle split" )
  (setq sway-rotate   (if (= 0 sway-rotate) 1 0 )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 end of sway                                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            begin of toggle-font                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar my-default-font "bookerly")
(defvar my-alternate-font "Ubuntu Mono Nerd Font")

(defvar font-scale-list '((130 165)(140 190)(150 200)(160 210)(170 225)(180 240)(190 250)(200 270)(210 280)(220 285)(230 310)(240 315)(250 320)(260 350)))
(defvar fontfont t "in defualt-font or alternate-font, 1 stand for in default, 0 stand for alternate-font")
;; change font, stay scale

(defun my-toggle-font (&optional n)
  "Toggle between UbuntuMono and bookerly fonts."
  (interactive "^p")
  (setq fontfont (not fontfont))
  (let ((scale text-scale-mode-amount))
    (and (set-face-attribute
          'default  (selected-frame)
          :font
          (if fontfont my-default-font my-alternate-font)
          :height
          (cl-nth-value
           (if fontfont 0 1)
           (cl-nth-value
            (if toggle-book-mode book-mode prog-mode)
            font-scale-list)))
         (text-scale-set scale))))

(defvar  toggle-book-mode t "current state of toggle book"  )
(defvar  book-mode 6 "I should read more book")
(defvar  prog-mode 3 "I should write more code")
;; change font size, set scale to 0.
(defun toggle-book-mode()
  (interactive)
  (setq toggle-book-mode (not toggle-book-mode))
  (let ((num (if  toggle-book-mode 80 65)))
    (setq woman-fill-column num)
    (setq Man-width num)
    (setq fill-column num)
    (setq vterm-min-window-width num))
  (set-face-attribute 'default (selected-frame) :font
                      (if fontfont my-default-font my-alternate-font)
                      :height (cl-nth-value (if fontfont 0 1) (cl-nth-value
                                                               (- (if toggle-book-mode book-mode prog-mode) (if fontfont 0 3))
                                                               font-scale-list)))
  (text-scale-set 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            end of toggle-font                              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              begin of monitor                              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar monitor-state 1
  "Current monitor state, either 0 for read or  1 for watch.")
(defun monitor (&optional n)
  "swtich paperlike-hd monitor from read mode to watch mode using paperlike-cli"
  (interactive "^p")
  (if (numberp n)
      (progn
        (when (= n 123)
          (setq use-short-answers nil)
          (if (yes-or-no-p "(Bro, Are you in a Clear mind now? yes or no)")
              (setq monitor-state 0)
            (keyboard-quit))
          (setq use-short-answers t))
        (when (= n 3)
          (setq monitor-state 2)))
    (setq monitor-state 1))

  (let ((monitorprotocol "-i2c")
        (monitorpath "/dev/i2c-4")
        (monitorcli "paperlike-cli")
        (monitorarg '("-contrast" "-speed" "-mode" "-clear"))
        (mode-state '(("3" "5" "3")  ("9" "5" "1") ("9" "3" "1")))) ; hey,  eink is awesome!
    (sleep-for 0.5)
    (dotimes (number 3)
      (call-process monitorcli nil nil nil
                    monitorprotocol
                    monitorpath
                    (cl-nth-value number monitorarg)
                    (cl-nth-value number (cl-nth-value monitor-state mode-state)))
      (sleep-for 1))
    (setq monitor-state  1)
    (sleep-for 1)
    (call-process monitorcli nil nil nil monitorprotocol monitorpath (car (nthcdr 3 monitorarg))))
  (donothing))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               end of monitor                               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               begin of minor                               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-jichang(price traffic usage &optional used)
  "如 (my-jichang 14.9 130.0 0.39 86.08)， 用于计算梯子的大概天数/日均价格，
0.39是最近110天内计算的日均流量使用。 price 梯子价格, traffic 购得的流量
usage 个人一般每日使用的流量,used(可选) 己经使用的流量"
  (message (format (concat " 每GB流量%f元 \n 大概使用%f天或%f年 \n 每天大概%f元 \n " (if  (numberp used) (format "大概还剩%f天"     (/ (- traffic used) (* usage 1.0)))nil))
                   (/ price (* traffic 1.0))
                   (/ traffic (* usage 1.0))
                   (/ (/ traffic (* usage 1.0)) 365)
                   (/ price (/ traffic (* usage 1.0))))))

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

(defun donothing () (interactive)(message ""))
(defun sayshit () (interactive)(message "啊!啊!啊!啊!啊!啊!啊!啊!啊!啊!啊!啊!啊!啊!啊!啊!啊!啊!啊!啊!啊!啊!啊!啊!"))

(defun my--compilation-visible-p ()
  (get-buffer-window "*compilation*" t))

(defun my--maybe-hide-compilation (buffer alist)
  (when (and (string= (buffer-name buffer) "*compilation*")
             (not (my--compilation-visible-p)))
    ;; Returning non-nil tells Emacs “handled, do not display”
    t))

(add-to-list 'display-buffer-alist
             '("\\*compilation\\*"
               my--maybe-hide-compilation))

(defun consult-line-symbol-at-point ()
  "Search for a line matching the symbol found near point."
  (interactive)
  (consult-line
   (or (thing-at-point 'symbol))))

(defun consult-ripgrep-symbol-at-point  ()
  "Search for a project line matching the symbol found near point."
  (interactive)
  (consult-ripgrep  (project-root (project-current t))
                    (or (thing-at-point 'symbol))))

(defvar wake-up 0 "why the wakeup duplicate press??")
(defun wakeupcall ()
  (interactive)
  (setq wake-up (1+ wake-up))
  (when (= wake-up 2)
    (monitor)
    (message "(hibernate-mode -1)"))
  )

(defun hibernatecall()
  (interactive)
  (if (yes-or-no-p "hibernate? y or n")
      (progn
        (setq wake-up 0)
        (message "(hibernate-mode 1)")
        (call-process "systemctl" nil nil nil "hibernate"))
    (keyboard-quit)))

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

(defvar wc-regexp-chinese-char-and-punc (rx (category chinese)))
(defvar wc-regexp-chinese-punc "[。，！？；：「」『』（）、【】《》〈〉※—]")
(defvar wc-regexp-english-word "[a-zA-Z0-9-]+")
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
中英文合计(包含标点)：%s
%s"
             chinese-char chinese-char-and-punc english-word
             (+ chinese-char english-word)
             (+ chinese-char-and-punc english-word)
             (format "%s" (funcall-interactively 'count-words (region-beginning) (region-end)))))))

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
  (save-buffer))

(defun toggle-special-buffer ()
  (interactive)
  (if (< (count-windows) 2)
      (progn
        (split-window)
        (other-window 1)
        (switch-to-buffer
         (cl-find-if (lambda (buf)
                       (with-current-buffer buf
                         (derived-mode-p '(
                                           eww-mode
                                           help-mode
                                           occur-mode
                                           compilation-mode
                                           Info-mode
                                           rg-mode
                                           man-mode
                                           woman-mode
                                           ))))
                     (buffer-list)))
        (other-window 1))
    (delete-other-windows) ))

(defun switch-to-output ()
  "switch to rg/compilation/occur buffer"
  (interactive)
  (if (eq  (current-buffer) compilation-last-buffer)
      (previous-buffer)
    (switch-to-buffer compilation-last-buffer)))

(defun switch-to-eww ()
  "切换去eww/help"
  (interactive)
  (if (derived-mode-p
       '(eww-mode
         gfm-mode
         help-mode))
      (previous-buffer)
    (switch-to-buffer (cl-find-if
                       (lambda (buf)
                         (with-current-buffer buf
                           (derived-mode-p
                            '(eww-mode
                              gfm-mode
                              help-mode))))
                       (buffer-list)))))

(defun switch-to-eww ()
  "切换去eww/help"
  (interactive)
  (if (derived-mode-p
       '(eww-mode help-mode))
      (previous-buffer)
    (switch-to-buffer (cl-find-if
                       (lambda (buf)
                         (with-current-buffer buf
                           (derived-mode-p
                            '(eww-mode help-mode))))
                       (buffer-list)))))

(defun switch-to-man ()
  "切换去 info/woman/man"
  (interactive)
  (if (derived-mode-p
       '(Info-mode
         woman-mode
         man-mode))
      (previous-buffer)
    (switch-to-buffer
     (cl-find-if (lambda (buf)
                   (with-current-buffer
                       buf (derived-mode-p
                            '(Info-mode
                              woman-mode
                              man-mode))))
                 (buffer-list)))))

(defun switch-to-code ()
  "切换去两个最近使用的 c/c++/zig buffer"
  (interactive)
  (let ((my-buffer-name (buffer-name)))
    (switch-to-buffer
     (cl-find-if (lambda (buf)
                   (with-current-buffer buf
                     ( and (derived-mode-p
                            '(
                              c-mode
                              c++-mode
                              zig-mode
                              gud-mode
                              ))
                       (not (eq (buffer-name) my-buffer-name)))))
                 (buffer-list)))))

(defun replace-url-in-region ()
  "Replace URLs in region with 0.0.0.0 and convert / to 0.0.0.0."
  (interactive)
  (when (use-region-p)
    (save-excursion
      (save-restriction
        (narrow-to-region (region-beginning) (region-end))
        (goto-char (point-min))
        ;; Replace https:// → 0.0.0.0
        (while (search-forward "https://" nil t)
          (replace-match "0.0.0.0 " nil t))
        (goto-char (point-min))
        ;; Replace http:// → 0.0.0.0
        (while (search-forward "http://" nil t)
          (replace-match "0.0.0.0 " nil t))
        (goto-char (point-min))
        ;; Replace / →
        (while (search-forward "/" nil t)
          (replace-match "" nil t))
        (mark-whole-buffer)
        (widen)
        (sort-lines nil (point-min) (point-max))
        (delete-duplicate-lines (point-min) (point-max))
        ))))

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

(defun search-github-code ()
  "Search GitHub code for current major mode's language.
Uses word at point as default, or prompts for input."
  (interactive)
  (let* (
         (language (pcase major-mode
                     ('zig-mode "zig")
                     ('rust-mode "rust")
                     ('python-mode "python")
                     ('emacs-lisp-mode "emacs-lisp")
                     ('go-mode "go")
                     ('javascript-mode "javascript")
                     ('typescript-mode "typescript")
                     (_ (read-string "Language: "))))
         (input (read-string (format "Search %s code: " language) )))
    (consult-gh-search-code (format "%s -- --language=%s --limit 7" input language)
                            nil nil nil nil)))

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

(defun isearch-other-window ()
  (interactive)
  (save-selected-window
    (other-window 1)
    (isearch-forward)))

(defun comment-or-uncomment-region-or-line ()
  "Like comment-or-uncomment-region, but if there's no mark \(that means no
     region\) apply comment-or-uncomment to the current line"
  (interactive)
  (if (not mark-active)
      (comment-line 1)
    (if (< (point) (mark))
        (comment-or-uncomment-region (point) (mark))
      (comment-or-uncomment-region (mark) (point)))))

(defun ctags()
  (interactive)
  (start-process "ctags" nil shell-file-name shell-command-switch
                 (concat "cd" (project-root (project-current t)) "; " "ctags --recurse -e" )))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
     Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (let ((start (if (region-active-p) (region-beginning) (point-min)))
        (end (if (region-active-p) (region-end) (point-max))))
    (untabify start end)
    (indent-region start end)
    (call-interactively 'delete-trailing-whitespace)
    (set-buffer-file-coding-system 'utf-8))
  (save-buffer)
  (donothing))

(defun show-useful-list ()
  (interactive)
  (which-function-mode 1)
  (completion-in-region (point) (point) (list
                                         (format "%s" (buffer-name))
                                         (format "%s" (if (equal major-mode 'dired-mode) default-directory (buffer-file-name)))
                                         (format "处于第%s行 第%s列 %s函数中" (line-number-at-pos)(current-column) (which-function))
                                         (format "现在是%s点钟 %s" (format-time-string "%k")
                                                 (if (> 30 (string-to-number (format-time-string "%M")))
                                                     "未过半" "过半"))
                                         ;; (format "域: %s" easysession--current-session-name)
                                         ))
  (which-function-mode -1))

;; (setq mc/cmds-to-run-for-all
;;       '(
;;         backward-sexp
;;         beginning-of-buffer
;;         c-electric-semi&comma
;;         comment-or-uncomment-region-or-line
;;         dabbrev-expand
;;         delete-region-and-yank
;;         embark-act
;;         embark-dwim
;;         eval-last-sexp
;;         forward-sexp
;;         indent-for-tab-command
;;         kill-line-or-region
;;         kill-region
;;         move-text-down
;;         move-text-up
;;         org-beginning-of-line
;;         org-delete-char
;;         org-end-of-line
;;         org-kill-line
;;         org-self-insert-command
;;         org-yank
;;         surround-delete
;;         surround-insert
;;         toggle-input-method
;;         transpose-words
;;         wdired--self-insert
;;         wdired-change-to-wdired-mode
;;         wdired-finish-edit
;;         ))
;;
;; (setq mc/cmds-to-run-once
;;       '(
;;         dired-clean-directory
;;         iedit-switch-to-mc-mode
;;         ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                end of minor                                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                recent added                                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun buffer-occur (regex)
  (interactive "sRegex: ")
  (multi-occur (buffer-list (selected-frame)) regex))

(defun dec-to-hex (dec)
  (format "%02X" dec))

(defun rgb-to-hex (r g b)
  (concat "#" (mapconcat #'dec-to-hex (list r g b) "")))

(defun delete-this-buffer-and-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(defvar start-time nil
  "whether start record time, nil means didn't start yet, and time value means it is recording ")

(defun record-time ()
  "to record time by toggle twice"
  (interactive)
  (if start-time
      (if (yes-or-no-p "(Stop Recording now? yes or no)")
          (and (message  "Take about %s"
                         (format-time-string "%T"   (time-since start-time) '(0 "HKT")))
               (setq start-time nil))
        (keyboard-quit))
    (message "Record time at %s"
             (format-time-string "%T"  (setq start-time (current-time)) (current-time-zone)))))

(defun split-window-below-window-divider()
  (interactive)
  (window-divider-mode 1)
  (split-window-below))

(defun delete-other-windows-window-divider()
  (interactive)
  (window-divider-mode -1)
  (delete-other-windows))

(defun delete-window-window-divider()
  (interactive)
  (window-divider-mode -1)
  (delete-window))

(defun toggle-vertico ()
  "toggle vertico unobtrusive"
  (interactive)
  (if vertico-unobtrusive-mode
      (vertico-unobtrusive-mode 1)
    (vertico-unobtrusive-mode -1))
  (setq vertico-unobtrusive-mode
        (not vertico-unobtrusive-mode)))

(toggle-vertico)
(toggle-vertico)

(use-package webjump
  :bind (("C-M-?" . spike-leung/webjump-symbol-at-point))
  :config
  (setq webjump-sites
        '(("rc" . [simple-query "lib.rs" "https://lib.rs/search?q=" ""])
          ("rsd" . [simple-query "doc.rust-lang.org" "https://doc.rust-lang.org/std/?search=" ""])
          ("ci" . [simple-query "crates.io" "https://crates.io/crates/" ""])
          ("wiki" . [simple-query "wikipedia.org" "wikipedia.org/wiki/" ""])
          ("mdn" . [simple-querry "developer.mozilla.org" "developer.mozilla.org/en-US/search?q=" ""])
          ("google" . [simple-query "google.com" "https://www.google.com/ncr?q=" ""])
          ("g" . [simple-query "google.com.hk" "https://www.google.com.hk/search?q=-youtube+-reddit+-dailymotion+-pbslearningmedia+-dcnewsnow+-ted+-facebook+-douyin+-cctv+-bilibili+-iqiyi+-youku+-tencentvideo+-tiktok+-instagram+-twitter+-cnn+-yahoo+-aljazeera+-foxnews+" ""])
          ("np" . [simple-query "search.nixos.org/packages" "https://search.nixos.org/packages?channel=unstable&size=50&sort=relevance&type=packages&query=" ""])
          ("no" . [simple-query "search.nixos.org/options" "https://search.nixos.org/options?channel=unstable&from=0&size=50&sort=relevance&type=packages&query=" ""])
          ("nd" . [simple-query "discourse.nixos.org" "https://discourse.nixos.org/search?q=" ""])
          ("nw" . [simple-query "wiki.nixos.org" "https://wiki.nixos.org/wiki/" ""])
          ("pr" . [simple-query "nixpk.gs" "https://nixpk.gs/pr-tracker.html?pr=" ""])
          ("aw" . [simple-query "wiki.archlinux.org" "https://wiki.archlinux.org/title/" ""])
          ("pd" . [simple-query "docs.python.org" "https://docs.python.org/zh-cn/3/search.html?q=" ""])
          ("so" . [simple-query "stackoverflow.com" "https://stackoverflow.com/search?q=" ""])
          ("gp" . [simple-query "github.com" "https://github.com/search?q=" ""])
          ("gcn" . [simple-query "github.com" "https://github.com/search?q=language%3Anix+" ""])
          ("gce" . [simple-query "github.com" "https://github.com/search?q=language%3A%22Emacs+Lisp%22+" ""])
          ("gcr" . [simple-query "github.com" "https://github.com/search?q=language%3A%22Rust%22+" ""])
          ("gcl" . [simple-query "github.com" "https://github.com/search?q=language%3A%22Lua%22+" ""])
          ("gcp" . [simple-query "github.com" "https://github.com/search?q=language%3A%22Python%22+" ""])
          ("gcc" . [simple-query "github.com" "https://github.com/search?q=language%3A%22C%22+" ""])
          ("gcp" . [simple-query "github.com" "https://github.com/search?q=language%3AC%2B%2B+" ""])
          ("gi" . [simple-query "github.com" "https://github.com/search?type=issues&q=" ""])
          ("npk" . [simple-query "github.com" "https://github.com/search?q=repo%3ANixOS%2Fnixpkgs+" ""])
          ("tb" . [simple-query "taobao.com" "https://s.taobao.com/search?q=" ""])
          ("tg" . [simple-query "tiger-code.com" "https://tiger-code.com/search?query=" ""])
          ("jlc" . [simple-query "oshwhub.com" "https://oshwhub.com/search?wd=" ""])
          ("th" . [simple-query "thingiverse.com" "https://www.thingiverse.com/search?q=" ""])
          ("cd" . [simple-query "dictionary.cambridge.org" "https://dictionary.cambridge.org/zhs/词典/英语-汉语-简体/" ""])
          ))
  (defun spike-leung/webjump-symbol-at-point ()
    "获取光标下的 symbol 并通过 webjump 搜索。"
    (interactive)
    (let* ((completion-ignore-case t)
           (content (if (use-region-p)
                        (buffer-substring-no-properties (region-beginning) (region-end))
                      (thing-at-point 'symbol t)))
           (query (read-string (format "Webjump search (%s): " (or content ""))
                               nil nil content))
           (item (assoc-string
                  (completing-read "WebJump to site: " webjump-sites nil t)
                  webjump-sites t))
           (name (car item))
           (expr (cdr item))
           (query-prefix (aref expr 2))
           (query-suffix (aref expr 3))
           (fun (if webjump-use-internal-browser
                    (apply-partially #'browse-url-with-browser-kind 'internal)
                  #'browse-url)))
      (funcall fun (webjump-url-fix
                    (cond ((concat query-prefix (webjump-url-encode query) query-suffix))
                          (t (error "WebJump URL expression for \"%s\" invalid"
                                    name)))))
      (call-process "swaymsg" nil nil nil "[app_id=\"firefox-beta$\"] focus")
      (start-process  "fullscreen1" nil  "swaymsg" "fullscreen" )
      )))


(defun export-my-book()
  (interactive)
  (save-excursion
    (find-file (concat myconfigpath "index.org"))
    (org-latex-export-to-pdf)
    (org-html-export-to-html)
    (delete-file "./index-blx.bib" nil)
    (delete-file "./index.bbl" nil)

    (delete-file "./index.tex" nil)
    (find-file (concat myconfigpath "index.pdf"))
    ))



;; TODO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; do window divider mode when window number larger than 1.



;; 文灭志，博溺心，世界嘈杂、瘾品横流、随机与即时构成新的枷锁，而真正的自由来自主动的剥离。当目光离开诱惑、离开广告、离开无意义的瞬息刺激，心才开始变得干净。生活越简，能量越纯；越慢，感受越深；越少，越能看见真正的自己。
;;
;; 居事寂静而世事安宁，身体是意志的第一块土地。空腹的清明、弱光的安静、低温的醒觉、缓慢进食的耐性，都在一点点重塑我们早已被工业习惯磨钝的感官。行走、奔跑、提举、拉起、俯卧撑、壶铃、农夫行走——这些最朴素的动作让人重新理解力量的意义：力量不是爆发，而是日复一日不受伤、不懈怠、让心跳稳、饮食亦是自律的延伸。
;;
;; 避免加工。让精神长的那种沉稳、远离高糖盐油，回到豆果菜、坚果与发酵的本味，让身体习惯真实的能量，而不是被化学甜味与工业脂肪驱使的假饱与假快乐。克制不是苦行，而是温和地恢复本能。
;;
;; 事实是劳动者所知的最美的梦，至于技术，真正的价值不在追逐流行，而在深入底层、理解根本。用 Emacs，不是为了高效，而是为了与世界拉开距离，与自己靠得更近；不随机、不即时、无广告、无噪声，是一种长期的心性训练。读 LFS、LKD 与 SOC 文档，写驱动、调性能、跑 QEMU、玩内核、读源码、用\LaTeX{}和Org写书，是为了获得一种“我真的懂了”的安静感。而这种懂，不是为了炫耀，不是为了沉迷，而是为了让工作成为谋生技能，让生活成为真正的生活。
;;
;; 真正的智慧是：学会技术，然后把技术放下；拥有力量，然后让力量变得温柔。生活是吃饭、睡觉、读书、编程、走路、壶铃；生命是健康、乐观、会意、精进、闲适与稳稳的力量。
;;
;; 世界广袤、事物繁多，而心若清澈，幸福忽然变得极小，也极近——不来自外界，只来自自身安静而坚定的内心。
