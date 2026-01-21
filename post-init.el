;;; -*- lexical-binding: t -*-

(setq-default
 wake-up 0
 book-mode 1
 hide-all 0
 my-alternate-font "Ubuntu Mono Nerd Font"
 my-default-font "bookerly"
 enable-dir-local-variables nil
 shell-file-name "bash"
 wc-regexp-chinese-char-and-punc (rx (category chinese))
 wc-regexp-chinese-punc "[。，！？；：「」『』（）、【】《》〈〉※—]"
 wc-regexp-english-word "[a-zA-Z0-9-]+"
 ;; consult-gh-default-clone-directory "~/"
 ;; consult-gh-default-save-directory "~/"
 ;; org-agenda-files (directory-files-recursively "~/.me/" "\\.org$")
 ;; my/leetcode-root "~/codebase/Leetcode/src/"
 debug-on-error t
 woman-cache-level 3
 epg-pinentry-mode 'loopback      ;使用minibuffer输入密码
 confirm-kill-processes nil
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
 frame-title-format '((:eval (if (buffer-file-name) (abbreviate-file-name (buffer-file-name))"%b")))
 ;; warn when opening files bigger than 100MB
 large-file-warning-threshold 100000000
 ;; Don't create backup files
 make-backup-files nil ; stop creating backup~ files
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
 dictionary-use-single-buffer t
 dictionary-default-dictionary "dict-db-wordnet-542"
 browse-url-firefox-program "firefox-beta"
 ;; browse-url-firefox-arguments "-P firefox"
 browse-url-new-window-flag nil
 ;; browse-url-firefox-new-window-is-tab t
 dired-movement-style nil
 line-number-mode t
 column-number-mode t
 mode-line-position-column-line-format '("%l:%C")
 isearch-allow-scroll t
 package-install-upgrade-built-in nil ;; I am using nix to manage it.
 ;;; no message of revert buffer
 auto-revert-verbose nil
 ;;; no fringe bookmark
 bookmark-fringe-mark nil
 ;;; wdired
 wdired-allow-to-change-permissions t
 wdired-create-parent-directories t
 ;;;  scroll
 scroll-preserve-screen-position t
 scroll-conservatively 101 ; affects scroll-step
 scroll-margin 0
 next-screen-context-lines 0
 ;;; tooltips
 tooltip-hide-delay 20
 tooltip-delay 0.4
 tooltip-short-delay 0.08
 ;;; manual
 woman-fill-column 75
 woman-cache-level 3
 fill-column 75
 Man-width 75
 ;;; magit
 magit-log-margin-show-committer-date t
 magit-log-margin '(t age magit-log-margin-width t 3)
 ;;; compilation
 compilation-window-height nil
 compilation-scroll-output nil
 compilation-auto-jump-to-first-error nil
 ;;; duplicate
 duplicate-line-final-position 1
 duplicate-region-final-position 1
 treesit-font-lock-level 1              ; bro I dont even use font lock
 font-lock-maximum-decoration nil
 resize-mini-windows t
 read-buffer-completion-ignore-case t
 read-file-name-completion-ignore-case t
 visible-cursor nil
 x-stretch-cursor nil
 mode-line-end-spaces nil
 isearch-allow-motion t
 bidi-inhibit-bpa t
 cursor-type 'box
 auto-mode-case-fold nil
 inhibit-compacting-font-caches nil
 bidi-display-reordering 'left-to-right
 bidi-paragraph-direction 'left-to-right
 buffer-file-coding-system 'utf-8
 indent-tabs-mode nil
 indicate-buffer-boundaries nil
 ;; hs-hide-comments-when-hiding-all nil
 )

(require 'use-package)

(use-package psession
  :init
  (let ((nonono "~/.emacs.d/var/elisp-objects/buffer-name-history.elc"))
    (when (file-exists-p nonono) (delete-file nonono)))
  :config
  ;; (set-language-environment "UTF-8")
  ;; (prefer-coding-system 'utf-8-unix)
  (set-face-attribute 'default nil :font my-default-font :height 190)
  ;; (add-hook 'prog-mode-hook 'hs-minor-mode)
  ;; Why people keep folding and unfolding *shit*. we have imenu!
  (add-hook 'after-make-frame-functions
            (defun setup-blah-keys (frame)
              (with-selected-frame frame
                (when (display-graphic-p)
                  (define-key input-decode-map (kbd "C-i") [CTRL-i])
                  (define-key input-decode-map (kbd "C-S-i") [CTRL-Shift-i])
                  (define-key input-decode-map (kbd "C-[") [CTRL-lsb]) ; left square bracket
                  (define-key input-decode-map (kbd "C-m") [CTRL-m])
                  (define-key input-decode-map (kbd "C-S-m") [CTRL-Shift-m])
                  ))))
  (setq psession-object-to-save-alist
        (assq-delete-all 'buffer-name-history psession-object-to-save-alist))
  (setf (alist-get 'truncation fringe-indicator-alist) '(nil nil))
  (setf (alist-get 'continuation fringe-indicator-alist) '(nil nil))
  (psession-mode 1)
  (psession-savehist-mode 1)
  (psession-autosave-mode 1))

(use-package consult
  :ensure t
  :demand t
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.5)
  (setq register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref)
  (setq xref-show-definitions-function #'consult-xref)
  :config
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (add-hook 'after-init-hook #'minibuffer-depth-indicate-mode)
  (rg-enable-default-bindings)
  (move-text-default-bindings)
  (use-package surround
    :bind-keymap
    ("C-<tab>" . surround-keymap))
  (setq consult-async-input-debounce 0.02
        consult-async-input-throttle 0.05
        consult-async-refresh-delay 0.02)
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<")
  ;; stop addiction of configging emacs here. 我的人生大约是废了。
  (global-unset-key (kbd "C-z"))        ;hey, what's going on? and I say hayayayayayyay.
  :bind (
         ([remap comment-line] . (lambda ()"comment-or-uncomment-region-or-line, Like comment-or-uncomment-region, but if there's no mark \(that means no region\) apply comment-or-uncomment to the current line"(interactive) (if (not mark-active)(comment-line 1) (if (< (point) (mark))(comment-or-uncomment-region (point) (mark)) (comment-or-uncomment-region (mark) (point))))))
         ([remap completion-at-point] . hippie-expand)
         ([remap complete-symbol] . hippie-expand)
         ([remap imenu] . consult-imenu)
         ([remap indent-rigidly] . (lambda()"cleanup-buffer, Perform a bunch of operations on the whitespace content of a buffer. Including indent-buffer, which should not be called automatically on save."(interactive) (let ((start (if (region-active-p) (region-beginning) (point-min)))(end (if (region-active-p) (region-end) (point-max)))) (untabify start end)(check-parens) (indent-region start end)(call-interactively 'delete-trailing-whitespace) (set-buffer-file-coding-system 'utf-8))))
         ([remap list-buffers] . ibuffer)
         ([remap project-switch-to-buffer] . consult-project-buffer)
         ([remap kill-buffer] . kill-current-buffer)
         ([remap switch-to-buffer] . consult-buffer)
         ([remap yank-pop] . consult-yank-pop)
         ;; stop hurt my finger girls here. 我的小姆指大约是废了。
         ;; 防止右手内卷
         ;; ("C-." . (lambda () (interactive) (message "别再内卷自己的右手姑娘了")))
         ;; ("C-," . (lambda () (interactive) (message "别再内卷自己的右手姑娘了")))
         ("M-." . (lambda () (interactive) (message "别再内卷自己的右手姑娘，用C-i")))
         ("M-," . (lambda () (interactive) (message "别再内卷自己的右手姑娘，用s-u")))
         ;; 爱护我们的左手小拇指
         ("C-a" . (lambda () (interactive) (message "为了我的小姆指!!!!!!! 用 <Home>!!")))
         ("M-a" . (lambda () (interactive) (message "为了我的小姆指!!!!!!! 这玩意本来就没啥用吧! 用avy!")))
         ("M-z" . (lambda () (interactive) (message "为了我的小姆指!!!!!!! 这玩意本来就没啥用吧! 用M-x招唤吧!")))
         ("C-c C-; z" . (lambda () (interactive) (message "我不再使用isearch-forward-symbol-at-point用emabark-symbol or occur/rg 它不香吗")))
         ("C-c C-~ z" . (lambda () (interactive) (message "我不再使用navz来删窗口, navd它不香吗?")))
         ;; 爱护我们的右手小拇指
         ("<end>" . (lambda () (interactive) (message "为了我的小姆指!!!!!!! 用 C-e!!")))
         ("<right>" . (lambda () (interactive) (message "为了我的小姆指!!!!!!! 用 nav层的f!!"))) ; 有些时候，我就是不喜欢适应。去tm的世界。
         ("C-<right>" . (lambda () (interactive) (message "为了我的小姆指!!!!!!! 用m-f不是更爽吗!!"))) ; 有些时候，我就是不喜欢适应。去tm的世界。
         ("C-S-<right>" . (lambda () (interactive) (message "为了我的小姆指!!!!!!! 用m-s-f不是更爽吗!!"))) ; 有些时候，我就是不喜欢适应。去tm的世界。
         ("C-M-S-<right>" . (lambda () (interactive) (message "为了我的小姆指!!!!!!! 用C-m-s-f不是更爽吗!!"))) ; 有些时候，我就是不喜欢适应。去tm的世界。
         ("C-/" . (lambda () (interactive) (message "为了我的小姆指!!!!!!! 用 C-f 来撤回吧!!")))
         ("C-?" . (lambda () (interactive) (message "把小姆指留着挖鼻屎，用C-b 来重做，用食指来代劳吧!")))
         ("M-/" . (lambda () (interactive) (message "为了我的小姆指!   别tm的用这个补全了，用M-return和M-tab对就干!!")))
         ("M-o" . (lambda () (interactive) (message "为了我的小姆指!   别tm的用这个换窗口了，用M-e，反正我不是写小说的。")))
         ("M-O" . (lambda () (interactive) (message "为了我的小姆指!   别tm的用这个换窗口了，用M-E，后正我不是写论文的。")))
         ;; 也要爱护我们的食指，它可是各管着六个键的啊!
         ("C-x k" . (lambda () (interactive) (message "别tm的kill-buffer癌了, Use Ibuffer 杀(窗)口 !!!")))
         ;; ("C-c C-; k" . (lambda () (interactive) (message "别tm的kill-buffer癌了, Use Ibuffer 杀(窗)口 !!!")))
         ("C-\\" . (lambda () (interactive) (message "别tm的用这个换输入法了，明明不用伸食指就行!")))
         ("<WakeUp>". (lambda () (interactive) (if  (= wake-up 2) (monitor) (setq wake-up (1+ wake-up)))))
         ("ESC <f5>". hibernatecall)    ;电脑用来睡觉的键
         ;; ----------------------------------------
         ;; No Need "qbj'" if in inner
         ;; No Need "qbj'vk"  if in middle
         ;; No Need all if in outter, take care of your thumb, they are finger killers
         ;; use less z/ ao q', because little finger are easy to get hurt.
         ;; mos arstb are specific to native emacs binding
         ;; ("C-." . hs-toggle-hiding)
         ;; ("C-," . (lambda () (interactive) (if  (= hide-all 0) (progn (hs-hide-all)(setq hide-all 1)) (progn (hs-show-all)(setq hide-all 0)))))
         ("C-f" . undo)              ;; 我也不用这个啥c-bnfp来上下左右啊
         ("C-b" . undo-redo)
         ("M-g n" . my-next-error)
         ("M-g p" . my-previous-error)
         ("C-p" . goto-last-change)        ;; GOAT
         ("M-p". goto-last-change-reverse)  ;这两本来就没bind啊
         ;; ----------------------------------------
         ;; M-<backspace>，backward-kill-word 爽爽爽爽!
         ("C-<backspace>" . duplicate-dwim)
         ("S-<backspace>" . kill-sexp)
         ;; ----------------------------------------
         ("C-M-<return>" . avy-copy-region)
         ("C-S-<return>" . avy-goto-line)
         ("C-<return>" . avy-goto-word-0)
         ("S-<return>" . comment-indent-new-line) ; 下一行注释
         ;; ----------------------------------------
         ;; 想想 C-SPC C-SPC 就是做标记，而M-SPC就是去标记点，爽了吧。
         ("M-SPC" . consult-mark) ; Why I need Bloate Emacs Bindings?
                                        ; I just need a brave new world. give me soma!!!!
         ;; ----------------------------------------
         ;; M-<tab>用来补全，其它的用来修定 alt+tab
         ;; ("C-<tab>" . surround-mark)           ; ctrl+tab
         ("<backtab>" . surround-insert)       ; shift+ tab加括号
         ("C-<iso-lefttab>" . surround-delete) ; ctrl+shift+tab删括号
         ("C-M-<tab>" . surround-kill)         ; ctrl+alt+tab删括号内外
         ("s-<tab>" . surround-change)         ; ctrl+meta+tab改括号
         ;; ----------------------------------------
         ("C-v". (lambda() (interactive)(recenter-top-bottom 0))) ; use page up and down
         ("M-v". (lambda() (interactive)(recenter-top-bottom 38))) ; now, they are for top view and bottom view
         ("<CTRL-m>" . (lambda () (interactive) (start-process "wtype" nil "wtype" ";")))
         ;; all just for my little finger...
         ("<CTRL-Shift-m>" . (lambda () (interactive) (start-process "wtype" nil "wtype" ":")))
         ;; 中文: 我的小拇指很重要。 但是;就在小拇指处，elisp要用;注释，c要用;断句。
         ;; BTW，c-i 是用于go-to的。
         ("M-i". imenu)
         ("M-I" . consult-imenu-multi)
         ("M-e". other-window)
         ("M-E". (lambda () (interactive) (other-window -1)))
         ("M-*". (lambda () (interactive) (my/leetcode-open (string-to-number(current-word)))))
         ("C-*" . toggle-solution-question)
         ("C-%". iedit-mode)
         ;; ("C-,". magit-diff-dwim)
         ("M-+" . rotate-text)
         ("M-_" . rotate-text-backward)
         ("C-n" . donothing)
         ("M-n". donothing)

         ;;;; 我有主要有NAV/MOS/SYM/(NUM: 通过Sway调用:not anymore, I just all emacs now!) who need wtf os/wm... just using Shitty emacs.
         ;; MOS-begin-----------------------------------------------------------------
         ("C-c C-; d" .  dired)
         ("C-c C-; k" .  (lambda () (interactive) (save-selected-window (other-window 1) (isearch-forward))))
         ("C-c C-; f" . magit-dispatch)
         ("C-c C-; g" . er/expand-region)
         ("C-c C-; p" . consult-bookmark)
         ("C-c C-; m" . devdocs-browser-open)
         ("C-c C-; u" . delete-all-space)
         ("C-c C-; v" . multi-vterm-project)
         ("C-c C-; y" . yas-insert-snippet)
         ("C-c C-; x" . multi-vterm-dedicated-toggle)
         ("C-c C-; c" . compile)
         ("C-c C-; l" . git-link-dispatch)
         ("C-c C-; w" . (lambda () "completion with buffer/file name and time"(interactive) (if (get-buffer-window "*Completions*") (delete-window (get-buffer-window "*Completions*")) (completion-in-region (point) (point) (list (format "%s" (buffer-name)) (format "%s" (if (equal major-mode 'dired-mode) default-directory (buffer-file-name))) (format "处于第%s行 第%s列 %s函数中" (line-number-at-pos)(current-column) (which-function)) (format "现在是%s点钟 %s" (format-time-string "%k")(if (> 30 (string-to-number (format-time-string "%M")))
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          "未过半" "过半")))))))
         ;; 当其无，有手之用
         ("C-c C-; q" . quick-sdcv-search-at-point)
         ("C-c C-; j" . donothing)
         ("C-c C-; '" . donothing)
         ;; MOS-end-----------------------------------------------------------------

         ;; NAV-begin--------------------------------------------------------------
         ("C-c C-~ x" .  delete-other-windows)
         ("C-c C-~ c" .  split-window-below)
         ("C-c C-~ d" .  delete-window)
         ("C-c C-~ g" .  magit-status)
         ("C-c C-~ j" .  consult-ripgrep-symbol-at-point)
         ("C-c C-~ l" .   (lambda () "Search for a line matching the symbol found near point." (interactive) (consult-line (or (thing-at-point 'symbol)))))
         ("C-c C-~ u" . occur) ; it's better than consult-line-symbol-at-point
         ("C-c C-~ y" . (lambda () (interactive) (if (< (count-windows) 2) (progn (split-window) (other-window 1) (switch-to-buffer (cl-find-if (lambda (buf) (with-current-buffer buf (derived-mode-p '(eww-mode help-mode occur-mode compilation-mode Info-mode rg-mode man-mode woman-mode )))) (buffer-list))) (other-window 1)) (delete-other-windows) )))
         ("C-c C-~ f" . right-char)
         ("C-c C-~ w" . right-word) ;论我为什么要用occur和这么多rg
         ("C-c C-~ k" . woman)
         ;; rg用在真实文件好用，但是不能用在虚文件中，occur可以
         ;; rg比起occur更灵活，能指定文件种类/路径，能非常好直接修改。
         ("C-c C-~ m" . (lambda () (interactive)
                          (if (get-buffer-window "*compilation*")
                              (recompile)
                            (progn (recompile)
                                   ;; (delete-window (get-buffer-window "*compilation*"))
                                   (switch-to-buffer "*compilation*")
                                   (previous-buffer)))))
         ("C-c C-~ p" . rg-dwim)        ; stop use WTF Occur, because rg is KING.
         ("C-c C-~ b" . rg-dwim-current-file)
         ;; 当其无，有心之用
         ("C-c C-~ v" . magit-log-buffer-file)
         ("C-c C-~ q" .  donothing)
         ("C-c C-~ '" .  donothing)
         ;; NAV-end---------------------------------------------------------

         ;; SYM-begin-------------------------------------------------------
         ("C-c C-& j" . my-toggle-font)
         ("C-c C-& u" . (lambda () (interactive) (if vertico-unobtrusive-mode (vertico-unobtrusive-mode 1) (vertico-unobtrusive-mode -1)) (setq vertico-unobtrusive-mode (not vertico-unobtrusive-mode))))
         ("C-c C-& y" . (lambda () "to toggle my read-book-mode" (interactive) (if book-mode (progn (setq-default fontfont 1 line-spacing 0.3 woman-fill-column 70 Man-width 70 fill-column 70 font-me 6) (my-toggle-font 6) (text-scale-set 0)) (progn (setq-default fontfont 1 line-spacing nil woman-fill-column 80 Man-width 80 fill-column 80 font-me 3) (my-toggle-font 3) (text-scale-set 0))) (setq book-mode (not book-mode)))) ("C-c C-& l" . global-hide-mode-line-mode) ("C-c C-& m" . toggle-truncate-lines) ("C-c C-& h" . git-timemachine-toggle) ("C-c C-& ," . toggle-input-method)
         ("C-c C-& ." . (lambda () "Toggle the letter case of current word or text selection. Toggles between: “all lower”, “Init Caps”, “ALL CAPS”." (interactive) (let (p1 p2 (deactivate-mark nil) (case-fold-search nil)) (if (region-active-p) (setq p1 (region-beginning) p2 (region-end)) (let ((bds (bounds-of-thing-at-point 'word))) (setq p1 (car bds) p2 (cdr bds)))) (when (not (eq last-command this-command)) (save-excursion (goto-char p1) (cond ((looking-at "[[:lower:]][[:lower:]]") (put this-command 'state "all lower")) ((looking-at "[[:upper:]][[:upper:]]") (put this-command 'state "all caps")) ((looking-at "[[:upper:]][[:lower:]]") (put this-command 'state "init caps")) ((looking-at "[[:lower:]]") (put this-command 'state "all lower")) ((looking-at "[[:upper:]]") (put this-command 'state "all caps")) (t (put this-command 'state "all lower"))))) (cond ((string= "all lower" (get this-command 'state)) (upcase-initials-region p1 p2) (put this-command 'state "init caps")) ((string= "init caps" (get this-command 'state)) (upcase-region p1 p2) (put this-command 'state "all caps")) ((string= "all caps" (get this-command 'state)) (downcase-region p1 p2) (put this-command 'state "all lower"))))) )
         ("C-c C-& DEL" . (lambda () "delete many blank line, and left one for you :)" (interactive) (let (xbegin xend) (if (region-active-p) (setq xbegin (region-beginning) xend (region-end)) (setq xbegin (point-min) xend (point-max))) (save-excursion (save-restriction (narrow-to-region xbegin xend) (goto-char (point-min)) (while (re-search-forward "[ \t]+\n" nil 1) (replace-match "\n")) (goto-char (point-min)) (while (re-search-forward "\n\n\n+" nil 1) (replace-match "\n\n")) (goto-char (point-max)) (while (eq (char-before) 32) (delete-char -1))))) (message "%s done" real-this-command)))
         ("C-c C-& k" . (lambda () "count chinese word number" (interactive) (let* ((words-to-be-counted (if mark-active (buffer-substring-no-properties (region-beginning) (region-end)) (buffer-substring-no-properties (point-min) (point-max)))) (v-buffer-string (progn (if (eq major-mode 'org-mode) (setq v-buffer-string (replace-regexp-in-string "^#\\+.+" "" words-to-be-counted)) (setq v-buffer-string words-to-be-counted)) (replace-regexp-in-string (format "^ *%s *.+" comment-start) "" v-buffer-string))) (chinese-char-and-punc 0) (chinese-punc 0) (english-word 0) (chinese-char 0)) (with-temp-buffer (insert v-buffer-string) (goto-char (point-min)) (while (re-search-forward wc-regexp-chinese-char-and-punc nil :no-error) (setq chinese-char-and-punc (1+ chinese-char-and-punc))) (goto-char (point-min)) (while (re-search-forward wc-regexp-chinese-punc nil :no-error) (setq chinese-punc (1+ chinese-punc))) (goto-char (point-min)) (while (re-search-forward wc-regexp-english-word nil :no-error) (setq english-word (1+ english-word)))) (setq chinese-char (- chinese-char-and-punc chinese-punc)) (message (format "中文字数(不含标点)：%s \n 中文字数(包含标点)：%s \n 英文字数(不含标点)：%s \n ======================== \n 中英文合计(不含标点)：%s \n 中英文合计(包含标点)：%s \n %s" chinese-char chinese-char-and-punc english-word (+ chinese-char english-word) (+ chinese-char-and-punc english-word) (format "%s" (funcall-interactively 'count-words (region-beginning) (region-end))))))))
         ;; 得之心而运之于手
         ("C-c C-& /" . donothing)
         ("C-c C-& '" . donothing)
         ;; SYM-end-------------------------------------------------------

         ;; NUM-begin--------------------------------------------
         ("s-m" . (lambda () "switch to rg/compilation/occur buffer" (interactive) (if (derived-mode-p '(compilation-mode rg-mode occur-mode)) (previous-buffer) (switch-to-buffer (cl-find-if (lambda (buf) (with-current-buffer buf (derived-mode-p '(compilation-mode rg-mode occur-mode))))                                                                                                (buffer-list))))))
         ("s-h" . (lambda () "切换去eww/help" (interactive) (if (derived-mode-p '(eww-mode help-mode)) (previous-buffer) (switch-to-buffer (cl-find-if (lambda (buf) (with-current-buffer buf (derived-mode-p '(eww-mode help-mode)))) (buffer-list))))))
         ("s-<" . (lambda () "切换去info/woman/man" (interactive) (if (derived-mode-p '(Info-mode woman-mode man-mode)) (previous-buffer) (switch-to-buffer (cl-find-if (lambda (buf) (with-current-buffer buf (derived-mode-p '(Info-mode woman-mode man-mode)))) (buffer-list))))))
         ("s->" . (lambda () "切换去两个最近使用的c/c++/zig buffer" (interactive) (if (derived-mode-p '(c-mode c++-mode zig-mode)) (progn (setq my-buffer-name (buffer-name)) (switch-to-buffer (cl-find-if (lambda (buf) (with-current-buffer buf (and (derived-mode-p '(c-mode c++-mode zig-mode)) (not (eq (buffer-name) my-buffer-name))))) (buffer-list)))) (switch-to-buffer (cl-find-if (lambda (buf) (with-current-buffer buf (derived-mode-p '(c-mode c++-mode zig-mode)))) (buffer-list))))))
         ("s-j" . (lambda () (interactive)(find-file "~/.emacs.d/post-init.el")))
         ("s-\"" . (lambda () (interactive)(find-file "~/.me/flake.nix")))
         ("s-u" . previous-buffer)
         ("s-l" . next-buffer)
         ;; NUM-end--------------------------------------------

         ;; minor keycut ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         ("M-#" . consult-register-load)
         ("M-$" . consult-register-store)
         ("C-M-#" . consult-register)
         ("M-g M-g" . avy-goto-line)
         ("M-g g" . consult-goto-line)
         ("M-s b" . magit-blame-addition)
         ("M-s k" . flush-lines)
         ("M-s K" . consult-keep-lines)
         ("M-s M-k" . avy-kill-whole-line)
         ("M-s d" . delete-duplicate-lines)
         ("M-s u" . (lambda () "url-to-hosts, Convert a URL on the current line to '    0.0.0.0 hostname' format." (interactive) (let* ((line (thing-at-point 'line t)) (url (when line (string-match "https?://\\([^/]+\\)" line) (match-string 0 line))) (hostname (when url (replace-regexp-in-string "^https?://\\([^/]+\\).*" "\\1" url)))) (when hostname (beginning-of-line) (kill-line) (insert (concat "0.0.0.0 " hostname)) (beginning-of-line)))))
         ("M-s w" .  (lambda () "Search GitHub code for current major mode's language. Uses word at point as default, or prompts for input." (interactive) (let* ( (language (pcase major-mode ('zig-mode "zig") ('rust-mode "rust") ('python-mode "python") ('emacs-lisp-mode "emacs-lisp") ('go-mode "go") ('javascript-mode "javascript") ('typescript-mode "typescript") (_ (read-string "Language: ")))) (input (read-string (format "Search %s code: " language) ))) (consult-gh-search-code (format "%s -- --language=%s --limit 7" input language) nil nil nil nil))))
         ("M-s f" . consult-fd)
         ("M-s o" . multi-occur)
         ("M-s SPC" . consult-global-mark)
         ;; ("<f19>" . (lambda () "Rotate your windows." (interactive) (cond ((not (> (count-windows)1)) (message "You can't rotate a single window!")) (t (setq i 1) (setq numWindows (count-windows)) (while  (< i numWindows) (let* ( (w1 (elt (window-list) i)) (w2 (elt (window-list) (+ (% i numWindows) 1))) (b1 (window-buffer w1)) (b2 (window-buffer w2)) (s1 (window-start w1)) (s2 (window-start w2)) ) (set-window-buffer w1 b2) (set-window-buffer w2 b1) (set-window-start w1 s2) (set-window-start w2 s1) (setq i (1+ i))))))))
         :map prog-mode-map
         ("M-<return>" . dabbrev-expand) ; 和alt-tab 针锋相对
         :map isearch-mode-map
         ("C-\\" . (lambda () (interactive) (message "别拉伸宝贵的食指了")))
         ("M-r" . consult-isearch-history)
         :map minibuffer-local-map
         ("M-r" . consult-history)
         :map transient-map
         ("M-w". transient-copy-menu-text)
         ))

(use-package pyim
  :ensure t
  :defer t
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
        (find-file "~/.local/share/mysource/hmdz.pyim")
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
  (setq pyim-page-tooltip '(minibuffer popup posframe))
  (setq pyim-dicts nil)  ; Initialize the list if it's not already defined
  (setq pyim-cloudim nil)
  (setq pyim-candidates-search-buffer-p nil)
  (setq pyim-enable-shortcode nil)
  (setq pyim-punctuation-dict '(("^" "…")("\\" "、")("." "。")("," "，")("'" "‘" "’") ("\"" "“" "”")))
  (when (file-exists-p "~/.local/share/mysource/hmdz.pyim") (add-to-list 'pyim-dicts '(:name "hmdz" :file "~/.local/share/mysource/hmdz.pyim"))))

(use-package quick-sdcv
  :defer t
  :bind ( :map quick-sdcv-mode-map
          ("SPC" . scroll-up-command)
          ("DEL" . scroll-down-command)
          ("y" . (lambda () (interactive) (previous-buffer) (other-window 1)))
          ("u" . delete-window)))

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
  ;; (defun my/suppress-recentf-messages (orig-fun &rest args)
  ;;   "Run `recentf-cleanup` without showing messages in the echo area."
  ;;   (let ((inhibit-message t)
  ;;         (message-log-max nil))
  ;;     (apply orig-fun args)))
  (add-hook 'kill-emacs-hook #'recentf-cleanup -90)
  ;; (advice-add 'recentf-cleanup :around #'my/suppress-recentf-messages)
  )

(use-package super-save
  :config
  (super-save-mode 1)
  (setq
   super-save-silent t
   super-save-all-buffers  t
   super-save-idle-duration 4
   super-save-remote-files nil
   super-save-auto-save-when-idle t
   ))

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

(use-package undo-fu-session
  :ensure t
  :commands undo-fu-session-global-mode
  :config
  (use-package undo-fu
    :ensure t
    :commands (undo-fu-only-undo
               undo-fu-only-redo
               undo-fu-only-redo-all
               undo-fu-disable-checkpoint)
    :bind (
           ([remap undo] . undo-fu-only-undo)
           ([remap undo-redo] . undo-fu-only-redo))
    :custom
    (undo-fu-allow-undo-in-region nil))
  :hook (after-init . undo-fu-session-global-mode))

(use-package saveplace
  :ensure nil
  :commands (save-place-mode save-place-local-mode)
  :hook
  (after-init . save-place-mode)
  :custom
  (save-place-forget-unreadable-files nil)
  (save-place-limit 400)
  (save-place-file
   (expand-file-name "saveplace" user-emacs-directory)))

(use-package vterm
  :ensure t
  :defer t
  :commands (vterm--internal multi-vterm-project)
  :bind (:map vterm-mode-map
              ("C-p" . vterm-copy-mode)
              ("M-e" . other-window)
              ("C-c C-~ f" . (lambda () (interactive) (start-process "wtype" nil "wtype" "-M" "ctrl" "f" "-m" "ctrl")))
              ("C-c C-~ w" . (lambda () (interactive) (start-process "wtype" nil "wtype" "-M" "alt" "f" "-m" "alt"))) ;论我为什么要用occur和这么多rg
              :map vterm-copy-mode-map
              ("C-p" . vterm-previous-prompt)
              ("C-f" . vterm-next-prompt )
              ("C-<return>" . compile-goto-error))
  :config
  (setq vterm-shell "fish")
  (setq vterm-always-compile-module t)
  (setq vterm-timer-delay 0.01)
  (setq vterm-kill-buffer-on-exit t)
  (add-hook 'vterm-mode-hook #'compilation-shell-minor-mode 1)
  ;; (with-eval-after-load 'vterm
  ;;   ;; (advice-add 'vterm :after
  ;;   ;;             (lambda (buf)
  ;;   ;;               (with-current-buffer buf
  ;;   ;;                 (set-process-query-on-exit-flag
  ;;   ;;                  (get-buffer-process (current-buffer)) nil))))
  ;;   )
  )

(use-package magit
  :defer t
  :config
  ;; (use-package magit-todos
  ;;   :after magit
  ;;   :config
  ;;   (magit-todos-mode 1))
  (use-package forge
    :after magit)
  (use-package diff-mode
    :custom
    (diff-default-read-only t)
    (diff-font-lock-syntax 'hunk-also)
    (diff-font-lock-prettify t))
  (setq magit-bury-buffer-function 'magit-restore-window-configuration)
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  (setq magit-diff-refine-hunk  'all)
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

(use-package avy
  :ensure t
  :config
  (setq avy-all-windows t)
  (setq avy-keys '( ?t ?n ?r ?i ?s ?e))
  (avy-setup-default)
  (defun avy-action-mark-to-char (pt)
    (activate-mark)
    (goto-char pt))
  (setf (alist-get ?  avy-dispatch-alist) 'avy-action-mark-to-char ))

(use-package trashed
  :ensure t
  :defer t
  :commands (trashed)
  :config
  (setq trashed-action-confirmer 'y-or-n-p
        trashed-use-header-line t
        trashed-sort-key '("Date deleted" . t)
        trashed-date-format "%m-%d %H:%M:%S"))

(use-package dired
  :ensure nil
  :commands (dired)
  :bind
  ( :map dired-mode-map
    ("SPC" . scroll-up-command)
    ("p" . (lambda () (interactive)(dired-find-file-other-window) (other-window 1)(next-line)))
    ("DEL" . scroll-down-command)
    ("," . dired-omit-mode)
    ("h" . dired-do-eww)
    ("<tab>" . dired-subtree-toggle)
    ("e" . wdired-change-to-wdired-mode))
  :hook
  ((dired-mode . dired-hide-details-mode)
   (dired-mode . dired-omit-mode))
  :config
  ;; (use-package dired-rsync
  ;;   :bind (:map dired-mode-map
  ;;               ("C-c C-r" . dired-rsync)))
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
    :after dired
    :defer t
    :commands (dired)
    :config
    (setq dired-subtree-use-backgrounds nil))
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t)
  (with-eval-after-load 'dired
    (let ((args "--group-directories-first -ahlv"))
      (when (or (eq system-type 'darwin) (eq system-type 'berkeley-unix))
        (if-let* ((gls (executable-find "gls")))
            (setq insert-directory-program gls)
          (setq args nil)))
      (when args
        (setq dired-listing-switches args))))
  :custom
  (dired-omit-files
   (rx (or (seq bol ".")                      ; dotfiles
           ;; (seq ".js" (? ".meta") eos)        ; .js.meta
           ;; (seq "." (or "elc" "cls" "fls" "bib" "fdb_latexmk" "log" "aux" "a" "bbl" "o" "pyc" "pyo" "swp" "class") eos)
           ;; (seq bol ".DS_Store")
           ;; (seq bol ".ccls-cach" eos)
           ;; (seq bol "__pycache__" eos)
           ;; (seq bol ".project" (? "ile") eos)
           ;; (seq bol (or "flycheck_" "flymake_"))
           (seq bol "." (or "svn" "git") eos)
           (seq bol (or "flake.lock" "Cargo.lock" "LICENSE" "LICENCE" "Downloads") eos)
           ))))

(use-package aggressive-indent
  :defer t
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode))

(use-package compile
  :config
  ;; (defun my/nix-store-shorten-paths ()
  ;;   "Replace long /nix/store paths with shortened ...-pkg-version."
  ;;   (let ((inhibit-read-only t))
  ;;     (goto-char (point-min))
  ;;     (delete-region (line-beginning-position) (1+ (line-end-position)))
  ;;     (while (re-search-forward
  ;;             "/nix/store/[a-z0-9]+-\\([^[:space:]]+\\)" nil t)
  ;;       (replace-match "...-\\1" t nil)))
  ;;   (goto-char (point-max))
  ;;   (next-line)
  ;;   (delete-region (line-beginning-position) (line-end-position)))
  (defun compilation-start (command &optional mode name-function highlight-regexp continue) "Run compilation command COMMAND (low level interface). If COMMAND starts with a cd command, that becomes the `default-directory'. The rest of the arguments are optional; for them, nil means use the default. MODE is the major mode to set in the compilation buffer.  Mode may also be t meaning use `compilation-shell-minor-mode' under `comint-mode'. If NAME-FUNCTION is non-nil, call it with one argument (the mode name) to determine the buffer name.  Otherwise, the default is to reuses the current buffer if it has the proper major mode, else use or create a buffer with name based on the major mode. If HIGHLIGHT-REGEXP is non-nil, `next-error' will temporarily highlight the matching section of the visited source line; the default is to use the global value of `compilation-highlight-regexp'. If CONTINUE is non-nil, the buffer won't be emptied before compilation is started.  This can be useful if you wish to combine the output from several compilation commands in the same buffer.  The new output will be at the end of the buffer, and point is not changed. Returns the compilation buffer created." (or mode (setq mode 'compilation-mode)) (let* ((name-of-mode (if (eq mode t) "compilation" (replace-regexp-in-string "-mode\\'" "" (symbol-name mode)))) (thisdir default-directory) (thisenv compilation-environment) (buffer-path (and (local-variable-p 'exec-path) exec-path)) (buffer-env (and (local-variable-p 'process-environment) process-environment)) outwin outbuf) (with-current-buffer (setq outbuf (get-buffer-create (compilation-buffer-name name-of-mode mode name-function))) (let ((comp-proc (get-buffer-process (current-buffer)))) (if comp-proc (if (or (not (eq (process-status comp-proc) 'run)) (eq (process-query-on-exit-flag comp-proc) nil) (yes-or-no-p (format "A %s process is running; kill it? " name-of-mode))) (condition-case () (progn (interrupt-process comp-proc) (sit-for 1) (delete-process comp-proc)) (error nil)) (error "Cannot have two processes in `%s' at once" (buffer-name))))) (setq default-directory thisdir) (let ((inhibit-read-only t) (default-directory thisdir)) (cd (cond ((not (string-match "\\`\\s *cd\\(?:\\s +\\(\\S +?\\|'[^']*'\\|\"\\(?:[^\"`$\\]\\|\\\\.\\)*\"\\)\\)?\\s *[;&\n]" command)) default-directory) ((not (match-end 1)) "~") ((eq (aref command (match-beginning 1)) ?\') (substring command (1+ (match-beginning 1)) (1- (match-end 1)))) ((eq (aref command (match-beginning 1)) ?\") (replace-regexp-in-string "\\\\\\(.\\)" "\\1" (substring command (1+ (match-beginning 1)) (1- (match-end 1))))) (t (let* ((substituted-dir (substitute-env-vars (match-string 1 command))) (expanded-dir (file-expand-wildcards substituted-dir))) (if (= (length expanded-dir) 1) (car expanded-dir) substituted-dir))))) (if continue (progn (setq continue (point)) (goto-char (point-max))) (erase-buffer)) (if (not (eq mode t)) (progn (buffer-disable-undo) (funcall mode)) (setq buffer-read-only nil) (with-no-warnings (comint-mode)) (compilation-shell-minor-mode)) (setq-local compilation-directory thisdir) (setq-local compilation-environment thisenv) (if buffer-path (setq-local exec-path buffer-path) (kill-local-variable 'exec-path)) (if buffer-env (setq-local process-environment buffer-env) (kill-local-variable 'process-environment)) (if highlight-regexp (setq-local compilation-highlight-regexp highlight-regexp)) (if (or compilation-auto-jump-to-first-error (eq compilation-scroll-output 'first-error)) (setq-local compilation-auto-jump-to-next t))  (compilation-insert-annotation command "\n") (setq compilation--start-time (float-time)) (setq thisdir default-directory)) (set-buffer-modified-p nil)) (setq outwin (display-buffer outbuf '(nil (allow-no-window . t)))) (with-current-buffer outbuf (let ((process-environment (append compilation-environment (and (derived-mode-p 'comint-mode) (comint-term-environment)) (list (format "INSIDE_EMACS=%s,compile" emacs-version)) (list "PAGER=") (copy-sequence process-environment)))) (setq-local compilation-arguments (list command mode name-function highlight-regexp)) (setq-local revert-buffer-function 'compilation-revert-buffer) (when (and outwin (not continue) (not compilation-scroll-output)) (set-window-start outwin (point-min))) (let ((desired-visible-point (cond (continue continue) (compilation-scroll-output (point-max)) (t (point-min))))) (goto-char desired-visible-point) (when (and outwin (not (eq outwin (selected-window)))) (set-window-point outwin desired-visible-point))) (if compilation-process-setup-function (funcall compilation-process-setup-function)) (and outwin (compilation-set-window-height outwin)) (if (fboundp 'make-process) (let ((proc (if (eq mode t) (with-connection-local-variables (get-buffer-process (with-no-warnings (comint-exec outbuf (compilation--downcase-mode-name mode-name) shell-file-name nil `(,shell-command-switch ,command))))) (start-file-process-shell-command (compilation--downcase-mode-name mode-name) outbuf command)))) (setq mode-line-process '((:propertize ":%s" face compilation-mode-line-run) compilation-mode-line-errors)) (when compilation-always-kill (set-process-query-on-exit-flag proc nil)) (set-process-sentinel proc #'compilation-sentinel) (unless (eq mode t) (set-process-filter proc #'compilation-filter)) (set-marker (process-mark proc) (point-max) outbuf) (when compilation-disable-input (condition-case nil (process-send-eof proc) (error nil))) (run-hook-with-args 'compilation-start-hook proc) (compilation--update-in-progress-mode-line) (push proc compilation-in-progress)) (message "Executing `%s'..." command) (setq mode-line-process '((:propertize ":run" face compilation-mode-line-run) compilation-mode-line-errors)) (force-mode-line-update) (sit-for 0) (save-excursion (goto-char (point-max)) (let* ((inhibit-read-only t) (compilation-filter-start (point)) (status (call-process shell-file-name nil outbuf nil "-c" command))) (run-hooks 'compilation-filter-hook) (cond ((numberp status) (compilation-handle-exit 'exit status (if (zerop status) "finished\n" (format "exited abnormally with code %d\n" status)))) ((stringp status) (compilation-handle-exit 'signal status (concat status "\n"))) (t (compilation-handle-exit 'bizarre status status))))) (set-buffer-modified-p nil) (message "Executing `%s'...done" command))) (setq default-directory thisdir) (when compilation-scroll-output (goto-char (point-max)))) (setq next-error-last-buffer outbuf))) (defun compilation-handle-exit (process-status exit-status msg) "Write MSG in the current buffer and hack its `mode-line-process'." (let ((inhibit-read-only t) (status (if compilation-exit-message-function (funcall compilation-exit-message-function process-status exit-status msg) (cons msg exit-status))) (omax (point-max)) (opoint (point)) (cur-buffer (current-buffer))) (goto-char omax) (compilation-insert-annotation ?\n mode-name " " (car status)) (if (and (numberp compilation-window-height) (zerop compilation-window-height)) (message "%s" (cdr status))) (if (bolp) (forward-char -1)) (compilation-insert-annotation ", duration " (let ((elapsed (- (float-time) compilation--start-time))) (cond ((< elapsed 10) (format "%.2f s" elapsed)) ((< elapsed 60) (format "%.1f s" elapsed)) (t (format-seconds "%h:%02m:%02s" elapsed))))) (goto-char (point-max)) (add-text-properties omax (point) (append '(compilation-handle-exit t) nil)) (setq mode-line-process (list (let ((out-string (format ":%s [%s]" process-status (cdr status))) (msg (format "%s %s" mode-name (replace-regexp-in-string "\n?$" "" (car status))))) (message "%s" msg) (propertize out-string 'help-echo msg 'face (if (> exit-status 0) 'compilation-mode-line-fail 'compilation-mode-line-exit))) compilation-mode-line-errors)) (force-mode-line-update) (if (and opoint (< opoint omax)) (goto-char opoint)) (run-hook-with-args 'compilation-finish-functions cur-buffer msg)))
  ;; (with-eval-after-load 'compile
  ;;   ;; (add-to-list 'compilation-environment "TERM=dumb-emacs-ansi")
  ;;   ;;   (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
  ;;   (remove-hook 'compilation-filter-hook #'my/nix-store-shorten-paths)
  ;;   )
  )

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
  (setq-default
   c-basic-offset 4
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
                     (define-key c-mode-base-map (kbd "C-,") 'cff-find-other-file)
                     (electric-operator-mode)
                     (smart-semicolon-mode)
                     ))))

(use-package ligature
  :demand t
  :config
  (let ((ligs '("ff" "fi" "ffi" "fl" "ffl")))
    (ligature-set-ligatures 't ligs))
  (global-ligature-mode 1))

(use-package dumb-jump
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq dumb-jump-force-searcher 'rg)
  (setq dumb-jump-default-project "/home/leeao/.local/bin/lib/std"))

(use-package multiple-cursors
  :bind (
         ("C-S-c C-S-c" . mc/edit-lines)
         ("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C-$"     . mc/mark-all-like-this)
         ("C-$"        . mc/skip-to-next-like-this)
         ("C-^"         . mc/skip-to-previous-like-this)
         :map mc/keymap
         ("<return>" . electric-newline-and-maybe-indent)))

(use-package tldr
  :defer t
  :bind ( :map tldr-mode-map
          ("SPC" . scroll-up-command)
          ("DEL" . scroll-down-command)
          ("t" . tldr)))

(use-package embark
  :ensure t
  :defer t
  :commands (embark-act
             embark-dwim
             embark-export
             embark-collect
             embark-bindings
             embark-prefix-help-command)
  :bind (("<CTRL-i>" . embark-dwim)
         ("C-." . embark-act)
         ("C-h B" . embark-bindings))
  :init (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  (use-package embark-consult
    :ensure t
    :hook
    (embark-collect-mode . consult-preview-at-point-mode)))

(use-package real-mono-themes
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
   '(trailing-whitespace ((t (:background "white" :foreground "white"))))
   '(makefile-space ((t (:underline t))))
   '(dictionary-word-definition-face ((t (:font "bookerly"))))
   '(dictionary-word-entry-face ((t (:font "bookerly"))))))

(use-package vertico
  :ensure t
  :commands vertico-mode
  :bind(
        :map vertico-map
        ("<next>" . scroll-up-command)
        ("<prior>" . scroll-down-command)
        ("<right>" . donothing)
        ("M-<next>" . vertico-next-group)
        ("M-<prior>" . vertico-previous-group))
  :custom
  (vertico-scroll-margin 0)
  (vertico-count 8)
  (vertico-resize nil)
  (vertico-cycle nil)
  :config
  (use-package orderless
    :ensure t
    :custom
    (completion-styles '(orderless basic))
    (completion-category-defaults nil)
    (completion-category-overrides '((file (styles partial-completion)))))
  (use-package vertico-flat
    :after vertico
    :ensure nil)
  (use-package vertico-directory
    :after vertico
    :ensure nil
    ;; More convenient directory navigation commands
    :bind (:map vertico-map
                ("RET" . vertico-directory-enter)
                ("DEL" . vertico-directory-delete-char)
                ("M-DEL" . vertico-directory-delete-word))
    ;; Tidy shadowed file names
    :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))
  ;; (use-package vertico-repeat
  ;;   :ensure nil
  ;;   :after vertico
  ;;   :hook (minibuffer-setup . vertico-repeat-save))
  :hook (after-init . vertico-mode))

(use-package yasnippet
  :ensure t
  :hook
  (after-init . yas-global-mode)

  :custom
  (yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-also-auto-indent-first-line t)  ; Indent first line of snippet
  (yas-also-indent-empty-lines t)
  (yas-snippet-revival nil)  ; Setting this to t causes issues with undo
  (yas-wrap-around-region nil) ; Do not wrap region when expanding snippets
  ;; (yas-triggers-in-field nil)  ; Disable nested snippet expansion
  ;; (yas-indent-line 'fixed) ; Do not auto-indent snippet content
  ;; (yas-prompt-functions '(yas-no-prompt))  ; No prompt for snippet choices

  :init
  ;; Suppress verbose messages
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
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-function] . helpful-callable)
  ([remap describe-key] . helpful-key)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  :custom
  (helpful-max-buffers 7))

;; (use-package compile-angel
;;   :ensure t
;;   :demand t
;;   :custom
;;   (compile-angel-verbose nil)
;;   :config
;;   (push "/post-init.el" compile-angel-excluded-files)
;;   (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode)
;;   (compile-angel-on-load-mode))

;; (use-package rotate-text
;;   :vc (:url "https://github.com/debug-ito/rotate-text.el"
;;             :rev :newest)
;;   ;; I wish anything can be download though melpa.. but oh, no, so I copy it's source into this file.
;;   )))

;; (use-package info
;;   :bind (:map Info-mode-map
;;               ("C-M-i" . Info-history-back)
;;               ("<prior>" . Info-scroll-down)
;;               ("<next>" . Info-scroll-up)
;;               ("DEL" . Info-scroll-down)
;;               ("SPC" . Info-scroll-up)
;;               ;; ("<prior>" . (lambda () (interactive)(Info-scroll-down)(bookmark-set (string-trim Info-current-file "/run/current-system/sw/share/info/"))(donothing)))
;;               ;; ("<next>" . (lambda () (interactive)(Info-scroll-up)(bookmark-set (string-trim Info-current-file "/run/current-system/sw/share/info/"))(donothing)))
;;               ;; ("DEL" . (lambda () (interactive)(Info-scroll-down)(bookmark-set (string-trim Info-current-file "/run/current-system/sw/share/info/"))(donothing)))
;;               ;; ("SPC" . (lambda () (interactive)(Info-scroll-up)(bookmark-set (string-trim Info-current-file "/run/current-system/sw/share/info/"))(donothing)))
;;               ("b" . Info-next-preorder)))

;; (use-package tempel
;;   :bind
;;   (:map tempel-map
;;         ("C-c C-c" . tempel-done)
;;         ("<tab>" . tempel-expand))
;;   :config
;;   (use-package tempel-snippets
;;     :after tempel
;;     :vc (:url "https://github.com/gs-101/tempel-snippets")
;;     ))

;; (use-package markdown-mode
;;   :defer t
;;   :commands (gfm-mode
;;              gfm-view-mode
;;              markdown-mode
;;              markdown-view-mode)
;;   :mode (("\\.markdown\\'" . markdown-mode)
;;          ("\\.md\\'" . markdown-mode)
;;          ("README\\.md\\'" . gfm-mode))
;;   :bind
;;   (:map markdown-mode-map
;;         ("C-c C-e" . markdown-do)))

;; (use-package envrc
;;   :defer 2
;;   :config (envrc-global-mode 1)
;;   (advice-add 'org-babel-eval :around #'envrc-propagate-environment))

;; (use-package marginalia
;;   :ensure t
;;   :defer t
;;   :commands (marginalia-mode marginalia-cycle)
;;   :hook (after-init . marginalia-mode))

;; (use-package eglot
;;   :ensure nil
;;   :defer t
;;   :hook (eglot-managed-mode . (lambda ()
;;                                 (eglot-inlay-hints-mode -1) ; bloate
;;                                 (eldoc-mode -1) ; bloate
;;                                 (flymake-mode -1) ; bloate
;;                                 ))
;;   :custom
;;   (eglot-sync-connect 0)
;;   (eldoc-mode -1)
;;   (eglot-autoshutdown t)
;;   (eglot-extend-to-xref t)
;;   (eglot-events-buffer-config '(:size 0 :format short))
;;   (eglot-ignored-server-capabilities '(:documentLinkProvider
;;                                        :documentOnTypeFormattingProvider
;;                                        :foldingRangeProvider
;;                                        :colorProvider
;;                                        :inlayHintProvider))
;;   :config
;;   (setq flymake-no-changes-timeout nil)  ; Don't run on idle
;;   (setq flymake-start-on-flymake-mode nil) ; Don't auto-start
;;   (setq flymake-start-on-save-buffer nil)
;;   (setq flymake-fringe-indicator-position nil)
;;   (setq flymake-margin-indicator-position nil)
;;   (setq flymake-margin-indicators-string
;;         '((error "" compilation-error) (warning "" compilation-warning)
;;           (note "" compilation-info)))
;;   (setq flymake-indicator-type nil)
;;   (setq eglot-server-programs
;;         '((c-mode . ("clangd"))
;;           (c++-mode . ("clangd"))
;;           (zig-mode . ("zls"))
;;           (latex-mode . ("texlab"))
;;           (rust-mode . ("rust-analyzer")))))

;; (use-package corfu
;;   :ensure t
;; :defer t
;;   :commands (corfu-mode global-corfu-mode)
;;   :config
;;   (use-package corfu-history
;;     :ensure nil
;;     :after corfu
;;     :init
;;     (corfu-history-mode 1)
;;     :config
;;     (with-eval-after-load 'savehist
;;       (cl-pushnew 'corfu-history savehist-additional-variables)))
;;   ;; (use-package corfu-popupinfo
;;   ;;   :ensure nil
;;   ;;   :after corfu
;;   ;;   :init
;;   ;;   (corfu-popupinfo-mode 1)
;;   ;;   :config
;;   ;;   (setq corfu-popupinfo-delay '(1.0 . 1.0))
;;   ;;   (setq corfu-quick1 "rst")
;;   ;;   (setq corfu-quick2 "nei")
;;   ;;   (keymap-set corfu-map "M-<tab>" #'corfu-quick-complete)
;;   ;;   (keymap-set corfu-map "C-<tab>" #'corfu-quick-insert))
;;   :hook (
;;          (prog-mode . corfu-mode)
;;          (shell-mode . corfu-mode)
;;          (eshell-mode . corfu-mode))
;;   :custom
;;   (read-extended-command-predicate #'command-completion-default-include-p)
;;   (text-mode-ispell-word-completion nil)
;;   (tab-always-indent 'complete)
;;   :config (global-corfu-mode))

;; (use-package cape
;;   :ensure t
;;   :defer t
;;   :commands (cape-dabbrev cape-file cape-elisp-block)
;;   :bind ("C-c p" . cape-prefix-map)
;;   :init
;;   (add-hook 'completion-at-point-functions #'cape-dabbrev)
;;   (add-hook 'completion-at-point-functions #'cape-file)
;;   (add-hook 'completion-at-point-functions #'cape-elisp-block))

;; (use-package which-key
;;   :ensure nil
;;   :commands which-key-mode
;;   :hook (after-init . which-key-mode)
;;   :custom
;;   (which-key-idle-delay 1.5)
;;   (which-key-idle-secondary-delay 0.25)
;;   (which-key-add-column-padding 1)
;;   (which-key-max-description-length 40)
;;   (which-key-use-C-h-commands nil))

;; (use-package uniquify
;;   :ensure nil
;;   :custom
;;   (uniquify-buffer-name-style 'reverse)
;;   (uniquify-separator "•")
;;   (uniquify-after-kill-buffer-p t))

;; (use-package org
;;   :ensure t
;;   :defer t
;;   :commands (org-mode org-version)
;;   :mode
;;   ("\\.org\\'" . org-mode)
;;   :custom
;;   (org-support-shift-select t)
;;   (org-latex-compiler "xelatex")
;;   (org-latex-src-block-backend 'listings)
;;   (org-export-with-broken-links t)
;;   (org-export-with-tags nil)
;;   (org-export-with-drawers nil)
;;   (org-export-with-toc nil)
;;   (org-hide-leading-stars nil)
;;   (org-startup-folded 'showeverything)
;;   (org-agenda-span 'week)
;;   (org-log-into-drawer t)
;;   (org-startup-indented nil)
;;   (org-adapt-indentation nil)
;;   (org-edit-src-content-indentation 0)
;;   (org-startup-truncated t)
;;   (org-fontify-done-headline nil)
;;   (org-fontify-todo-headline nil)
;;   (org-hide-emphasis-markers nil)
;;   (org-fontify-whole-heading-line nil)
;;   (org-fontify-quote-and-verse-blocks nil)
;;   (org-confirm-babel-evaluate nil)
;;   (org-startup-with-inline-images t)
;;   (org-link-descriptive nil)
;;   (org-todo-keywords ;; t要做的，f要修的，e暂时的，a失败的，k有缺陷的，o就这样吧
;;    '((sequence  "TODO(t)" "DONE(d)" "|" "FIXME(f)")
;;      (sequence "TEMP(e)" "FAIL(a)" "KLUDGE(k)"    "|" "OKAY(o)")))
;;   :bind (( "C-c a" . org-agenda)
;;          ( "C-c l" . org-store-link))
;;   :config
;; (with-eval-after-load 'ox-latex
;; (add-to-list 'org-latex-classes
;;              '("elegantbook"
;;                "\\documentclass[lang=cn,math=cm,10pt,scheme=chinese,toc=twocol,bibend=bibtex]{elegantbook}"
;;                ("\\chapter{%s}" . "\\chapter*{%s}")
;;                ("\\section{%s}" . "\\section*{%s}")
;;                ("\\subsection{%s}" . "\\subsection*{%s}")
;;                ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))
;;   ;; (remove-hook 'org-mode-hook  #'toggle-truncate-lines)
;;   (with-eval-after-load 'org
;;     (define-key org-mode-map (kbd "C-,") nil)
;;     (define-key org-mode-map (kbd "<f20>")
;;                 (lambda () (interactive)
;;                   (org-latex-export-to-pdf)
;;                   (org-html-export-to-html)
;;                   (delete-file "./index-blx.bib" nil)
;;                   (delete-file "./index.bbl" nil)
;;                   (delete-file "./index.tex" nil)
;;                   (find-file "index.pdf")
;;                   ))
;;     ;; (define-key org-mode-map (kbd "C-<return>") #'org-insert-heading)
;;     ;; (define-key org-mode-map (kbd "M-<return>") #'org-insert-item)
;;     ;; (define-key org-mode-map (kbd "C-S-<return>") #'org-insert-heading-respect-content)
;;     )
;;   (org-babel-do-load-languages
;;    'org-babel-load-languages
;;    '((C . t)
;;      (emacs-lisp . t)
;;      (shell . t)
;;      (python . t))))

;; (use-package no-emoji
;;   :config
;;   (setq no-emoji-display-table (make-display-table))
;;   (set-face-attribute 'no-emoji nil
;;                       :background (face-attribute 'default :background)
;;                       :foreground (face-attribute 'default :background)
;;                       :height 0.1)
;;   (global-no-emoji-minor-mode 1))

;; (use-package eww
;;   :ensure nil
;;   :config
;;
;;   ;; (defun my-jichang(price traffic usage &optional used)
;;   ;;   "如 (my-jichang 14.9 130.0 0.39 86.08)， 用于计算梯子的大概天数/日均价格，
;;   ;; 0.39是最近110天内计算的日均流量使用。 price 梯子价格, traffic 购得的流量
;;   ;; usage 个人一般每日使用的流量,used(可选) 己经使用的流量"
;;   ;;   (message (format (concat " 每GB流量%f元 \n 大概使用%f天或%f年 \n 每天大概%f元 \n " (if  (numberp used) (format "大概还剩%f天"     (/ (- traffic used) (* usage 1.0)))nil))
;;   ;;                    (/ price (* traffic 1.0))
;;   ;;                    (/ traffic (* usage 1.0))
;;   ;;                    (/ (/ traffic (* usage 1.0)) 365)
;;   ;;                    (/ price (/ traffic (* usage 1.0))))))
;;   ;; (defun replace-url-in-region ()
;;   ;;   "Replace URLs in region with 0.0.0.0 and convert / to 0.0.0.0."
;;   ;;   (interactive)
;;   ;;   (when (use-region-p)
;;   ;;     (save-excursion
;;   ;;       (save-restriction
;;   ;;         (narrow-to-region (region-beginning) (region-end))
;;   ;;         (goto-char (point-min))
;;   ;;         ;; Replace https:// → 0.0.0.0
;;   ;;         (while (search-forward "https://" nil t)
;;   ;;           (replace-match "0.0.0.0 " nil t))
;;   ;;         (goto-char (point-min))
;;   ;;         ;; Replace http:// → 0.0.0.0
;;   ;;         (while (search-forward "http://" nil t)
;;   ;;           (replace-match "0.0.0.0 " nil t))
;;   ;;         (goto-char (point-min))
;;   ;;         ;; Replace / →
;;   ;;         (while (search-forward "/" nil t)
;;   ;;           (replace-match "" nil t))
;;   ;;         (mark-whole-buffer)
;;   ;;         (widen)
;;   ;;         (sort-lines nil (point-min) (point-max))
;;   ;;         (delete-duplicate-lines (point-min) (point-max))
;;   ;;         ))))
;;
;;   (setq browse-url-browser-function 'eww-browse-url
;;         shr-use-colors nil
;;         eww-header-line-format nil
;;         eww-form-checkbox-selected-symbol "[X]"
;;         eww-form-checkbox-symbol "[ ]"
;;         shr-bullet "- "
;;         shr-folding-mode t
;;         shr-use-fonts t
;;         shr-inhibit-images t
;;         shr-width nil
;;         shr-max-image-proportion 0.6
;;         eww-search-prefix nil
;;         eww-default-download-directory "~/"
;;         url-privacy-level 'low
;;         eww-auto-rename-buffer 'url
;;         eww-prompt-history '(
;;                              "https://learnxinyminutes.com/"
;;                              ;; "https://ziggit.dev/"
;;                              ;; "http://c.doc:3001/" ; "https://en.cppreference.com/w/c"
;;                              ;; "http://cpp.doc:3002/" ; "https://en.cppreference.com/w/cpp"
;;                              ;; "http://linux.doc:3000/" ;"https://www.kernel.org/doc/html/latest/"
;;                              ;; C-h I "https://www.gnu.org/software/emacs/manual/"
;;                              ))
;;   (defun my-eww-edit-url ()
;;     "Edit the current EWW URL and reload the page."
;;     (interactive)
;;     (unless (derived-mode-p 'eww-mode)
;;       (user-error "Not in EWW buffer"))
;;     (let ((current-url (plist-get eww-data :url)))
;;       (setq eww-data (plist-put eww-data :url
;;                                 (read-string "Edit URL: " current-url)))
;;       (eww-reload)))
;;   (defun my-eww-forward-url()   (interactive) (eww-forward-url) (recenter-top-bottom 0))
;;   (defun my-eww-back-url()   (interactive) (eww-back-url) (recenter-top-bottom 0))
;;   (defun eww-follow-link (&optional external mouse-event)
;;     "Browse the URL under point.
;;     If EXTERNAL is single prefix, browse the URL using
;;     `browse-url-secondary-browser-function'.
;;
;;     If EXTERNAL is double prefix, browse in new buffer."
;;     (interactive
;;      (list current-prefix-arg last-nonmenu-event)
;;      eww-mode)
;;     (mouse-set-point mouse-event)
;;     (let* ((orig-url (get-text-property (point) 'shr-url))
;;            (url (eww--transform-url orig-url))
;;            target)
;;       (cond
;;        ((not url)
;;         (message "No link under point"))
;;        ((string-match-p eww-use-browse-url url)
;;         ;; This respects the user options `browse-url-handlers'
;;         ;; and `browse-url-mailto-function'.
;;         (browse-url url))
;;        ((and (consp external) (<= (car external) 4))
;;         (funcall browse-url-secondary-browser-function url)
;;         (shr--blink-link))
;;        ;; This is a #target url in the same page as the current one.
;;        ((and (setq target (url-target (url-generic-parse-url url)))
;;              (eww-same-page-p url (plist-get eww-data :url)))
;;         (let ((point (point)))
;;           (eww-save-history)
;;           (eww--before-browse)
;;           (plist-put eww-data :url url)
;;           (goto-char (point-min))
;;           (if-let ((match (text-property-search-forward 'shr-target-id target #'member)))
;;               (goto-char (prop-match-beginning match))
;;             (goto-char (if (equal target "top")
;;                            (point-min)
;;                          point))))
;;         (recenter-top-bottom 0))
;;        (t
;;         (eww-browse-url orig-url external)))))
;;   (defun extract-base-urls ()
;;     "Extract base URLs from the current buffer containing text file links."
;;     (interactive)
;;     (save-excursion
;;       (goto-char (point-min))
;;       (let ((urls '()))
;;         (while (re-search-forward "https?://\\([^:/]+\\)" nil t)
;;           (push (match-string 0) urls))
;;         (with-output-to-temp-buffer "*Base URLs*"
;;           (dolist (url (delete-dups (nreverse urls)))
;;             (princ url)
;;             (princ "\n"))))))
;;   ;; (remove-hook 'eww-after-render-hook 'eww-readable)
;;   :bind (:map eww-mode-map
;;               ("L" . eww-list-bookmarks)
;;               ("r" . my-eww-forward-url)
;;               ("l" . my-eww-back-url)
;;               ("<return>" . eww-follow-link)
;;               ("C-M-i" . my-eww-back-url)
;;               ("b" . my-eww-forward-url)
;;               ("E" . my-eww-edit-url)))

;; (use-package nov
;;   ;; PEN 笔
;;   ;; 1. <tab>
;;   ;;    - double: <enter>
;;   ;;    - long: C-M-i
;;   ;; 2. <prior>
;;   ;;    - long: <esc>/<f5>
;;   ;; 3. <next>
;;   ;;    - long: <b>
;;   ;; 4. laser
;;   :custom
;;   (nov-header-line-format nil)
;;   :mode ("\\.epub\\'" . nov-mode)
;;   :bind (:map nov-mode-map
;;               ("p" . 'scroll-down-command)
;;               ("n" . 'scroll-up-command)
;;               ("s" . occur)
;;               ;; ("ESC <prior>" . (lambda () (interactive) (bookmark-set "epub")))
;;               ;; ("C-M-i" .              (lambda () (interactive) (bookmark-jump "epub")))
;;               ("<prior>" . nov-scroll-down)
;;               ("<next>" . nov-scroll-up)))

;; (use-package pdf-tools
;;   :ensure t
;;   :defer t
;;   :mode ("\\.pdf\\'" . pdf-view-mode)
;;   :init (pdf-loader-install)
;;   :commands (pdf-view-mode)
;;   :bind ( :map pdf-view-mode-map
;;           ("s" . pdf-occur)
;;           ("n" . pdf-view-next-page)
;;           ("p" . pdf-view-previous-page)
;;           ("C-M-i" . donothing)
;;           ("RET" . donothing)
;;           ("TAB" . donothing)
;;           ("M-v" . pdf-view-scroll-down-or-previous-page)
;;           ("C-v" . pdf-view-scroll-up-or-next-page)
;;           ("<prior>" . pdf-view-previous-page)
;;           ("<next>" . pdf-view-next-page))
;;   :config
;; (use-package doc-view
;;   :defer t
;;   :config
;;   (setq doc-view-continuous t)
;;   :bind (:map doc-view-mode-map
;;               ("M-v" . doc-view-scroll-down-or-previous-page)
;;               ("C-v" . doc-view-scroll-up-or-next-page)
;;               ("C-p" . (lambda () (interactive) (doc-view-previous-line-or-previous-page 3)))
;;               ("C-n" . (lambda () (interactive) (doc-view-next-line-or-next-page 3)))))
;;   (add-to-list 'revert-without-query ".pdf")
;;   (use-package saveplace-pdf-view))

;; (use-package image-mode
;;   :ensure nil
;;   :bind(:map image-mode-map
;;              ("<next>" . kmacro-end-or-call-macro)
;;              ("<prior>" . image-previous-file)))

;; (use-package savehist
;; :ensure nil
;; :commands (savehist-mode savehist-save)
;; :hook
;; (after-init . savehist-mode)
;; :custom
;; (savehist-file (expand-file-name "history" user-emacs-directory))
;; (savehist-autosave-interval 600)
;; (savehist-additional-variables
;;  '(kill-ring                        ; clipboard
;;    register-alist                   ; macros
;;    mark-ring global-mark-ring       ; marks
;;    search-ring regexp-search-ring)))

;; (use-package rust-mode
;;   :config
;;   (add-hook 'rust-mode-hook 'cargo-minor-mode))

;; (use-package zig-mode
;; :config
;; (defun my/get-project-vterm-buffer ()
;;   "Return the project vterm buffer, or nil if not exists."
;;   (let* ((proj (project-current t))
;;          (root (directory-file-name (project-root proj)))
;;          (pattern (format "\\*vterminal - %s" (regexp-quote root))))
;;     (catch 'found
;;       (dolist (buf (buffer-list))
;;         (when (string-match pattern (buffer-name buf))
;;           (throw 'found buf)))
;;       nil)))
;; (defun my/ensure-project-vterm ()
;;   "Ensure project vterm exists."
;;   (let ((buf (my/get-project-vterm-buffer)))
;;     (unless buf
;;       (multi-vterm-project)
;;       (setq buf (my/get-project-vterm-buffer)))
;;     buf))
;; (defun my/send-to-vterm ()
;;   "Send active region or current line to project vterm without touching kill-ring."
;;   (interactive)
;;   (let* ((text (if (use-region-p)
;;                    (buffer-substring-no-properties (region-beginning) (region-end))
;;                  (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
;;          (vbuf (my/ensure-project-vterm)))
;;     (unless vbuf (user-error "No project vterm buffer"))
;;     (let ((proc (get-buffer-process vbuf)))
;;       (unless proc (user-error "No process for vterm buffer"))
;;       (process-send-string proc (concat text "\n")))))
;; (defun firefox-search-zig()
;;   (interactive)
;;   (let ((meme (if (use-region-p)
;;                   (buffer-substring-no-properties (region-beginning) (region-end))
;;                 (thing-at-point 'word t)) ))
;;     (start-process "me" nil browse-url-firefox-program "--new-tab"
;;                    (concat "http://127.0.0.1:55555/#" meme))
;;     (swaywindow)))
;; (defun my/zig-build-vterm ()
;;   "Send active region or current line to project vterm without touching kill-ring."
;;   (interactive)
;;   (save-buffer)
;;   (let* ((text "c; zig build")
;;          (vbuf (my/ensure-project-vterm)))
;;     (unless vbuf (user-error "No project vterm buffer"))
;;     (let ((proc (get-buffer-process vbuf)))
;;       (unless proc (user-error "No process for vterm buffer"))
;;       (process-send-string proc (concat text "\n")))))
;;   :mode(("\\.zig\\'" . zig-mode)
;;         ("\\.zon\\'" . zig-mode))
;;   :bind(
;;         :map zig-mode-map
;;         ;; ("C-c C-; c" . my/zig-build-vterm)
;;         ;; ("C-c C-; m" . firefox-search-zig)
;;         ))

;; (use-package tramp
;;   :config
;;   ;; 似乎是支持 remote 启动其他进程的
;;   (connection-local-set-profile-variables
;;    'remote-direct-async-process
;;    '((tramp-direct-async-process . t)))
;;   ;; 如果设置了这条命令，就会导致 ssh 无法使用 eglot, 只能用 sshx
;;   ;; 而如果使用 sshx ，又无法使用 dirvish 的预览功能，乐
;;   ;; ssh 在远程机器上打开一个正常的交互 shell ，而 sshx 使用 `ssh -t -t host -l user /bin/sh` 来打开连接
;;   (connection-local-set-profiles
;;    '(:application tramp :protocol "ssh")
;;    'remote-direct-async-process)
;;   ;; (add-to-list 'tramp-remote-path "/home/qs/.local/bin")
;;   ;; 我目前的理解是 tramp-own-remote-path 表示的是用 user 在登录后使用的环境变量
;;   ;; 而原本的 tramp-remote-path 是没有登录后的变量的
;;   (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
;;   ;; 加速，更少的 tramp 信息
;;   (setq tramp-verbose 0)
;;   :custom
;;   ;; 禁用保存，提高速度
;;   (remote-file-name-inhibit-auto-save t)
;;   ;; (tramp-auto-save-directory (no-littering-expand-var-file-name "tramp-autosaves/"))
;;   (tramp-backup-directory-alist backup-directory-alist)
;;   ;; 禁用 shell history 文件
;;   (shell-history-file-name t)
;;   ;; tramp 的默认方法
;;   (tramp-default-method "ssh")
;;   ;; 加速，允许 cache 在 60s 内使用
;;   (remote-file-name-inhibit-cache 60)
;;   ;; 加速，不会使用文件锁
;;   (remote-file-name-inhibit-locks t)
;;   ;; 加速，禁用一些版本控制后端
;;   (vc-handled-backends '(Git))
;;   ;; (vc-handled-backends nil)
;;   ;; 只需要输入一次密码 https://www.reddit.com/r/emacs/comments/3liwm7/is_it_possible_to_configure_tramp_such_that_i/
;;   (tramp-use-ssh-controlmaster-options nil)
;;   (tramp-chunksize 2000))

;; (use-package devdocs-browser
;;   ;; :config
;;   ;; Programmatically install documentation
;;   ;; (dolist (doc '(
;;   ;;                ;; "c"
;;   ;;                ;; "cpp"
;;   ;;                ;; "zig"
;;   ;;                "rust"
;;   ;;                ;; "python~3.7"
;;   ;;                ))
;;   ;;   (devdocs-browser-install-doc doc)
;;   ;;   (devdocs-browser-download-offline-data doc))
;;   )

;; (use-package gptel
;;   :init
;;   (defalias 'dee 'gptel)
;;   (setq deepseek-api-key
;;         (with-temp-buffer
;;           (insert-file-contents "/run/secrets/deepseek_apikey")
;;           (buffer-string)))
;;   (require 'gptel-org)
;;   :config
;;   (setq  gptel-default-mode 'org-mode)
;;   (setq gptel-model   'deepseek-chat
;;         gptel-backend (gptel-make-deepseek "deepseek"
;;                         :stream t
;;                         :key deepseek-api-key))
;;   (require 'url-util)
;;   (setq-default gptel-directives
;;                 '((default . "You are a large language model living in Emacs and a helpful assistant. 编程规范（少于70字符一行），中文文本24行×34字（少于约800字/页）")
;;                   (programming . "You are a large language model and a careful programmer. Provide code and only code as output without any additional text, prompt or note.")
;;                   (writing . "You are a large language model and a writing assistant. Respond concisely.")
;;                   (chat . "You are a large language model and a conversation partner. Respond concisely.")
;;                   (bug . "You are a large language model and a careful programmer. The supplied code doesn't work, or contains bugs. Describe each problem using only one sentence. Provide fixes without changing the old behavior.")))
;;   (setq  gptel-stream nil))

;; (use-package consult-gh
;;   :defer t
;;   :after consult
;;   :custom
;;   (consult-gh-confirm-before-clone nil)
;;   (consult-gh-ask-for-path-before-save nil)
;;   (consult-gh-show-preview t)
;;   (consult-gh-preview-key "C-o")
;;   (consult-gh-repo-action #'consult-gh--repo-browse-files-action)
;;   (consult-gh-large-file-warning-threshold 2500000)
;;   (consult-gh-confirm-name-before-fork nil)
;;   (consult-gh-notifications-show-unread-only nil)
;;   (consult-gh-default-interactive-command)
;;   (consult-gh-prioritize-local-folder nil)
;;   (consult-gh-issues-state-to-show "all") ; show readmes in their original format
;;   (consult-gh-group-dashboard-by :reason)
;;   (consult-gh-repo-preview-major-mode nil) ; show readmes in their original format
;;   (consult-gh-preview-major-mode 'org-mode) ; use 'org-mode for editing comments, commit messages, ...
;;   :config
;;   (consult-gh-enable-default-keybindings)
;;   (use-package consult-gh-forge
;;     :after consult-gh
;;     :config
;;     (consult-gh-forge-mode +1))
;;   (use-package consult-gh-embark
;;     :after consult-gh
;;     :config
;;     (consult-gh-embark-mode +1)
;;     (setq consult-gh-forge-timeout-seconds 20)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               begin of tavily                              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq tavily-api-key
      (with-temp-buffer
        (insert-file-contents "/run/secrets/tavily_apikey")
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

                                                ;; "ansi.org"
                                                ;; "contiki-ng.org"
                                                ;; "freertos.org"
                                                ;; "gcc.gnu.org"
                                                ;; "gnu.org"
                                                ;; "gtk.org"
                                                ;; "ieee.org"
                                                ;; "ietf.org"
                                                ;; "iso.org"
                                                ;; "kernel.org"
                                                "libsdl.org"
                                                ;; "llvm.org"
                                                ;; "man7.org"
                                                ;; "musl-libc.org"
                                                ;; "mynewt.apache.org"
                                                ;; "newlib.sourceware.org"
                                                ;; "nuttx.apache.org"
                                                ;; "opencores.org"
                                                ;; "opensource.org"
                                                ;; "qemu-project.org"
                                                ;; "qt.io"
                                                ;; "riot-os.org"
                                                ;; "riscv.org"
                                                ;; "sdl.org"
                                                ;; "sourceware.org"
                                                ;; "uclibc-ng.org"
                                                ;; "w3.org"
                                                ;; "zephyrproject.org"

                                                ;; "arduino.cc"
                                                ;; "espressif.com"
                                                ;; "raspberrypi.org"

                                                ;; "archlinux.org"
                                                "nixos.org"
                                                ;; "gentoo.org"
                                                ;; "debian.org"

                                                ;; "100.github.io"
                                                ;; "250bpm.com"
                                                ;; "adtinfo.org"
                                                ;; "anjuta.org"
                                                ;; "apophenia.info"
                                                ;; "apr.apache.org"
                                                ;; "astyle.sourceforge.net"
                                                ;; "attractivechaos.github.io"
                                                ;; "avro.apache.org"
                                                ;; "bitbucket.org"
                                                ;; "blog.llvm.org"
                                                ;; "blog.noctua-software.com"
                                                ;; "blog.pkh.me"
                                                ;; "blogs.oracle.com"
                                                ;; "blosc.org"
                                                ;; "brechtsanders.github.io"
                                                ;; "c-faq.com"
                                                ;; "c2html.sourceforge.net"
                                                ;; "cairographics.org"
                                                ;; "ccache.dev"
                                                ;; "ccodearchive.net"
                                                ;; "cdecl.org"
                                                ;; "cedet.sourceforge.net"
                                                ;; "cesanta.com"
                                                ;; "chipmunk-physics.net"
                                                ;; "clang.llvm.org"
                                                ;; "cmake.org"
                                                ;; "cmocka.org"
                                                ;; "coap.technology"
                                                ;; "codeforwin.org"
                                                ;; "codeplea.com"
                                                ;; "computing.llnl.gov"
                                                ;; "conan.io"
                                                ;; "concurrencykit.org"
                                                ;; "cppcheck.sourceforge.net"
                                                ;; "criterion.readthedocs.io"
                                                ;; "criu.org"
                                                ;; "cunit.sourceforge.net"
                                                ;; "curl.haxx.se"
                                                ;; "cyan4973.github.io"
                                                ;; "cygwin.com"
                                                ;; "czmq.zeromq.org"
                                                ;; "danluu.com"
                                                ;; "dl.acm.org"
                                                ;; "docutils.sourceforge.net"
                                                ;; "dotat.at"
                                                ;; "download.libsodium.org"
                                                ;; "duckdb.org"
                                                ;; "duktape.org"
                                                ;; "ebassi.github.io"
                                                ;; "ed-von-schleck.github.io"
                                                ;; "ejdb.org"
                                                ;; "embed.cs.utah.edu"
                                                ;; "en.wikibooks.org"
                                                ;; "en.wikipedia.org"
                                                ;; "esbmc.org"
                                                ;; "expat.sourceforge.net"
                                                ;; "ezxml.sourceforge.net"
                                                ;; "fabutil.org"
                                                ;; "facebook.github.io"
                                                ;; "facil.io"
                                                ;; "faragon.github.io"
                                                ;; "flintlib.org"
                                                ;; "fragglet.github.io"
                                                ;; "freeglut.sourceforge.net"
                                                ;; "freeweb.siol.net"
                                                ;; "gcc.gnu.org"
                                                ;; "gist.github.com"
                                                ;; "git.mpich.org"
                                                ;; "git.sr.ht"
                                                ;; "github.com"
                                                ;; "glade.gnome.org"
                                                ;; "gmplib.org"
                                                ;; "gmsl.sourceforge.net"
                                                ;; "gnu.org"
                                                ;; "gnupg.org"
                                                ;; "google.github.io"
                                                ;; "graphics.stanford.edu"
                                                ;; "gstreamer.freedesktop.org"
                                                ;; "h2o.examp1e.net"
                                                ;; "hal.science"
                                                ;; "hardysimpson.github.io"
                                                ;; "hintjens.gitbooks.io"
                                                ;; "hirrolot.github.io"
                                                ;; "hplgit.github.io"
                                                ;; "icculus.org"
                                                ;; "igraph.org"
                                                ;; "ioquake3.org"
                                                ;; "jemalloc.net"
                                                ;; "joaotavora.github.io"
                                                ;; "jstimpfle.de"
                                                ;; "jvns.ca"
                                                ;; "kitsune-dsu.com"
                                                ;; "knking.com"
                                                ;; "kore.io"
                                                ;; "kristaps.bsd.lv"
                                                ;; "liballeg.org"
                                                ;; "libccv.org"
                                                ;; "libcello.org"
                                                ;; "libcheck.github.io"
                                                ;; "libcork.readthedocs.io"
                                                ;; "libcox.symisc.net"
                                                ;; "libdill.org"
                                                ;; "libevent.org"
                                                ;; "libgit2.org"
                                                ;; "libjpeg-turbo.virtualgl.org"
                                                ;; "libjpeg.sourceforge.net"
                                                ;; "liblfds.org"
                                                ;; "libmill.org"
                                                ;; "libsound.io"
                                                ;; "libspng.org"
                                                ;; "libtrading.org"
                                                ;; "liburcu.org"
                                                ;; "libusb.info"
                                                ;; "libuv.org"
                                                ;; "libvips.github.io"
                                                ;; "libzip.org"
                                                ;; "lionet.info"
                                                ;; "lipforge.ens-lyon.fr"
                                                ;; "lldb.llvm.org"
                                                ;; "llhttp.org"
                                                ;; "lloyd.github.io"
                                                ;; "locklessinc.com"
                                                ;; "lodev.org"
                                                ;; "logological.org"
                                                ;; "lvgl.io"
                                                ;; "lwan.ws"
                                                ;; "lz4.github.io"
                                                ;; "lzip.nongnu.org"
                                                ;; "maciejczyzewski.github.io"
                                                ;; "madmurphy.github.io"
                                                ;; "marek.vavrusa.com"
                                                ;; "math-atlas.sourceforge.net"
                                                ;; "mesonbuild.com"
                                                ;; "michaelrsweet.github.io?Z3"
                                                ;; "mihl.sourceforge.net"
                                                ;; "mike.steinert.ca"
                                                ;; "mingw-w64.yaxm.org"
                                                ;; "mingw.org"
                                                ;; "mongoc.org"
                                                ;; "mpfr.loria.fr"
                                                ;; "mpitutorial.com"
                                                ;; "mpv.io"
                                                ;; "msgpack.org"
                                                ;; "msune.github.io"
                                                ;; "msys2.github.io"
                                                ;; "musl.libc.org"
                                                ;; "nanomsg.github.io"
                                                ;; "nappgui.com"
                                                ;; "nemequ.github.io"
                                                ;; "netbeans.org"
                                                ;; "nethack4.org"
                                                ;; "nullprogram.com"
                                                ;; "openquantumsafe.org"
                                                ;; "opensource.org"
                                                ;; "opic.rocks"
                                                ;; "oprofile.sourceforge.net"
                                                ;; "orx-project.org"
                                                ;; "pari.math.u-bordeaux.fr"
                                                ;; "paulbatchelor.github.io"
                                                ;; "pcc.ludd.ltu.se"
                                                ;; "pdclib.e43.eu"
                                                ;; "pdos.csail.mit.edu"
                                                ;; "perf.wiki.kernel.org"
                                                ;; "pngquant.org"
                                                ;; "port70.net"
                                                ;; "pp.ipd.kit.edu"
                                                ;; "premake.github.io"
                                                ;; "projects.malikania.fr"
                                                ;; "proprogramming.org"
                                                ;; "re2c.org"
                                                ;; "redis.io"
                                                ;; "remove-to-waste.info"
                                                ;; "repo.hu"
                                                ;; "risoflora.github.io"
                                                ;; "roaringbitmap.org"
                                                ;; "rr-project.org"
                                                ;; "savannah.nongnu.org"
                                                ;; "savedparadigms.files.wordpress.com"
                                                ;; "scientificc.github.io"
                                                ;; "shop.oreilly.com"
                                                ;; "sigrok.org"
                                                ;; "site.icu-project.org"
                                                ;; "sites.google.com"
                                                ;; "slepc.upv.es"
                                                ;; "snaipe.me"
                                                ;; "sod.pixlab.io"
                                                ;; "software.schmorp.de"
                                                ;; "sophia.systems"
                                                ;; "sourceforge.net"
                                                ;; "sourceware.org"
                                                ;; "spdx.org"
                                                ;; "steve-yegge.blogspot.co.nz"
                                                ;; "svn.msweet.org"
                                                ;; "talloc.samba.org"
                                                ;; "tartarus.org"
                                                ;; "tatsuhiro-t.github.io"
                                                ;; "tiny-rex.sourceforge.net"
                                                ;; "tinycthread.github.io"
                                                ;; "tls.mbed.org"
                                                ;; "tools.ietf.org"
                                                ;; "troydhanson.github.io"
                                                ;; "trumpowen.github.io"
                                                ;; "tulipindicators.org"
                                                ;; "tuxfan.github.io"
                                                ;; "uclibc-ng.org"
                                                ;; "unqlite.org"
                                                ;; "uriparser.github.io"
                                                ;; "viewsourcecode.org"
                                                ;; "web.archive.org"
                                                ;; "webserver2.tecgraf.puc-rio.br"
                                                ;; "wiki.gnome.org"
                                                ;; "wiki.haskell.org"
                                                ;; "wiki.sei.cmu.edu"
                                                ;; "wiki.videolan.org"
                                                ;; "wolkykim.github.io"
                                                ;; "www.andre-simon.de"
                                                ;; "www.bzip.org"
                                                ;; "www.chiark.greenend.org.uk"
                                                ;; "www.codeblocks.org"
                                                ;; "www.codelite.org"
                                                ;; "www.codeproject.com"
                                                ;; "www.colm.net"
                                                ;; "www.coralbits.com"
                                                ;; "www.cprover.org"
                                                ;; "www.crasseux.com"
                                                ;; "www.digip.org"
                                                ;; "www.doxygen.nl"
                                                ;; "www.dyncall.org"
                                                ;; "www.enlightenment.org"
                                                ;; "www.eso.org"
                                                ;; "www.etalabs.net"
                                                ;; "www.etpan.org"
                                                ;; "www.fefe.de"
                                                ;; "www.feynarts.de"
                                                ;; "www.ffmpeg.org"
                                                ;; "www.fftw.org"
                                                ;; "www.flourish.org"
                                                ;; "www.freedesktop.org"
                                                ;; "www.geany.org"
                                                ;; "www.gedanken.org.uk"
                                                ;; "www.gii.upv.es"
                                                ;; "www.glfw.org"
                                                ;; "www.gnu.org"
                                                ;; "www.gnutls.org"
                                                ;; "www.greenend.org.uk"
                                                ;; "www.gtk.org"
                                                ;; "www.hboehm.info"
                                                ;; "www.hughes.com.au"
                                                ;; "www.infradead.org"
                                                ;; "www.kdevelop.org"
                                                ;; "www.koanlogic.com"
                                                ;; "www.learn-c.org"
                                                ;; "www.libjpeg-turbo.org"
                                                ;; "www.libpng.org"
                                                ;; "www.libretro.com"
                                                ;; "www.libsdl.org"
                                                ;; "www.libsigil.com"
                                                ;; "www.libtom.net"
                                                ;; "www.mcs.anl.gov"
                                                ;; "www.mega-nerd.com"
                                                ;; "www.mission-base.com"
                                                ;; "www.mongodb.org"
                                                ;; "www.mpich.org"
                                                ;; "www.multiprecision.org"
                                                ;; "www.netlib.org"
                                                ;; "www.nlnetlabs.nl"
                                                ;; "www.oberhumer.com"
                                                ;; "www.opengl.org"
                                                ;; "www.openmp.org"
                                                ;; "www.openssl.org"
                                                ;; "www.oracle.com"
                                                ;; "www.pcre.org"
                                                ;; "www.pearson.com"
                                                ;; "www.pell.portland.or.us"
                                                ;; "www.planetpdf.com"
                                                ;; "www.postgresql.org"
                                                ;; "www.pyyaml.org"
                                                ;; "www.rabbitmq.com"
                                                ;; "www.raylib.com"
                                                ;; "www.recurse.com"
                                                ;; "www.saphir2.com"
                                                ;; "www.scons.org"
                                                ;; "www.sfml-dev.org"
                                                ;; "www.sgi.com"
                                                ;; "www.shlomifish.org"
                                                ;; "www.spinellis.gr"
                                                ;; "www.sqlite.org"
                                                ;; "www.symas.com"
                                                ;; "www.tcl.tk"
                                                ;; "www.throwtheswitch.org"
                                                ;; "www.valgrind.org"
                                                ;; "www.webdav.org"
                                                ;; "www.xiph.org"
                                                ;; "xforms-toolkit.org"
                                                ;; "xiph.org"
                                                ;; "xmake.io"
                                                ;; "xmlsoft.org"
                                                ;; "zeromq.org"
                                                ;; "zinjai.sourceforge.net"
                                                ;; "zlib.net"
                                                ;; "zserge.com"
                                                "git.sr.ht"
                                                "codeberg.org"
                                                "github.com"
                                                "gitlab.com"
                                                "hackaday.com"
                                                "lwn.net"
                                                "osdev.org"
                                                "ziggit.dev"
                                                ;; "emacs-china.org"
                                                ;; "news.ycombinator.com"
                                                ;; "rustcc.cn"

                                                ;; "cmake.org"
                                                ;; "cppreference.com"
                                                ;; "elixir-lang.org"
                                                ;; "erlang.org"
                                                ;; "haskell.org"
                                                ;; "lua.org"
                                                ;; "python.org"
                                                ;; "rustlang.org"
                                                ;; "ziglang.org"

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               end of leetcode                              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                begin of sway                               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar rotate 0
  "Current sway rotate state, either 0 for horizental or  1 for vertical.")
(defun swaywindow()
  (interactive)
  (let ((direction '("down" "right")))
    (if (= (call-process-shell-command "swaymsg -t get_tree | jq -e '.. | select(.focused? == true and .fullscreen_mode == 1)' >/dev/null;" nil t t) 4)
        (start-process  "swaymsg" nil  "swaymsg" "focus" (car(nthcdr rotate direction)))(progn
                                                                                          (start-process  "fullscreen1" nil  "swaymsg" "fullscreen" )
                                                                                          (start-process  "swaymsg" nil  "swaymsg" "focus" (car(nthcdr rotate direction)))
                                                                                          (start-process  "fullscreen2" nil  "swaymsg" "fullscreen" )))))
(defun swayrotate()
  (interactive)
  (start-process  "fullscreen1" nil  "swaymsg" "layout toggle split" )
  (setq rotate   (if (= 0 rotate) 1 0 )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 end of sway                                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            begin of toggle-font                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar font-scale-list '((130 165)(140 190)(150 200)(160 210)(170 225)(180 240)(190 250)(200 270)(210 280)(220 285)(230 310)(240 315)(250 320)(260 350)))
(defvar fontfont 1)
(defun my-toggle-font (&optional size)
  "Toggle between UbuntuMono and bookerly fonts."
  (interactive)
  (if (= fontfont 1)
      (progn (set-face-attribute 'default nil :font my-default-font :height (car(nth (or size 3)  font-scale-list))) (setq fontfont 0))
    (progn (set-face-attribute 'default nil :font my-alternate-font :height (car (cdr(nth (or size 3)  font-scale-list)))) (setq fontfont 1))))
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
  ;; (interactive)
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
        (mode-state '(("3" "5" "3")  ("9" "5" "1") ("9" "2" "1")))) ; hey,  eink is awesome!
    (sleep-for 0.5)
    (dotimes (number 3)
      (call-process monitorcli nil nil nil
                    monitorprotocol
                    monitorpath
                    (car (nthcdr number monitorarg))
                    (car (nthcdr number (car (nthcdr monitor-state mode-state)))))
      (sleep-for 1))
    (setq monitor-state  1)
    (sleep-for 1)
    (call-process monitorcli nil nil nil monitorprotocol monitorpath (car (nthcdr 3 monitorarg))))
  (setq wake-up 0)
  (donothing))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               end of monitor                               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               begin of rotate                              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commentary:
;;
;; rotate-text allows you cycle through commonly interchanged text with a single
;; keystroke.  For example, you can toggle between "frame-width" and
;; "frame-height", between "public", "protected" and "private" and between
;; "variable1", "variable2" through "variableN".
;;
;; Add the following to your .emacs:
;;
;; (add-to-list 'load-path "/path/to/rotate-text")
;; (autoload 'rotate-text "rotate-text" nil t)
;; (autoload 'rotate-text-backward "rotate-text" nil t)
;;
;; Customize the variables `rotate-text-patterns', `rotate-text-symbols' and
;; `rotate-text-words'.  You can make buffer-local additions in
;; `rotate-text-local-patterns', `rotate-text-local-symbols' and
;; `rotate-text-local-words'.
;;
;; Use the commands `rotate-text' and `rotate-text-backward' to rotate the
;; text.
;;
;;; Change Log:
;;
;; 2009-04-13 (0.1)
;;    Initial release.
;;
;;; Code:

(eval-when-compile (require 'cl))

(add-to-list 'debug-ignored-errors "^Nothing to rotate$")

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
          (car (nthcdr (mod (- dir (length rest-pattern)) (length replacements))
                       replacements)))))))

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

;;;###autoload
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
           (return t))))
     ;; words
     (when (setq match (rotate-text-word-at-point))
       (dolist (words (append com-words rotate-text-local-words
                              rotate-text-words))
         (when (setq replacement
                     (rotate-text-replacement words (downcase match) arg))
           (setq replacement (rotate-text-match-case match replacement))
           (return t))))
     ;; regexp
     (dolist (pattern (append com-patterns rotate-text-local-patterns
                              rotate-text-patterns))
       (when (setq match (rotate-text-match-at-point (car pattern)))
         (setq replacement (rotate-text-replacement (cdr pattern) match arg))
         (return t))))

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

;;;###autoload
(defun rotate-text-backward (arg &optional default-string com-symbols com-words com-patterns)
  "Rotate the text at point backwards. If there is nothing to rotate at point and DEFAULT-STRING is non-nil,
DEFAULT-STRING is inserted at point.

COM-SYMBOLS, COM-WORDS and COM-PATTERNS are per-command addition to `rotate-text-symbols',
`rotate-text-words' and `rotate-text-patterns', respectively."
  (interactive (list (if (consp current-prefix-arg)
                         -1
                       (prefix-numeric-value current-prefix-arg))))
  (rotate-text (- arg) default-string com-symbols com-words com-patterns))

(autoload 'rotate-text "rotate-text" nil t)
(autoload 'rotate-text-backward "rotate-text" nil t)
;; the best way to config it is to see it's lang-mode's key words.
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
;;                                end of rotate                               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               begin of minor                               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defvar my-last-buffer nil
;;   "Stores the last buffer before switching.")
;; (defun my-toggle-buffer ()
;;   "Toggle between current buffer and last visited buffer."
;;   (interactive)
;;   (let ((current (current-buffer)))
;;     (when (and my-last-buffer
;;                (buffer-live-p my-last-buffer)
;;                (not (eq current my-last-buffer)))
;;       (switch-to-buffer my-last-buffer))
;;     (setq my-last-buffer current)))

;; (defun my/delete-all-duplicate-lines ()
;;   "Delete all lines in the buffer that appear more than once."
;;   (interactive)
;;   (let ((seen (make-hash-table :test 'equal))
;;         (dups (make-hash-table :test 'equal)))
;;     ;; 第一次遍历：找重复行
;;     (save-excursion
;;       (goto-char (point-min))
;;       (while (not (eobp))
;;         (let ((line (string-trim-right
;;                      (buffer-substring-no-properties
;;                       (line-beginning-position)
;;                       (line-end-position)))))
;;           (if (gethash line seen)
;;               (puthash line t dups)
;;             (puthash line t seen)))
;;         (forward-line 1)))
;;     ;; 第二次遍历：删掉所有出现过多次的行
;;     (save-excursion
;;       (goto-char (point-min))
;;       (while (not (eobp))
;;         (let* ((beg (line-beginning-position))
;;                (end (line-end-position))
;;                (line (string-trim-right
;;                       (buffer-substring-no-properties beg end))))
;;           (if (gethash line dups)
;;               (delete-region beg (1+ end)) ;; 删除整行
;;             (forward-line 1)))))))

;; (defun my/delete-line-and-append-to-hhh ()
;;   "Delete the current line and append it to a file named 'hhh' in the current directory."
;;   (interactive)
;;   (let* ((current-file (buffer-file-name))
;;          (dir (if current-file
;;                   (file-name-directory current-file)
;;                 default-directory))     ;; fallback when buffer has no file
;;          (target-file (expand-file-name "hhh" dir))
;;          (line (thing-at-point 'line t)))
;;     ;; Append line to file
;;     (with-temp-buffer
;;       (insert line)
;;       (write-region (point-min) (point-max) target-file t))
;;     ;; Delete the current line in original buffer
;;     (delete-region (line-beginning-position)
;;                    (line-beginning-position 2))))

(defun donothing () (interactive)(message ""))
;; (defun sayshit () (interactive)(message "You Are Note Suppose To Use This Key!"))

(defvar compilation-show 1
  "Current compilation buffer mode, either 0 for show or  1 for close.")
(defun my-next-error() (interactive)
       (if (get-buffer-window "*compilation*")
           (setq compilation-show nil)
         (setq compilation-show 1))
       (next-error))
(defun my-previous-error() (interactive)
       (if (get-buffer-window "*compilation*")
           (setq compilation-show nil)
         (setq compilation-show 1))
       (previous-error))
(add-hook 'next-error-hook (lambda () (interactive)
                             (when (and compilation-show (get-buffer-window "*compilation*"))
                               (if (>= (count-windows) 2)
                                   (progn
                                     (switch-to-buffer "*compilation*")
                                     (previous-buffer)
                                     (other-window -1)
                                     (delete-window (get-buffer-window "*compilation*")))))))
(add-hook 'previous-error-hook (lambda () (interactive)
                                 (when (and compilation-show (get-buffer-window "*compilation*"))
                                   (if (>= (count-windows) 2)
                                       (progn
                                         (switch-to-buffer "*compilation*")
                                         (previous-buffer)
                                         (other-window -1)
                                         (delete-window (get-buffer-window "*compilation*")))))))

(defun consult-ripgrep-symbol-at-point  ()
  "Search for a line matching the symbol found near point."
  (interactive)
  (consult-ripgrep  (project-root (project-current t))
                    (or (thing-at-point 'symbol))))

;; (consult-ripgrep "/home/leeao/.me/post-init.el" "ensure")

(defun hibernatecall()
  (interactive)
  (if (yes-or-no-p "(really hibernate ? yes or no)")
      (progn   (message "emacs睡着了，不要吵醒它。")
               (call-process "systemctl" nil nil nil "hibernate"))
    (keyboard-quit)))

(delete-selection-mode 1)
(display-line-numbers-mode -1)
(display-time-mode -1)
(electric-pair-mode 1)
(global-eldoc-mode -1)
(global-font-lock-mode 1)
(global-hide-mode-line-mode 1)
(repeat-mode -1)
(show-paren-mode -1)
(tooltip-mode -1)
(window-divider-mode 1)
(winner-mode -1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                end of minor                                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
