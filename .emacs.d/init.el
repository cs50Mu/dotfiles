;; disable welcome msg
(setq inhibit-startup-message t)

(defvar lf/default-font-size 180)

;;; I prefer cmd key for meta
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)

;; disable toolbar
(tool-bar-mode -1)
;; disable scrollbar
(scroll-bar-mode -1)
;; disable menubar
(menu-bar-mode -1)

;; 在 M-x 里按 M-n、M-p 来查看命令历史
(setq history-length 25)
(savehist-mode 1)

;; remember and restore the last cursor location of opened files
(save-place-mode 1)

;; use 4 spaces for one tab
;; and no tabs
;; https://www.emacswiki.org/emacs/NoTabs
(setq-default indent-tabs-mode nil
			  tab-width 4)

;; recent files
;; https://stackoverflow.com/questions/50417/how-do-i-get-list-of-recent-files-in-gnu-emacs
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; display line numbers
(column-number-mode)
(global-display-line-numbers-mode 1)

;; enable auto pair
(electric-pair-mode)

;; disable auto pair for `<' and `\[' in org-mode
;; since it will interface with code block
;; complete in org-mode
;; https://emacs.stackexchange.com/questions/26225/dont-pair-quotes-in-electric-pair-mode
(defun lf/electric-pair-inhibit-ignore ()
  (setq-local electric-pair-inhibit-predicate
              (lambda (c)
                (if (or (char-equal c ?<) (char-equal c ?\[))
                t
                (electric-pair-default-inhibit c)))))
(add-hook 'org-mode-hook 'lf/electric-pair-inhibit-ignore)

;; Disable line numbers for some modes
(defun lf/disable-line-number ()
  (display-line-numbers-mode 0))
(dolist (mode '(org-mode-hook
                shell-mode-hook
                eshell-mode-hook
                term-mode-hook))
  (add-hook mode 'lf/disable-line-number))

 
;; font && size
(set-face-attribute 'default nil :font "Fira Code Retina" :height lf/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height 150)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Fira Code Retina" :height 150 :weight 'regular)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)


;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platfor ms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell)

;; This sets $MANPATH, $PATH and exec-path from your shell, but only
;; when executed in a GUI frame on OS X and Linux.
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; If you launch Emacs as a daemon from systemd or similar
(when (daemonp)
  (exec-path-from-shell-initialize))

;; theme 
(use-package all-the-icons)

(use-package doom-themes
  ;; :init (load-theme 'doom-dracula t))
  ;; :init (load-theme 'doom-one t))
  :init (load-theme 'doom-vibrant t))

;; 需要 M-x all-the-icons-install-fonts
;; 否则 modeline 的图标会乱码
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom (
           (doom-modeline-height 15)
           (doom-modeline-time-icon nil)
           ))

(advice-add #'fit-window-to-buffer :before (lambda (&rest _) (redisplay t)))

;; (load-theme 'modus-vivendi t)


(use-package ivy
  :diminish ivy-mode
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

;; ;; 有 bug，输入法不能跟随
;; (use-package ivy-posframe
;;   :diminish ivy-posframe-mode
;;   ;; The :custom keyword allows customization of package custom variables.
;;   :custom
;;   (ivy-posframe-height-alist
;;    '((swiper . 15)
;;      (t . 10)))
;;   (ivy-posframe-display-functions-alist
;;    '((complete-symbol . ivy-posframe-display-at-point)
;;      (counsel-describe-function . nil)
;;      (counsel-describe-variable . nil)
;;      (swiper . nil)
;;      (swiper-isearch . nil)
;;      (t . ivy-posframe-display-at-frame-center)
;;      )
;;    )
;;   :config
;;   (ivy-posframe-mode 1)
;;   )

;; More friendly interface for ivy
(use-package ivy-rich
  :config
  (setcdr (assq t ivy-format-functions-alist)
          #'ivy-format-function-line)
  (ivy-rich-mode 1)
  )

(use-package swiper
  :after ivy
  )

(use-package counsel
  :after (ivy amx)
  :diminish counsel-mode
  :custom
  ;; enable fuzzy searching in ivy, but not swiper
  ;; https://oremacs.com/2016/01/06/ivy-flx/
  (ivy-re-builders-alist
   '((swiper . regexp-quote)
     ;; 让 counsel-projectile-rg 不要用 fuzzy find，否则会搜到很多无用的东西
     ;; 它底层调用的是 counsel-rg
     ;; https://github.com/ericdanan/counsel-projectile/issues/113
     (counsel-rg . ivy--regex-plus)
     (t      . ivy--regex-fuzzy))) 
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         ("C-x C-r" . counsel-recentf)
         ("C-h v" . counsel-describe-variable)
         ("C-h f" . counsel-describe-function)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))) 

;; ivy 自己的 fuzzy find 的排序太烂了
;; 还好它在文档里提到了 flx
;; https://oremacs.com/swiper/#ivy--regex-fuzzy
;; https://github.com/lewang/flx
(use-package flx)

;; prioritizing your most-used commands in the completion list
;; https://github.com/DarwinAwardWinner/amx
;; 看到 Prot 在一个视频中说他已经换成 prescient.el 了，目前用着这个还好，先不换了 
(use-package amx
  :after ivy
  :custom
  (amx-backend 'auto)
  (amx-save-file "~/.emacs.d/amx-items")
  (amx-history-length 50)
  ;; (amx-show-keybindings nil)
  :config
  (amx-mode 1)
  )

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(defun open-init ()
  "Open the init file."
  (interactive)
  (find-file user-init-file))

(use-package general
  :config
  ;; use C-; to switch input source
  (general-define-key "C-;" 'sis-switch)

  (general-define-key "C-c c" 'org-capture)
  (general-define-key "C-c a" 'org-agenda)

  ;; use SPC for leader key
  ;; lf is short for linuxfish
  (general-create-definer lf/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (lf/leader-keys
    "ts" '(hydra-text-scale/body :which-key "scale text")
	"i" '(open-init :which-key "open init file")
    ;; "l" '(counsel-load-theme :which-key "load theme")
    "p" '(projectile-command-map :which-key "project")
    "w" '(evil-window-map :which-key "window management")
    ;; 常用的窗口管理提取到外面方便使用
    "s" '(evil-window-split :which-key "split window horizontally")
    "v" '(evil-window-vsplit :which-key "split window vertically")
    "h" '(evil-window-left :which-key "move cursor to left")
    "l" '(evil-window-right :which-key "move cursor to right")
    "j" '(evil-window-down :which-key "move cursor to down")
    "k" '(evil-window-up :which-key "move cursor to up")
    "c" '(evil-window-delete :which-key "close window")
    "o" '(delete-other-windows :which-key "close other windows")

    "f" '(counsel-find-file :which-key "find file")
    "F" '(find-file-other-window :which-key "find file other window")
    "g" '(magit-status :which-key "magit status")
    ;; "d" '(dired-jump :which-key "dired")
    ; try ranger
    "d" '(ranger :which-key "ranger")
    "SPC" '(counsel-ibuffer :which-key "switch buffer")
    "bh" '(previous-buffer :which-key "switch to previous buffer")
    "bl" '(next-buffer :which-key "switch to next buffer")
    ))

;; fix for "Key sequence starts with non-prefix key"
;; https://emacs.stackexchange.com/questions/68328/general-el-error-key-sequence-starts-with-non-prefix-key
(general-auto-unbind-keys)


;; 可以用 `C-z` 来 toggle evil mode
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  ;; search by symbol
  ;; `-'默认不是一个 word
  (setq evil-symbol-word-search t)
  ;; (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  ;; paste from clipboard
  (define-key evil-insert-state-map (kbd "M-v") 'clipboard-yank)

  ;; ;; use SPC as leader key for window management
  ;; ;; https://stackoverflow.com/questions/33725550/emacs-evil-general-window-movement-remap
  ;; ;; evil-window-map 在 evil-mode 中有定义
  ;; ;; 其它的 keybinding 的优化可以参考这个思路
  ;; (define-key evil-motion-state-map " " 'evil-window-map)
  ;; (define-key evil-visual-state-map " " 'evil-window-map)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-global-set-key 'motion "gs" 'avy-goto-char-timer)

  (evil-set-undo-system 'undo-redo)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

;; for folding / unfolding in evil-mode using za/zc/zm/zr
;; https://www.reddit.com/r/emacs/comments/4h1f2d/question_how_can_i_enable_hideshow_for_all_its/
(add-hook 'prog-mode-hook 'hs-minor-mode)

;; 让`-'和`_'也算做一个 word，这样就可以在有`-'和`_'存在时也能正常使用cw、dw
;; https://emacs.stackexchange.com/questions/9583/how-to-treat-underscore-as-part-of-the-word
;; https://evil.readthedocs.io/en/latest/faq.html#underscore-is-not-a-word-character
(defun lf/hyphen-as-word ()
  (modify-syntax-entry ?_ "w")
  (modify-syntax-entry ?- "w")
  )
(add-hook 'prog-mode-hook #'lf/hyphen-as-word)
(add-hook 'emacs-lisp-mode-hook #'lf/hyphen-as-word)

;; evil 自带的 jump 的逻辑有点怪，是以 buffer 为单位来 jump 的，
;; 并不是 location，参考：https://github.com/emacs-evil/evil/issues/684
;; 虽然在这个 issue 里，最后说解决了这个问题，但我（2022-05-23）实测并没有
;; 然后又看到有人推荐用 better-jumper 这个库，然而按照它的文档来操作后并没有解决
;; 问题。最后，是在 /r/emacs 里看到有人分享他的配置，实测可以正常工作
;; https://www.reddit.com/r/emacs/comments/ntnhkc/how_i_jump_around_emacs_with_betterjumper/
(use-package better-jumper
    :custom
    ; this is the key to avoiding conflict with evils jumping stuff
    (better-jumper-use-evil-jump-advice nil)
    :config  
    (better-jumper-mode 1)  
    ; this lets me toggle between two points. (adapted from evil-jump-backward-swap)
    (evil-define-motion better-jumper-toggle (count)    
      (let ((pnt (point)))      
        (better-jumper-jump-backward 1)      
        (better-jumper-set-jump pnt)))  
                           
    ; this is the key here. This advice makes it so you only set a jump point
    ; if you move more than one line with whatever command you call. For example
    ; if you add this advice around evil-next-line, you will set a jump point
    ; if you do 10 j, but not if you just hit j. I did not write this code, I 
    ; I found it a while back and updated it to work with better-jumper.
    (defun my-jump-advice (oldfun &rest args)    
      (let ((old-pos (point)))      
        (apply oldfun args)      
        (when (> (abs (- (line-number-at-pos old-pos) (line-number-at-pos (point))))                                   
                  1)	
          (better-jumper-set-jump old-pos))))

    ; jump scenarios
    (advice-add 'evil-next-line :around #'my-jump-advice)  
    (advice-add 'evil-previous-line :around #'my-jump-advice)  
    (advice-add 'helm-swoop :around #'my-jump-advice)  
    (advice-add 'evil-goto-definition :around #'my-jump-advice)  
    (advice-add 'evil-goto-mark  :around #'my-jump-advice))
    ; ... whenever you want a new jump scenario just use the above pattern. 


(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-commentary
  :config (evil-commentary-mode))

;; 在 ivy-occur 里不要启用 evil-commentary
;; 因为我想用 gc 来开启 ivy-occur 里的 ivy-calling 模式
(defun lf/disable-evil-commentary ()
  (evil-commentary-mode -1)
  )
(add-hook 'ivy-occur-grep-mode-hook #'lf/disable-evil-commentary)

;; https://github.com/emacs-evil/evil-surround
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;; 更强大的类似 vim 里的 f/t
;; https://github.com/hlissner/evil-snipe
(use-package evil-snipe
  :config
  (evil-snipe-mode 1)
  (evil-snipe-override-mode 1)
)

;; https://github.com/hlissner/evil-snipe#conflicts-with-other-plugins
(add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode)


(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(defhydra hydra-window-scale (:timeout 4)
  "scale window"
  ("j" evil-window-increase-height "increase height")
  ("k" evil-window-decrease-height "decrease height")
  ("h" evil-window-increase-width "increase width")
  ("l" evil-window-decrease-width "decrease width")
  ("f" nil "finished" :exit t))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(defun lf/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; make the text centered in org mode
(defun lf/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . lf/org-mode-visual-fill))

(defun lf/org-font-setup ()

  ;; Increase the size of various headings
  (set-face-attribute 'org-document-title nil :font "Fira Code Retina" :weight 'bold :height 1.3)

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    ;; (set-face-attribute (car face) nil :font "Iosevka Aile" :weight 'medium :height (cdr face)))
    (set-face-attribute (car face) nil :font "Fira Code Retina" :weight 'medium :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(use-package org
  :hook
  (org-mode . lf/org-mode-setup)
  ;; shortcut for evecute code block
  :bind (
         ;; ("M-RET" . org-meta-return)
         ("C-c r" . org-babel-execute-src-block)
         :map org-mode-map
         ;; disable these
         ;; according to
         ;; http://doc.norang.ca/org-mode.html#AgendaSetup
         ("C-c [" . nil)
         ("C-c ]" . nil)
         ("C-c ;" . nil)
         )
  :custom
  ;; Fast todo selection allows changing
  ;; from any task todo state to any other state directly
  (org-use-fast-todo-selection t)
  (org-ellipsis " ▾"
	org-pretty-entities t)

  ;; 不要 split line，永远跳到新行
  ;; https://irreal.org/blog/?p=6297
  (org-M-RET-may-split-line '((default . nil)))

  (org-directory "~/Dropbox/org")
  (org-agenda-files '("~/Dropbox/org/task.org"))
  ;; always no blank line when create new entry
  ;; https://stackoverflow.com/questions/28351465/emacs-orgmode-do-not-insert-line-between-headers
  (org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))
  ;; ;; 调整 tag 在 agenda view 里显示的位置
  ;; (org-agenda-tags-column (- 20 (window-body-width)))

  ;; templates
  (org-capture-templates
        '(("t" "Tasks")
          ("tw" "Work tasks" entry
           (file+headline "task.org" "Work")
           "* TODO %^{Task} :work:\n SCHEDULED: %^t\n %a\n %?")
          ("tp" "Personal task" entry
           (file+headline "task.org" "折腾记录")
           "* TODO %^{要折腾啥} :personal:\n SCHEDULED: %^t\n %a\n %?")
          ("j" "Journal" entry
           (file+datetree "journal.org")
          "* %U - %^{标题}\n  %?")
          ("i" "Ideas" entry
           (file "idea.org")
          "* %U - %^{标题}\n  %?")
          ("c" "Code Snippet" entry
           (file "snippet.org")
          "* %?\t:%\\1:\n#+BEGIN_SRC %^{lanauage}\n%i\n#+END_SRC")
          ))
  (org-agenda-custom-commands
   '(("h" "Agenda and Home-related tasks"
      ((agenda "")
       (tags-todo "home")
       (alltodo "")))
     ("o" "Agenda and Office-related tasks"
      ((agenda "")
       (tags-todo "work")
       (tags "office")))))

  ;; ;; not display DONE task in agenda view
  ;; ;; https://stackoverflow.com/questions/8281604/remove-done-tasks-from-agenda-view
  ;; (org-agenda-skip-function-global '(org-agenda-skip-entry-if 'todo 'done))
  :config
  (lf/org-font-setup))

;; code block autocomplete, eg, use `<el`
(with-eval-after-load 'org
  ;; This is needed as of Org 9.2
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("org" . "src org"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python")))

;; languages enabled for evaluation
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (python . t)
   (emacs-lisp . t)
   (shell . t)
   ))

;; no confirm for babel-evaluate
(setq org-confirm-babel-evaluate nil)

;; allow use j k in `org-agenda'
(defun lf/org-agenda-jk ()
  (define-key org-agenda-mode-map "j" 'evil-next-line)
  (define-key org-agenda-mode-map "k" 'evil-previous-line)
  )
(add-hook 'org-agenda-mode-hook #'lf/org-agenda-jk)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  ;; :hook (lsp-mode . lf/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  ;; 开启后 doc 多的时候会占将近一半屏幕
  (lsp-eldoc-render-all nil)
  ;; 在输入函数的时候不要显示其文档，否则跳来跳去有点乱
  (lsp-signature-render-documentation nil)
  (lsp-idle-delay 0.6)
  ;; 暂时禁用 inlay-hints，有点卡，看起来也有点乱
  ;; ;; enable / disable the hints as you prefer:
  ;; (lsp-rust-analyzer-server-display-inlay-hints t)
  ;; (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  ;; (lsp-rust-analyzer-display-chaining-hints nil)
  ;; (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  ;; (lsp-rust-analyzer-display-closure-return-type-hints t)
  ;; (lsp-rust-analyzer-display-parameter-hints nil)
  ;; (lsp-rust-analyzer-display-reborrow-hints nil)
  :config
  (lsp-enable-which-key-integration t))

;; ;; TODO: 特定情况下启动 flycheck?
;; (use-package flycheck
;;   :init (global-flycheck-mode))

;; golang
(defun lsp-go-before-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

;; ;; TODO: yasnippet?
;; ;; 这种写法不能启动 lsp。。
;; (use-package go-mode
;;   :hook (lsp-deferred lsp-go-before-save-hooks))
(use-package go-mode
  :init 
  (add-hook 'go-mode-hook #'lsp-go-before-save-hooks)
  (add-hook 'go-mode-hook #'lsp-deferred))

;; remap godef-jump to lsp-find-definition, as it is much faster
;; 本来想把`gd`重新绑定到 lsp 上的，死活不行。。
(define-key go-mode-map [remap godef-jump] 'lsp-find-definition)

;; K 来显示文档
(define-key go-mode-map [remap godef-describe] 'lsp-describe-thing-at-point)

;; for golang struct tag
;; https://github.com/brantou/emacs-go-tag
(use-package go-tag)


;; for code completion
(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))


;; enable autocomplete for emacs lisp
;; only enable fuzzy matching for elisp
;; because fuzzy match could not work properly with goang and rust
(defun lf/enable-complete-elisp()
  (company-mode)
  (company-fuzzy-mode)
  )
(add-hook 'emacs-lisp-mode-hook #'lf/enable-complete-elisp)

;; for fuzzy match
;; https://github.com/jcs-elpa/company-fuzzy
(use-package company-fuzzy
  :after
  company
  :custom
  ((company-fuzzy-sorting-backend 'flx)
   ;; https://github.com/jcs-elpa/company-fuzzy/pull/19
   (company-fuzzy-passthrough-backends '(company-capf))
   (company-fuzzy-prefix-on-top nil)
   (company-fuzzy-trigger-symbols '("." "->" "<" "\"" "'" "@"))
   )
  )

;; ;; 需要保证`company-backends' 只有 company-fuzzy-all-other-backends 一个才行
;; ;; https://github.com/jcs-elpa/company-fuzzy#-why-is-company-fuzzy-not-working
;; ;; https://github.com/jcs-elpa/company-fuzzy/issues/2
;; ;; 貌似只有启动 lsp mode 的情况下 company-backends 才会多出一个 `company-capf'
;; ;; 而 elisp 补全不需要启动 lsp，所以先注释掉
;; (defun lf/set-company-backends ()
;;   (setq-local company-backends '(company-fuzzy-all-other-backends))
;;   )
;; (add-hook 'company-fuzzy-mode-hook #'lf/set-company-backends)

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  ;; Mac 上的 ls 不支持，需要 coreutils 的 ls
  ;; https://emacs-china.org/t/macos-dired/21205/12
  :custom
  ((insert-directory-program "gls" dired-use-ls-dired t)
   (dired-listing-switches "-agho --group-directories-first"))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer))

;; https://github.com/crocket/dired-single
;; for dired-single-up-directory to work
;; 实际看了下，并不能保证只在一个 buffer 里打开目录
;; 考虑去掉
;; https://www.emacswiki.org/emacs/DiredReuseDirectoryBuffer
(use-package dired-single)

;; hide dotfiles
(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  ;; not show info in the minibar
  (setq dired-hide-dotfiles-verbose nil)
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(setq insert-directory-program "gls" dired-use-ls-dired t)
(setq dired-listing-switches "-al --group-directories-first")

;; try ranger
(use-package ranger
  :config
  (setq ranger-preview-file nil))
;; set ranger as the default directory manager
(ranger-override-dired-mode t)

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/work/gitlab.luojilab.com")
    ;; path 是一个 list paths
    ;; 每个 path 都支持定义搜索深度
    (setq projectile-project-search-path '(("~/work/gitlab.luojilab.com" . 3))))
  (setq projectile-switch-project-action #'projectile-dired))

;; use rg to search everywhere
(use-package rg
  :config
  ;; https://www.youtube.com/watch?v=4qLD4oHOrlc
  (rg-define-search lf/grep-vc-or-dir
    :query ask
    :format regexp
    :files "everything"
    :dir (let ((vc (vc-root-dir)))
           (if vc
               vc
             default-directory
             )
           )
    :confirm prefix
    :flags ("--hidden -g !.git")
    )

  :bind
  (
   ;; 在当前目录或者当前项目根目录下搜索
   ("M-s g" . lf/grep-vc-or-dir)
   )
  )

(use-package counsel-projectile
  :config (counsel-projectile-mode))

;; ;; https://github.com/lujun9972/emacs-document/blob/master/emacs-common/Smartparens%E7%94%A8%E6%B3%95%E8%AF%A6%E8%A7%A3.org
;; (use-package smartparens-config
;;   :ensure smartparens
;;   :config
;;   (progn
;;     (show-smartparens-global-mode t)))

;; (add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
;; (add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode)


;; rust related
(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ;; ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ;; ("K" . lsp-describe-thing-at-point)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c n" . flycheck-next-error)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; evil模式下 K 绑定在 evil-lookup, 而 evil-lookup-func
  ;; 默认是在 `woman'，做如下更改后，在 rust 里可以按 K 来显示文档
  (setq evil-lookup-func #'lsp-describe-thing-at-point)

  ;; 有时候发现 save 后 format 不起作用， rustic 实际上依赖 rust-mode
  ;; 发现手动切换一下 rust-mode 后再 save 就好了。。
  ;; 参考：https://github.com/brotzeit/rustic/issues/343
  ;; 若实在不行，可以`M-x'在命令行执行`rustic-format-buffer'先顶一下
  ;; (20220805) 新发现，按`:w'是可以 format 的，`:wa' 不行
  ;; 所以看起来不是 rustic 的问题（实测 `:wa' 貌似并没有触发 save file）
  ;; 提了个issue：https://github.com/brotzeit/rustic/issues/424
  (setq rustic-format-trigger 'on-save)
  ;; (add-hook 'rustic-mode-hook 'lf/rustic-mode-hook)
  (add-hook 'rustic-mode-hook 'rustic-mode-auto-save-hook))

(defun lf/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))

;; https://github.com/brotzeit/rustic#rustfmt
(defun rustic-mode-auto-save-hook ()
  "Enable auto-saving in rustic-mode buffers."
  (when buffer-file-name
    (setq-local compilation-ask-about-save nil)))


;; 修复在 rust 中 minibuffer 里不能正确显示函数签名的问题
;; https://emacs-china.org/t/lsp-mode-rust-go-eldoc/13686
;; https://github.com/emacs-lsp/lsp-mode/pull/1740
;; https://github.com/scturtle/dotfiles/blob/f1e087e247876dbae20d56f944a1e96ad6f31e0b/doom_emacs/.doom.d/config.el#L74-L85
(cl-defmethod lsp-clients-extract-signature-on-hover (contents (_server-id (eql rust-analyzer)))
  (-let* (((&hash "value") contents)
          (groups (--partition-by (s-blank? it) (s-lines (s-trim value))))
          (sig_group (if (s-equals? "```rust" (car (-third-item groups)))
                         (-third-item groups)
                       (car groups)))
          (sig (--> sig_group
                    (--drop-while (s-equals? "```rust" it) it)
                    (--take-while (not (s-equals? "```" it)) it)
                    (--map (s-trim it) it)
                    (s-join " " it))))
    (lsp--render-element (concat "```rust\n" sig "\n```"))))

;; https://github.com/flycheck/flycheck-rust
;; https://www.reddit.com/r/emacs/comments/cw96wp/my_emacs26_setup_for_rust/
(use-package flycheck-rust)
(with-eval-after-load 'rustic-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))


;; js && vue
(use-package vue-mode
  :init
  (add-hook 'vue-mode-hook #'lsp-deferred))

(use-package js2-mode
  :init
  (add-hook 'js2-mode-hook #'lsp-deferred)
  :mode
  (("\\.js\\'" . js2-mode))
  )


;; move customization variables to a seperate file and load it
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; automaticlly load file changes from disk
(global-auto-revert-mode 1)

;; automaticlly load dir changes, too.
;; useful for dired mode
(setq global-auto-revert-non-file-buffers t)

(windmove-default-keybindings)

;; auto focus on new help window
(setq help-window-select t)

;; the t parameter apends to the hook, instead of prepending
;; this means it'd be run after other hooks that might fiddle
;; with the frame size
;; refer to: https://emacsredux.com/blog/2020/12/04/maximize-the-emacs-frame-on-startup/
(add-hook 'window-setup-hook 'toggle-frame-maximized t)

;; scroll margin
;; Emacs 的 scroll method 跟 vim 的不一样
;; 有时间具体看下：https://www.gnu.org/software/emacs/manual/html_node/emacs/Auto-Scrolling.html
(setq scroll-margin 5)

;; highlight current line
(global-hl-line-mode 1)

;; show matching parens etc..
(show-paren-mode 1)

;; make all backup / autosave files go into a directory
;; https://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files
;; https://emacs.stackexchange.com/questions/17210/how-to-place-all-auto-save-files-in-a-directory
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Backquote.html
(setq backup-directory-alist `(("." . "~/.emacs-saves"))
      delete-old-versions t
      kept-new-versions 4
      kept-old-versions 2
      version-control t
      backup-by-copying t
      auto-save-file-name-transforms
      `((".*" "~/.emacs-saves/" t)))

;; always confirm to exit emacs
;; https://www.reddit.com/r/emacs/comments/uwk9kx/make_q_or_wq_not_killl_emacs_in_evil_mode/
(setq confirm-kill-emacs #'yes-or-no-p)

;; self defined functions
(defun lf/google-search ()
  "Googles a query or region if any."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (if mark-active
        (buffer-substring (region-beginning) (region-end))
      (read-string "Google: ")))))

;; ;; line wrapping
;; ;; https://stackoverflow.com/questions/950340/how-do-you-activate-line-wrapping-in-emacs
;; ;; 默认不 wrap，可以用 `toggle-truncate-lines'来切换
;; (setq-default truncate-lines t)
;; (setq truncate-partial-width-windows nil) ;; for vertically-split windows

;; 还是折行吧，不折行太难受了
;; 上面的配置貌似也不起作用。。
(setq-default truncate-lines nil)

;; display time in modeline
;; https://www.emacswiki.org/emacs/DisplayTime
;; https://www.reddit.com/r/emacs/comments/kf3tsq/what_is_this_number_after_the_time_in_the_modeline/
(setq display-time-default-load-average nil)
(setq display-time-format "%H:%M")
(display-time-mode 1)


;; 内置输入法
(use-package sis
  ;; :hook
  ;; enable the /follow context/ and /inline region/ mode for specific buffers
  ;; (((text-mode prog-mode) . sis-context-mode)
  ;;  ((text-mode prog-mode) . sis-inline-mode))

  :config
  ;; For MacOS
  (sis-ism-lazyman-config

   ;; English input source may be: "ABC", "US" or another one.
   ;; "com.apple.keylayout.US"
   "com.apple.keylayout.ABC"

   ;; Other language input source: "rime", "sogou" or another one.
   ;; "im.rime.inputmethod.Squirrel.Rime"
   "com.sogou.inputmethod.sogou.pinyin")

  ;; enable the /cursor color/ mode
  (sis-global-cursor-color-mode t)
  ;; enable the /respect/ mode
  (sis-global-respect-mode t)
  ;; enable the /context/ mode for all buffers
  (sis-global-context-mode t)
  ;; enable the /inline english/ mode for all buffers
  (sis-global-inline-mode t)
  )

;; 拼音按首字母搜索
;; https://github.com/laishulu/evil-pinyin
(use-package evil-pinyin
  :init
  (setq-default evil-pinyin-scheme 'simplified-xiaohe-all)
  (setq-default evil-pinyin-with-search-rule 'custom)

  :config
  ;; 作用是 / 会被映射成 evil-ex-search-forward，不然的话会是 evil-search-forward
  ;; 这个插件貌似只支持使用 evil-ex-search-forward
  ;; 参考：https://emacs-china.org/t/evil-search-pinyin/13455/40
  (evil-select-search-module 'evil-search-module 'evil-search)
  (global-evil-pinyin-mode))

;; https://github.com/abo-abo/avy
;; Avy allows to use the search mechanic to
;; efficiently move to another place **within the visible area.**
;; https://www.youtube.com/watch?v=zar4GsOBU0g&list=PLhXZp00uXBk4np17N39WvB80zgxlZfVwj&index=7
;; 只能在当前屏幕可见区域内移动，不能替代 /
;; 在 evil 的配置里把 `gs' 绑定到 avy-goto-char-timer 上了
(use-package avy)

(use-package evil-mc
  :config
  (global-evil-mc-mode  1))

;; evil-mc
(evil-define-key '(normal visual) 'global
  "gzm" #'evil-mc-make-all-cursors
  "gzu" #'evil-mc-undo-all-cursors
  "gzI" #'evil-mc-make-cursor-in-visual-selection-beg
  "gzA" #'evil-mc-make-cursor-in-visual-selection-end
  "gzn" #'evil-mc-make-and-goto-next-cursor
  "gzp" #'evil-mc-make-and-goto-prev-cursor
  "gzN" #'evil-mc-make-and-goto-last-cursor
  "gzP" #'evil-mc-make-and-goto-first-cursor)
;; `C-n' / `C-p' 用于在 cursor 之间移动
;; `M-n' / `M-p' 用于取消掉不想要的 cursor
(with-eval-after-load 'evil-mc
  (evil-define-minor-mode-key '(normal visual) evil-mc-mode
    (kbd "C-n") #'evil-mc-make-and-goto-next-cursor
    (kbd "M-n") #'evil-mc-skip-and-goto-next-cursor
    (kbd "C-p") #'evil-mc-make-and-goto-prev-cursor)
    (kbd "M-p") #'evil-mc-skip-and-goto-prev-cursor)


;; org 里 `M-<return>' 被绑定到 `org-ctrl-c-ret'
;; 我想绑定到 `org-meta-return'，折腾半天不行，后来发现
;; 是 evil mode 的问题，需要如下设置才行
;; https://github.com/noctuid/evil-guide#preventing-certain-keys-from-being-overridden
(general-override-mode)
(general-def 'normal 'override "M-<return>" 'org-meta-return)
;; what is '/body'?
;; it's generated by defhydra
;; https://github.com/abo-abo/hydra/blob/master/hydra-examples.el
;; this overrides the evil default keybinding for window split
;; I already binded horizontal split to `SPC s'
(general-def '(normal visual) 'override "SPC w s" '(hydra-window-scale/body :which-key "scale window"))

;; org-roam
(use-package org-roam
  :custom
  (org-roam-directory (file-truename "~/Dropbox/org/notes"))
  :bind
  (
   ("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n g" . org-roam-graph)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n c" . org-roam-capture)
   )
  :config
  (org-roam-db-autosync-mode)
  )

;; undo / redo window
;; winner mode is built in
(use-package winner
  :bind (
         :map evil-window-map
         ("u" . winner-undo)
         ("r" . winner-redo)
         )
  :config
  (winner-mode)
  )

(use-package vterm
  :custom
  (
   (vterm-max-scrollback 10000)
   )
  )

;; irc
(setq erc-server "irc.libera.chat"
      erc-nick "linuxfish"    
      erc-user-full-name "linuxfish"  
      erc-track-shorten-start 8
      erc-autojoin-channels-alist '(("irc.libera.chat" "#systemcrafters" "#emacs"))
      erc-kill-buffer-on-part t
            erc-auto-query 'bury)


;; 如何恢复之前打开过的窗口？
;; https://stackoverflow.com/questions/7641755/maximizing-restoring-a-window-in-emacs
