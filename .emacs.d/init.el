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

;; disable auto pair for `<` in org-mode
;; since it will interface with code block
;; complete in org-mode
;; https://emacs.stackexchange.com/questions/26225/dont-pair-quotes-in-electric-pair-mode
(add-hook 'org-mode-hook (lambda ()
           (setq-local electric-pair-inhibit-predicate
                   (lambda (c)
                  (if (char-equal c ?<) t (electric-pair-default-inhibit c))))))

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		shell-mode-hook
                eshell-mode-hook
                term-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

 
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
  :custom ((doom-modeline-height 15)))

;; (load-theme 'modus-vivendi t)


(use-package ivy
  :diminish
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

(use-package swiper)

;; (use-package ivy-hydra)


(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))) 

;; enable fuzzy searching in ivy, but not swiper
;; https://oremacs.com/2016/01/06/ivy-flx/
(setq ivy-re-builders-alist
      '((swiper . regexp-quote)
	;; 让 counsel-projectile-rg 不要用 fuzzy find，否则会搜到很多无用的东西
	;; 它底层调用的是 counsel-rg
	;; https://github.com/ericdanan/counsel-projectile/issues/113
	(counsel-rg . ivy--regex-plus)
        (t      . ivy--regex-fuzzy))) 

;; ivy 自己的 fuzzy find 的排序太烂了
;; 还好它在文档里提到了 flx
;; https://oremacs.com/swiper/#ivy--regex-fuzzy
;; https://github.com/lewang/flx
(use-package flx)

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
  ;; use SPC for leader key
  ;; lf is short for linuxfish
  (general-create-definer lf/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (lf/leader-keys
    "t"  '(:ignore t :which-key "change display text")
    "ts" '(hydra-text-scale/body :which-key "scale text")

	"i" '(open-init :which-key "open init file")

    ;; "l" '(counsel-load-theme :which-key "load theme")

    "p" '(projectile-command-map :which-key "project")
    ;; what is '/body'?
    ;; it's generated by defhydra
    ;; https://github.com/abo-abo/hydra/blob/master/hydra-examples.el
    "w" '(hydra-window-scale/body :which-key "scale window")
    "f" '(counsel-find-file :which-key "find file")
    "g" '(magit-status :which-key "magit status")
    "d" '(dired-jump :which-key "dired")
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

  ;; use SPC as leader key for window management
  ;; https://stackoverflow.com/questions/33725550/emacs-evil-general-window-movement-remap
  ;; evil-window-map 在 evil-mode 中有定义
  ;; 其它的 keybinding 的优化可以参考这个思路
  (define-key evil-motion-state-map " " 'evil-window-map)
  (define-key evil-visual-state-map " " 'evil-window-map)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-undo-system 'undo-redo)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

;; for folding / unfolding in evil-mode using za/zc/zm/zr
;; https://www.reddit.com/r/emacs/comments/4h1f2d/question_how_can_i_enable_hideshow_for_all_its/
(add-hook 'prog-mode-hook 'hs-minor-mode)

;; 让`-'也算做一个 word，这样就可以在有`-'存在时也能正常使用cw、dw
;; https://emacs.stackexchange.com/questions/9583/how-to-treat-underscore-as-part-of-the-word
;; https://evil.readthedocs.io/en/latest/faq.html#underscore-is-not-a-word-character
(add-hook 'prog-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
;; 不知道为啥在设置了下面这一行后，在编辑 elisp 的时候仍然不能把`-'当成一个 word
;; 可能是需要重启 emacs，等重启的时候再看看吧
(add-hook 'emacs-lisp-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))

;; ;; https://github.com/gabesoft/evil-mc
;; ;; https://github.com/hlissner/evil-multiedit
;; ;; https://www.reddit.com/r/emacs/comments/iu0euj/getting_modern_multiple_cursors_in_emacs/
;; (use-package evil-mc)


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

;; https://github.com/emacs-evil/evil-surround
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

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
  :hook (org-mode . lf/org-mode-setup)
  ;; shortcut for evecute code block
  :bind (("C-c r" . org-babel-execute-src-block))
  :config
  (setq org-ellipsis " ▾"
	org-pretty-entities t)
  (lf/org-font-setup))

;; code block autocomplete, eg, use `<el`
(with-eval-after-load 'org
  ;; This is needed as of Org 9.2
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python")))

;; no confirm for babel-evaluate
(setq org-confirm-babel-evaluate nil)


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
  ;; 不知为啥在 emacs-lisp-mode 里不生效
  :hook ((lsp-mode emacs-lisp-mode) . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

;; (use-package company-box
;;   :hook (company-mode . company-box-mode))

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

;; ;; required by projectile-ripgrep
;; (use-package rg)

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

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'lf/rustic-mode-hook))

(defun lf/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))


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

;; line wrapping
;; https://stackoverflow.com/questions/950340/how-do-you-activate-line-wrapping-in-emacs
;; 默认不 wrap，可以用 `toggle-truncate-lines'来切换
(setq-default truncate-lines t)
(setq truncate-partial-width-windows nil) ;; for vertically-split windows

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


;; 如何恢复之前打开过的窗口？
;; https://stackoverflow.com/questions/7641755/maximizing-restoring-a-window-in-emacs
