;; ;; Packages

(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(use-package no-littering
  :ensure t)

(use-package recentf
  :config
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))


(eval-when-compile
  (require 'use-package))

(use-package all-the-icons
  :ensure t)

(use-package dired
  :bind ("C-x C-j" . dired-jump)
  :config
  (setq dired-dwim-target t))

(use-package which-key
  :ensure t
  :config
  (which-key-mode 1))

(use-package define-word
  :ensure t)

(use-package whitespace
  :config
  (add-hook 'prog-mode 'whitespace-mode)
  (setq whitespace-style '(face tabs tab-mark trailing empty)))

(use-package git-timemachine
  :ensure t
  :defer t)

(use-package magit
  :ensure t
  :demand t
  :bind ("C-x g" . magit-status)
  :config
  (setq magit-repository-directories '(("~/projects/" . 2)))
  (setq magit-revision-show-gravatars 'author))

(use-package forge
  :ensure t)

(use-package diff-hl
  :ensure t
  :hook ((prog-mode . diff-hl-mode)
         (org-mode . diff-hl-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh)))

(use-package ag
  :ensure t
  :commands (ag ag-regexp ag-project))

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)
  (setq ivy-initial-inputs-alist nil)  ;; no default regex
  (setq ivy-use-virtual-buffers t)
  (setq magit-completing-read-function 'ivy-completing-read)
  (global-set-key (kbd "C-c r") 'ivy-resume)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (global-set-key (kbd "M-i") 'counsel-imenu)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

(use-package projectile
  :ensure t
  :bind-keymap ("C-x p" . projectile-command-map)
  :config
  (setq projectile-enable-caching t
        projectile-completion-system 'ivy
        projectile-file-exists-remote-cache-expire nil
        projectile-project-search-path '("~/projects")
        projectile-sort-order 'recently-active)
  (projectile-mode +1))

(use-package counsel-projectile
  :ensure t
  :after (projectile)
  :config
  (counsel-projectile-mode))

(use-package ibuffer-projectile
  :ensure t
  :config)

(use-package ibuffer
  :config
  (global-set-key (kbd "C-x C-b") 'ibuffer)
  (add-hook 'ibuffer-hook
    (lambda ()
      (ibuffer-projectile-set-filter-groups)
      (unless (eq ibuffer-sorting-mode 'alphabetic)
        (ibuffer-do-sort-by-alphabetic))))
  (setq ibuffer-formats
      '((mark modified read-only " "
              (name 18 18 :left :elide)
              " "
              (size 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              project-relative-file))))

(use-package minions
  :ensure t
  :config
  (minions-mode 1))

(use-package company
  :ensure t
  :config
  (global-company-mode t))

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode))

(use-package flyspell
  :hook ((prog-mode . flyspell-prog-mode)
         (text-mode . flyspell-mode)))

;; erc
(use-package erc
  :config
  (setq-default erc-echo-notices-in-minibuffer-flag t)
  (setq erc-rename-buffers t)
  (setq erc-lurker-hide-list '("JOIN" "QUIT"))
  (setq erc-lurker-threshold-time 3600)
  (setq erc-input-line-position -2))

(setq auth-sources '((:source "~/Dropbox/secrets/.authinfo.gpg")))

(use-package yasnippet  :ensure t
  :hook (prog-mode . yas-global-mode)
  :config
  (setq yas-verbosity 1)
  (setq yas-wrap-around-region t))

(use-package yasnippet-snippets
  :ensure t)

(use-package exec-path-from-shell
    :ensure t
    :config
    (exec-path-from-shell-initialize))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package rainbow-mode
  :ensure t)

(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config))

(use-package markdown-mode
  :ensure t
  :config
  (add-hook 'markdown-mode-hook #'toggle-word-wrap)
  (setq markdown-command "/usr/bin/pandoc"))

(use-package markdown-preview-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.html.eex\\'" . web-mode)
         )
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-expanding t)
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-enable-css-colorization t))

(add-hook 'prog-mode (lambda () (setq truncate-lines t)))

(use-package emmet-mode
  :ensure t
  :hook web-mode
  :hook css-mode)

(use-package bundler
  :ensure t)

;; Ruby and Rails
(use-package inf-ruby
  :ensure t
  :hook (ruby-mode . inf-ruby-minor-mode))

(use-package inflections
  :ensure t)

(use-package rspec-mode
  :ensure t
  :config
  (setq-default rspec-use-spring-when-possible nil)
  (rspec-install-snippets))

(add-hook 'after-init-hook 'inf-ruby-switch-setup)

(use-package robe
  :ensure t
  :config
  (global-robe-mode))

(eval-after-load 'company
  '(push 'company-robe company-backends))

(use-package rubocop :ensure t)
(use-package feature-mode :ensure t) ;; cucumber

(use-package alchemist :ensure t)
(use-package terraform-mode :ensure t)
;; END

;; Python
(use-package elpy
  :ensure t
  :config
  (elpy-enable))

;; Figure out how to only enable this on python, because it is conflicting with org-mode scheduling
;; (use-package pyenv-mode
;;   :ensure t
;;   :config
;;   (pyenv-mode))
;; END

(use-package dashboard
  :ensure t
  :config
  (setq dashboard-banner-logo-title "Focus on using yasnippets this week!")
  (setq dashboard-startup-banner 'official)
  (setq dashboard-center-content t)
  (setq dashboard-items '((agenda . 10)
                          (bookmarks . 5)
                          (projects . 5)))
  (setq show-week-agenda-p t)
  (dashboard-setup-startup-hook))

(use-package nyan-mode
  :ensure t
  :config
  (setq nyan-wavy-trail t)
  (setq nyan-bar-length 24)
  (nyan-mode))

(use-package ledger-mode
  :ensure t)

(use-package groovy-mode
  :ensure t)

(use-package elfeed
  :ensure t
  :config
  (global-set-key (kbd "C-x w") 'elfeed)
  (setq elfeed-feeds
        '("https://planet.emacslife.com/atom.xml"
          "https://www.ruby-lang.org/en/feeds/news.rss"
          "https://rubyweekly.com/rss"
          "https://wilhelmbot.com/feed.xml")))

;; (use-package ox-reveal
;;   :ensure t
;;   :config
;;   (setq Org-Reveal-root "https://cdn.sdelivr.net/npm/reveal.js")
;;   (setq Org-Reveal-title-slide nil))

(use-package helpful
  :ensure t
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-c C-d") #'helpful-at-point)
  (global-set-key (kbd "C-h C") #'helpful-command)
  (global-set-key (kbd "C-h F") #'helpful-function)
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable))

(use-package easy-jekyll
  :ensure t
  :init
  (setq easy-jekyll-basedir "~/projects/wkirschbaum/blog")
  (setq easy-jekyll-url "https://wilhelmbot.com")
  (setq easy-jekyll-root "")
  (setq easy-jekyll-previewtime "300")
  :bind ("C-c C-e" . easy-jekyll))

(use-package tramp
  :config
  (setq tramp-default-method "ssh")
  (setq remote-file-name-inhibit-cache nil)
  (setq tramp-completion-reread-directory-timeout nil)
  (setq vc-ignore-dir-regexp
        (format "%s\\|%s"
                vc-ignore-dir-regexp
                tramp-file-name-regexp)))

(use-package coverage
  :ensure t
  :config
  (global-set-key (kbd "C-c , ,") #'coverage-mode))
