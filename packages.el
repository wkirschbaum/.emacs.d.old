;; ;; Packages

;; Possible packages to use
;; https://github.com/ahungry/fast-scroll

;; It is important that this gets loaded before any other package hooking into org mode
;; (use-package org
;;   :straight t)

(use-package all-the-icons
  :straight t)

;; Ensures most used M-x commands on top
(use-package smex
  :straight t)

(use-package dired
  :bind ("C-x C-j" . dired-jump)
  :config
  (setq dired-dwim-target t))

(use-package which-key
  :straight t
  :config
  (which-key-mode 1))

(use-package whitespace
  :straight t
  :config
  (add-hook 'prog-mode 'whitespace-mode)
  (setq whitespace-style '(face tabs tab-mark trailing empty)))

(use-package git-timemachine
  :straight t
  :defer t)

(use-package magit
  :straight t
  :demand t
  :bind ("C-x g" . magit-status))

(use-package forge
  :straight t)

(use-package diff-hl
  :straight t
  :hook ((prog-mode . diff-hl-mode)
         (org-mode . diff-hl-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh)))

(use-package ag
  :straight t
  :commands (ag ag-regexp ag-project))

(use-package ivy
  :straight t
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
  :straight t
  :bind-keymap ("C-x p" . projectile-command-map)
  :config
  (setq projectile-enable-caching t
        projectile-completion-system 'ivy
        projectile-file-exists-remote-cache-expire nil
        projectile-sort-order 'recently-active)
  (projectile-mode +1))

(use-package counsel-projectile
  :straight t
  :config
  (counsel-projectile-mode))

(use-package company
  :straight t
  :config
  (global-company-mode t))

(use-package flycheck
  :straight t
  :config
  (global-flycheck-mode))

(use-package flyspell
  :straight t
  :hook ((prog-mode . flyspell-prog-mode)
         (text-mode . flyspell-mode)))

;; erc
(use-package erc
  :straight t
  :config
  (setq-default erc-echo-notices-in-minibuffer-flag t)
  (setq erc-rename-buffers t)
  (setq erc-lurker-hide-list '("JOIN" "QUIT"))
  (setq erc-lurker-threshold-time 3600)
  (setq erc-input-line-position -2))

(setq auth-sources '((:source "~/Dropbox/secrets/.authinfo.gpg")))

(use-package yasnippet  :straight t
  :hook (prog-mode . yas-global-mode)
  :config
  (setq yas-verbosity 1)
  (setq yas-wrap-around-region t))

(use-package yasnippet-snippets
  :straight t)

(use-package nov
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(use-package restclient :straight t)
(use-package dired-filter :straight t)

(use-package browse-at-remote :straight t)

(use-package exec-path-from-shell
    :straight t
    :config
    (exec-path-from-shell-initialize))

(use-package expand-region
  :straight t
  :bind ("C-=" . er/expand-region))

(use-package rainbow-mode :straight t)

(use-package smartparens
  :straight t
  :config
  (require 'smartparens-config))

(use-package markdown-mode
  :straight t
  :config
  (add-hook 'markdown-mode-hook #'toggle-word-wrap)
  (setq markdown-command "/usr/bin/pandoc"))

(use-package markdown-preview-mode :straight t)

(use-package yaml-mode :straight t)

(use-package dockerfile-mode :straight t)

(use-package web-mode
  :straight t
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
  :straight t
  :hook web-mode
  :hook css-mode)

(use-package haskell-mode :straight t)
(use-package intero
  :straight t
  :config
  (intero-global-mode 1))

(use-package bundler :straight t)

;; Ruby and Rails
(use-package inf-ruby
  :straight t
  :hook (ruby-mode . inf-ruby-minor-mode))

(use-package inflections :straight t)
(use-package rake :straight t)
(use-package projectile-rails
  :straight t
  :config
  (projectile-rails-global-mode))

(use-package rspec-mode
  :straight t
  :config
  (setq-default rspec-use-spring-when-possible nil)
  (rspec-install-snippets))

(add-hook 'after-init-hook 'inf-ruby-switch-setup)

(use-package robe
  :straight t
  :config
  (global-robe-mode))

(eval-after-load 'company
  '(push 'company-robe company-backends))

(use-package yari :straight t) ;; ruby documentation
(use-package rubocop :straight t)
(use-package feature-mode :straight t) ;; cucumber

(use-package alchemist :straight t)
(use-package terraform-mode :straight t)
;; END

;; Python
(use-package elpy
  :straight t
  :config
  (elpy-enable))

;; Figure out how to only enable this on python, because it is conflicting with org-mode scheduling
;; (use-package pyenv-mode
;;   :straight t
;;   :config
;;   (pyenv-mode))
;; END

(use-package dashboard
  :straight t
  :config
  (setq dashboard-banner-logo-title "Focus on using yasnippets this week!")
  (setq dashboard-startup-banner 'official)
  (setq dashboard-center-content t)
  (setq dashboard-items '((agenda . 10)
                          (bookmarks . 5)
                          (projects . 5)))
  (setq show-week-agenda-p t)
  (dashboard-setup-startup-hook))

(use-package emms
  :straight t
  :config
  (emms-standard)
  (emms-default-players))

(use-package nyan-mode
  :straight t
  :config
  (setq nyan-wavy-trail t)
  (setq nyan-bar-length 24)
  (nyan-mode))

(use-package ledger-mode
  :straight t)

(use-package groovy-mode
  :straight t)

(use-package elfeed
  :straight t
  :config
  (global-set-key (kbd "C-x w") 'elfeed)
  (setq elfeed-feeds
        '("https://planet.emacslife.com/atom.xml"
          "https://www.ruby-lang.org/en/feeds/news.rss"
          "https://rubyweekly.com/rss"
          "https://wilhelmbot.com/feed.xml")))

(use-package telega
  :straight t
  :commands (telega)
  :config
  (setq telega-use-notifications t)
  (add-hook 'telega-chat-mode-hook (lambda ()
    (setq company-backends '(telega-company-emoji))
    (company-mode 1)))
  :defer t)

(use-package alert
  :straight t)

;; (use-package ox-reveal
;;   :straight t
;;   :config
;;   (setq Org-Reveal-root "https://cdn.jsdelivr.net/npm/reveal.js")
;;   (setq Org-Reveal-title-slide nil))

(use-package helpful
  :straight t
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
  :straight t
  :init
  (setq easy-jekyll-basedir "~/projects/wkirschbaum/blog")
  (setq easy-jekyll-url "https://wilhelmbot.com")
  (setq easy-jekyll-root "")
  (setq easy-jekyll-previewtime "300")
  :bind ("C-c C-e" . easy-jekyll))

;; (use-package excorporate
;;   :straight t
;;   :config
;;   (setq excorporate-configuration (quote ("wilhelm.kirschbaum@prodigyfinance.com" . "https://outlook.office365.com/EWS/Exchange.asmx")))
;;   (excorporate-diary-enable)
;;   (excorporate))

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
  :straight t
  :config
  (global-set-key (kbd "C-c , ,") #'coverage-mode))
