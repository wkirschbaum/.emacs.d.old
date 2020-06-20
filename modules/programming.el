(use-package yasnippet  :ensure t
  :hook (prog-mode . yas-global-mode)
  :config
  (setq yas-verbosity 1
        yas-wrap-around-region t))

(use-package yasnippet-snippets
  :ensure t)

(use-package inf-ruby
  :ensure t)

(use-package f
  :ensure t)

(use-package rake
  :ensure t)

(use-package inflections
  :ensure t)

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
         ("\\.html.leex\\'" . web-mode)
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

(use-package rspec-mode
  :ensure t
  :config
  (setq-default rspec-use-spring-when-possible t)
  (rspec-install-snippets))

(add-hook 'after-init-hook 'inf-ruby-switch-setup)

(use-package rubocop :ensure t)
(use-package feature-mode :ensure t) ;; cucumber

(use-package terraform-mode :ensure t)

(use-package groovy-mode
  :ensure t)

(use-package flycheck-mix
  :ensure t)

(use-package flycheck-credo
  :ensure t)

(use-package grip-mode
  :ensure t
  :bind (:map markdown-mode-command-map
              ("g" . grip-mode)))

(use-package company-restclient
  :ensure t
  :config
  (add-to-list 'company-backends 'company-restclient))

(setq lsp-keymap-prefix "C-c l")
(setq lsp-clients-elixir-server-executable "~/src/opensource/elixir-ls/language_server.sh")

(use-package lsp-mode
  :ensure t
  :hook ((ruby-mode . lsp)
         (elixir-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui :ensure t :commands lsp-ui-mode)
(use-package lsp-ivy :ensure t :commands lsp-ivy-workspace-symbol)
(use-package dap-mode :ensure t)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

(use-package company-lsp
  :ensure t
  :config
  (add-to-list 'company-backends 'company-lsp))
