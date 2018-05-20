(provide 'haskell)

(ensure-packages-installed 'haskell-mode 'intero)

(add-hook 'haskell-mode-hook 'intero-mode)
