(provide 'anki)

(ensure-packages-installed 'anki-editor 'anki-connect)

(require 'anki-editor)

(setq anki-editor-create-decks t)
