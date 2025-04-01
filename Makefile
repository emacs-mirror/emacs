LISP ?= sbcl --dynamic-space-size 4GiB --noinform --no-sysinit --no-userinit
PREFIX ?= /usr/local

ncurses:
	qlot install
	$(LISP) --load .qlot/setup.lisp \
		--load scripts/build-ncurses.lisp

sdl2:
	qlot install
	$(LISP) --load .qlot/setup.lisp \
		--load scripts/build-sdl2.lisp

sdl2-ncurses:
	qlot install
	$(LISP) --load .qlot/setup.lisp \
		--load scripts/build-sdl2-ncurses.lisp

server:
	qlot install
	$(LISP) --load .qlot/setup.lisp \
		--load scripts/build-server.lisp

lem: sdl2-ncurses

install-bin: lem
	install -m 755 lem $(PREFIX)/bin

# TODO: on the fly edit lem.desktop depends on $(PREFIX)
install: install-bin
	install -m 644 scripts/install/lem.svg /usr/share/icons/hicolor/scalable/apps/
	gtk-update-icon-cache /usr/share/icons/hicolor
	desktop-file-install --dir=/usr/share/applications scripts/install/lem.desktop
	@echo "+--------------------------------+"
	@echo "|   Lem installation complete!   |"
	@echo "+--------------------------------+"

test:
	qlot install
	.qlot/bin/rove lem-tests.asd

doc:
	qlot install
	$(LISP) --load .qlot/setup.lisp --load scripts/generate-documentation-tests.lisp --eval '(progn (lem-documentation-mode/tests::generate-markdown-file "test.md" :test) (quit))'

update:
	git pull
	qlot install

lint:
	.qlot/bin/sblint lem.asd
	.qlot/bin/sblint extensions/asciidoc-mode/lem-asciidoc-mode.asd
	.qlot/bin/sblint extensions/asm-mode/lem-asm-mode.asd
	.qlot/bin/sblint extensions/c-mode/lem-c-mode.asd
	.qlot/bin/sblint extensions/color-preview/lem-color-preview.asd
	.qlot/bin/sblint extensions/css-mode/lem-css-mode.asd
	.qlot/bin/sblint extensions/dart-mode/lem-dart-mode.asd
	.qlot/bin/sblint extensions/documentation-mode/lem-documentation-mode.asd
	.qlot/bin/sblint extensions/dot-mode/lem-dot-mode.asd
	.qlot/bin/sblint extensions/elisp-mode/lem-elisp-mode.asd
	.qlot/bin/sblint extensions/elixir-mode/lem-elixir-mode.asd
#	.qlot/bin/sblint extensions/encodings/lem-encodings.asd
	.qlot/bin/sblint extensions/erlang-mode/lem-erlang-mode.asd
	.qlot/bin/sblint extensions/go-mode/lem-go-mode.asd
	.qlot/bin/sblint extensions/haskell-mode/lem-haskell-mode.asd
	.qlot/bin/sblint extensions/html-mode/lem-html-mode.asd
	.qlot/bin/sblint extensions/java-mode/lem-java-mode.asd
	.qlot/bin/sblint extensions/js-mode/lem-js-mode.asd
	.qlot/bin/sblint extensions/json-mode/lem-json-mode.asd
	.qlot/bin/sblint extensions/language-client/lem-language-client.asd
	.qlot/bin/sblint extensions/language-server/lem-language-server.asd
	.qlot/bin/sblint extensions/legit/lem-legit.asd
	.qlot/bin/sblint extensions/lisp-mode/lem-lisp-mode.asd
	.qlot/bin/sblint extensions/lisp-syntax/lem-lisp-syntax.asd
	.qlot/bin/sblint extensions/lsp-base/lem-lsp-base.asd
	.qlot/bin/sblint extensions/lsp-mode/lem-lsp-mode.asd
	.qlot/bin/sblint extensions/lua-mode/lem-lua-mode.asd
	.qlot/bin/sblint extensions/makefile-mode/lem-makefile-mode.asd
	.qlot/bin/sblint extensions/markdown-mode/lem-markdown-mode.asd
	.qlot/bin/sblint extensions/nim-mode/lem-nim-mode.asd
	.qlot/bin/sblint extensions/ocaml-mode/lem-ocaml-mode.asd
	.qlot/bin/sblint extensions/paredit-mode/lem-paredit-mode.asd
	.qlot/bin/sblint extensions/patch-mode/lem-patch-mode.asd
	.qlot/bin/sblint extensions/posix-shell-mode/lem-posix-shell-mode.asd
	.qlot/bin/sblint extensions/process/lem-process.asd
	.qlot/bin/sblint extensions/python-mode/lem-python-mode.asd
	.qlot/bin/sblint extensions/review-mode/lem-review-mode.asd
	.qlot/bin/sblint extensions/rust-mode/lem-rust-mode.asd
	.qlot/bin/sblint extensions/scala-mode/lem-scala-mode.asd
	.qlot/bin/sblint extensions/scheme-mode/lem-scheme-mode.asd
	.qlot/bin/sblint extensions/shell-mode/lem-shell-mode.asd
	.qlot/bin/sblint extensions/sql-mode/lem-sql-mode.asd
	.qlot/bin/sblint extensions/swift-mode/lem-swift-mode.asd
	.qlot/bin/sblint extensions/terminal/lem-terminal.asd
	.qlot/bin/sblint extensions/typescript-mode/lem-typescript-mode.asd
	.qlot/bin/sblint extensions/vi-mode/lem-vi-mode.asd
	.qlot/bin/sblint extensions/welcome/lem-welcome.asd
	.qlot/bin/sblint extensions/xml-mode/lem-xml-mode.asd
	.qlot/bin/sblint extensions/yaml-mode/lem-yaml-mode.asd
	.qlot/bin/sblint extensions/ruby-mode/lem-ruby-mode.asd
